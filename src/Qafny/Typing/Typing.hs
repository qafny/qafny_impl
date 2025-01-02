{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , MultiWayIf
  , NamedFieldPuns
  , ParallelListComp
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , TypeFamilies
  #-}


module Qafny.Typing.Typing where

-- | Typing though Fused Effects

-- Effects
import           Control.Effect.NonDet
import           Qafny.Effect

-- Qafny
import           Qafny.Analysis.Interval
import           Qafny.Analysis.Partial
    (Reducible (reduce))
import           Qafny.Error
    (QError (..))
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Syntax.Subst
import           Qafny.Typing.Utils
import           Qafny.Utils.Utils

import           Qafny.Typing.Partial
import           Qafny.Utils.EmitBinding

-- Utils
import           Control.Carrier.State.Lazy
    (evalState, execState, runState)
import           Control.Lens
    (at, (%~), (?~), (^.))
import           Control.Monad
    (forM, forM_, liftM2, unless, when, zipWithM, (>=>))
import           Data.Bifunctor
    (Bifunctor (first, second))
import           Data.Bool
    (bool)
import           Data.Functor
    ((<&>))
import           Data.Functor.Identity

import qualified Data.List                  as List
import           Data.List.NonEmpty
    (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Strict            as Map
import           Data.Maybe
    (catMaybes, isJust, listToMaybe, mapMaybe)
import qualified Data.Set                   as Set
import           Data.Sum
import           GHC.Stack
    (HasCallStack)
import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Typing.Cast
import           Qafny.Typing.Error
    (SCError (SplitENError), failureAsSCError)
import           Qafny.Typing.Locus
    (updateMetaStByLocus)

throwError'
  :: ( Has (Error Builder) sig m, DafnyPrinter s )
  => s -> m a
throwError' = throwError . ("[Typing] " <!>)

-- | Compute the simple type of the given expression
typingExp
  :: ( Has (Reader TEnv) sig m
     , Has (Error Builder) sig m
     , HasCallStack
     )
  => Exp' -> m Ty
typingExp (ENum _)  = return TNat
typingExp (EVar x)  = do
  env <- view kEnv
  return ((env ^. at x) >>= projTy) `rethrowMaybe` UnknownVariableError x env
typingExp (EMeasure p) = return TMeasured
typingExp (EOp2 op2 e1 e2) =
  do top <- typingOp2 op2
     t1 <- typingExp e1
     t2 <- typingExp e2
     checkSubtype2 top t1 t2
  where
    -- typingOp2 :: Op2 -> m (Ty, Ty, Ty)
    -- | Types of binary operators
    typingOp2 OAnd = return (TBool, TBool, TBool)
    typingOp2 OOr  = return (TBool, TBool, TBool)
    -- We might need to solve the issue of nat vs int 0
    typingOp2 OAdd = return (TNat, TNat, TNat)
    typingOp2 OMod = return (TNat, TNat, TNat)
    typingOp2 OMul = return (TNat, TNat, TNat)
    typingOp2 OLt  = return (TNat, TNat, TBool)
    typingOp2 OLe  = return (TNat, TNat, TBool)
    typingOp2 ONor = exp2AExp e1 >>= \ae -> return (TNat, TNat, TQReg ae)
typingExp e = throwError' $
  "Expression " <!> e <!> " has no proper type."


-- | Compute the quantum type of a given (possibly incomplete) partition
--
-- For example, a partition `s = { x [0..1], y [0..1]}` can be treated as the
-- composition of `s1 ⊎ s2 = s`. Therefore, when dereferencing `s1`, it should
-- resolve and give me `s` instead of `s1` as the partition itself is
-- inseparable!
--
-- Examine each Range in a given Partition and resolve to a Locus
resolvePartition
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Partition -> m Locus
resolvePartition = (fst <$>) . resolvePartition'

-- | Look up the Locus record by the Loc reference in the session state.
findLocusByLoc :: HasResolution sig m => Loc -> m Locus
findLocusByLoc loc = do
  (part, (qty, degrees)) <-
    use (sSt . at loc) `rethrowMaybe` (UnknownLocError loc)
  return Locus {loc, part, qty, degrees}

resolvePartition'
  :: HasResolution sig m => Partition -> m (Locus, [(Range, Range)])
resolvePartition' se' = do
  -- resolve the canonical range names
  rsResolved <- rs `forM` resolveRange
  let locs = [ ((rSe, rSt), loc)
             | (rSe, rSt, ans, loc) <- concat rsResolved, included ans ]
  constraints <- ask @IEnv
  let related = incr4 $ vsep
        [ showRel r1 r2 b | (r1, r2, b, _) <- concat rsResolved ]

  trace . showEmit0 $ vsep
    [ "[resolvePartition] resolve (" <!> se
    , incr2 $ "within" <+> ppIEnv constraints
    , incr2 $ line <!> related ]
  case List.nub (snd <$> locs) of
    [] -> throwError $ errInternal related
    [loc] ->
      findLocusByLoc loc <&> (, fst <$> locs)
    ss -> throwError $ errNonunique ss related
  where
    se@(Partition rs) = reduce se'

    -- | an inclusion predicate is satisfied only if it holds under all possible
    -- environment constraints.
    included :: NonEmpty (Maybe Bool, AEnv) -> Bool
    included = all ((== Just True) . fst)

    -- Format
    errNonunique :: [Loc] -> Builder -> Builder
    errNonunique ss related = vsep
      [ "Type Error:"
        <+> "`" <!> se <!> "` is not the sub-partition of a unique partition."
      , incr2 "Counterexample: " <!> list ss
      , incr2 related
      ]
    errInternal related =
      "Type Error:" <+>
      "The partition `" <!> se <!> "` is not a sub-partition of any existing ones!"
      <!> line <!> related

    mkRelOp (Just True)  = " ⊑ "
    mkRelOp (Just False) = " ⋢ "
    mkRelOp Nothing      = "?⊑?"

    showRel :: Range -> Range -> NonEmpty (Maybe Bool, AEnv) -> Builder
    showRel r1 r2 ne = vsep . NE.toList $ ne
      <&> \(rel, env) ->
            r1 <!> " " <!> mkRelOp rel <!> " " <!> r2 <!> " at " <!> byComma env

removeTStateByLocus
  :: (Has (State TState) sig m)
  => Locus -> m ()
removeTStateByLocus st@(Locus{loc, part, qty}) = do
  sSt %= flip Map.withoutKeys (Set.singleton loc)
  xSt %= Map.map (filter ((/= loc) . snd))
  deleteEmPartition loc (nranges part)

-- | Query all ranges related to the given range.
-- Return a list of tuple standing for
-- * matched ranges
-- * statistics of the resolution
-- * the owner locus of the canonical range
-- FIXME: provide "resolveRanges" that instantiates the predicate only once!
resolveRange
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     )
  => Range -> m [(Range, Range, NonEmpty (Maybe Bool, AEnv), Loc)]
resolveRange r@(Range name _ _) = do
  (⊑//) <- (⊑/) -- specialize the predicate using the current constraints
  rlocs <- use (xSt . at name) `rethrowMaybe` UnknownRangeError r
  return [ (r, r', r ⊑// r', loc)
         | (Normalized r', loc) <- rlocs ]

-- | Resolve the ranges using the Range name state and the current constraint
-- environment.  It is strict in that the resolution is considered strict if all
-- constraints are satisfied and the range is a sub-range of only one locus.
resolveRangesStrict :: HasResolution sig m => [Range] -> m [(Range, Range, Loc)]
resolveRangesStrict rs = do
  (∀⊑//) <- (∀⊑/)
  st <- use xSt
  let rslocsM = rs <&> \r -> st Map.!? getRangeName r
  rslocs <- zipWithM (fromMaybeM . errUnknownRange) rs rslocsM
  zipWithM (go (∀⊑//)) rs rslocs
  where
    go cmp rGiven rlFound =
      case filter (cmp rGiven . denorm . fst) rlFound of
        [(Normalized rFound, lFound)] ->
          pure (rGiven, rFound, lFound)
        []    -> errUnknownRange rGiven
        found -> errAmbiguousRange rGiven (first denorm <$> found)
    errUnknownRange = throwError' . pp .UnknownRangeError
    errAmbiguousRange rGiven found =
      throwError' (pp (AmbiguousRange rGiven found))

-- | Resolve the ranges using the Range name state and the current constraint
-- environment.  It is strict in that the resolution is considered strict if all
-- constraints are satisfied and the range is a sub-range of only one locus.
resolveRangesStrictIntoLoci ::
  HasResolution sig m => [Range] -> m [(Range, Range, Locus)]
resolveRangesStrictIntoLoci =
  resolveRangesStrict >=> mapM go
  where
    go (r1, r2, l) = findLocusByLoc l <&> (r1, r2, )

resolvePartitions
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => [Partition] -> m Locus
resolvePartitions =
  resolvePartition . Partition . concatMap unpackPart


inferRangeTy
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m)
  => Range -> m (QTy, [Int])
inferRangeTy r =
  go <$>  resolvePartition (Partition [r])
  where
    go Locus{qty, degrees} = (qty, degrees)

--------------------------------------------------------------------------------
-- | Split Typing
--------------------------------------------------------------------------------
-- | Given a partition and a range, compute a split scheme if the range is a
-- part of the partition.
--
-- The returned 'Locus' is the partition covering the range
--
-- For the optional return value, return 'Nothing' if no split needs **on a specific
-- range** needs to be performed, i.e. no codegen needs to be done.
splitScheme
  :: ( Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (State TState) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Locus
  -> Range
  -> m (Locus, Maybe SplitScheme)
splitScheme s r = errTrace "`splitScheme`" $ do
  splitScheme' s r

-- | Rationale: there's no good reason to split a partition with multiple ranges
-- in entanglement. So, we're safe to reject non-singleton partitions for now.
splitSchemePartition
  :: ( Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (State TState) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Locus
  -> Partition
  -> m (Locus, Maybe SplitScheme)
splitSchemePartition st p =
  case unpackPart p of
    [r] -> splitScheme st r
    _   -> throwError' $
      "Partition" <+> p <+> "contains multiple ranges for split, which is likely to be a bug!"


-- | Remove range if it is equivalent to the bottom
contractRange
  :: ( Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     )
  => Normalized Range -> m (Maybe (Normalized Range))
contractRange nr = do
  botHuh <- ($ r) <$> isBotI
  case botHuh of
    _ | all (== Just True) botHuh -> return Nothing
    _ | all isJust botHuh         ->
        -- may or may not be, therefore leave it there
        return (Just nr)
    _iDontKnow                    -> err botHuh
  where
    r = denorm nr
    err reason = do
      ienv <- ask @IEnv
      throwError' $ vsep
        ["Cannot decide if " <!> r <!> " is empty"
        ,"context: " <!> ppIEnv ienv
        ]

-- TODO1: Pass in an environment and perform substitution before doing value
-- checking
-- TODO2: Use 'NonDeterm' effects instead.
splitScheme'
  :: ( Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (State TState) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Locus
  -> Range
  -> m (Locus, Maybe SplitScheme)
splitScheme' s@(Locus{loc, part, qty, degrees}) rSplitTo'  = do
  -- FIXME: Type checking of `qt` should happen first because splitting an EN
  -- typed partition should not be allowed.
  -- TODO: Is this correct?
  when (isEn qty) $
    throwError'' errSplitEN

  rangeSplit@RangeSplits { rsRsUnchanged, rsRLeft, rsRRight, rsRAffected} <-
    getRangeSplits s rSplitTo
  trace $ showEmit0
    ("range split:" <+> rsRAffected <+> rsRLeft <+> rsRRight)
  case NE.nonEmpty (rsRsRemainder rangeSplit) of
    Nothing -> -- | No split!
      return (s, Nothing)
    Just rsRemainder -> do
      let psRemainder = npart . Identity <$> rsRemainder
          psRemainder' = npart . List.singleton <$> rsRemainder
          pSplitInto  = npart [rSplitTo]
      -- | ill-formed partition
      unless (null rsRsUnchanged) $
        throwError'' errInconsistentRemainder

      -- 1. Allocate partitions, break ranges and move them around
      lociRem <- mapM (gensymLoc . rangeVar) rsRemainder
      -- Aux reuses the current loc
      sSt %= (at loc ?~ (pSplitInto, (qty, degrees)))
      sequence_ $ NE.zipWith
        (\locMain pMain -> sSt %= (at locMain ?~ (pMain, (qty, degrees))))
        lociRem psRemainder'

      -- | retrieve all (range, loc) pair associated with the variable `x`
      xRangeLocs <- use (xSt . at to) `rethrowMaybe` errXST
      let xrl =
            -- "split range -> old loc"
            [ (rSplitTo, loc) ] ++
            -- "quotient ranges -> new loci"
            [ (rRemainder, locRem)
            | rRemainder <- NE.toList rsRemainder
            | locRem <- NE.toList lociRem
            ] ++
            -- "the rest with the old range removed"
            List.filter ((/= rsRAffected) . fst) xRangeLocs
      -- update the range name state
      xSt %= (at to ?~ xrl)

      -- 2. Generate emit symbols for split ranges
      let locusSplitInto = s{part=pSplitInto}
          lociRemainder  = NE.zipWith updateLocPartOfS lociRem psRemainder

      -- locate EmitData of both range and locus
      edRAffected@EmitData{evBasis=vEmitRMaybe} <- findEm (inj rsRAffected)
      edLAffected@EmitData{evPhaseRef=vPhaseMaybe
                          ,evAmp=vEmitAMaybe} <- findEm (inj loc)
      vEmitR <- findVisitEm evBasis (inj rsRAffected)
      -- delete the range record
      deleteEms [inj rsRAffected]

      -- generate EmitData for new ranges
      edRSplit <- genEmStByRange qty rSplitTo
      let schEdAffected = (rsRAffected, edLAffected, edRAffected)
      let schEdSplit = (rSplitTo,edRSplit)
      schEdRemainders <- forM lociRemainder $ \locRem -> do
        (edLoc, Identity (rRem, edR)) <- genEmStFromLocus locRem
        return (rRem, edLoc, edR)
      return $ ( locusSplitInto
               , Just SplitScheme { schEdAffected, schEdSplit, schEdRemainders })
  where
    rSplitTo@(Normalized (Range to rstL rstR)) = normalize rSplitTo'
    updateLocPartOfS l p = s{loc=l, part=p}
    rangeVar (Normalized (Range x _ _)) = x
    -- infoSS :: String = printf
    --   "[splitScheme] from (" <!> X <!> ") to (" <!> X <!> ")" (show s) (show rSplitTo)
    throwError'' s' = throwError' ("[split]" <+> s')
    -- errUnsupprtedTy = printf
    --   "Splitting a " <!> X <!> " partition is unsupported." (show qty)
    errXST =
      "No range beginning with " <!> to <!> " cannot be found in `xSt`"
    errSplitEN = "Splitting an EN partition is disallowed!"
    errInconsistentRemainder =
      "Splittable partition" <!> part <!> "should not include more than one range."

-- | Duplicate a phase type by allocating a new reference to its Repr if
-- necessary.
--
-- duplicatePhaseTy
--   :: ( Has (Gensym Emitter) sig m
--      )
--   => Loc -> QTy -> PhaseTy
--   -> m PhaseTy
-- duplicatePhaseTy _ _ PT0 = return PT0
-- duplicatePhaseTy v qt pty@(PTN i (PhaseRef { prBase=vBase, prRepr=_ })) = do
--   -- allocate a new repr variable without allocating a new base one
--   vRepr <- gensym (LBinding (deref v, inj pty))
--   return (PTN i (PhaseRef { prBase=vBase, prRepr=vRepr }))


data RangeSplits = RangeSplits
  { rsRLeft       :: Maybe (Normalized Range)
    -- ^ left remainder
  , rsRRight      :: Maybe (Normalized Range)
    -- ^ right remainder
  , rsRSplitInto  :: Normalized Range
    -- ^ resulting range
  , rsRAffected   :: Normalized Range
    -- ^ original range that is affected
  , rsRsUnchanged :: [Normalized Range]
    -- ^ other ranges untouched
  , rsRsRemainder :: [Normalized Range]
    -- ^ remainders introduced by breaking the affected range (left + right)
  }

-- | Compute, in order to split the given range from a resolved partition, which
-- range in the partition needs to be split as well as the resulting quotient
-- ranges.
--
-- Note: this function is entanglement-type-agnostic and works for any kind of
-- partitions regardless of the number of ranges in the partition.
-- Ranges that are kept as is will be put in `rsRsUnchanged`
getRangeSplits
  :: ( Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Locus -> Normalized Range -> m RangeSplits
getRangeSplits
  s@(Locus{loc, part=p, degrees})
  nrSplitTo@(Normalized (Range to rstL rstR)) = do
  trace $ showEmit0
    ("splitInto:" <+> "from" <+> p <+> "to" <+> nrSplitTo)
  botHuh <- ($ (denorm nrSplitTo)) <$> isBotI
  case botHuh of
    _ | all (== Just True)  botHuh -> throwError' errBotRx
    _ | all (== Just False) botHuh -> return ()
    _ -> do ienv <- ask @IEnv
            throwError' $ vsep
              [ "Cannot decide if" <+> nrSplitTo <+> "is empty."
              , "Env:" <+> ppIEnv ienv ]
  (⊑??) <- (∀⊑/)
  case matched (⊑??) of
    Nothing -> throwError' errImproperRx
    Just (rRemL, _, rRemR, rOrigin, idx) -> do
      rRemLeft  <- contractRange rRemL
      rRemRight <- contractRange rRemR
      let rsRest = removeNth idx (nranges p)
      return $ RangeSplits
        { rsRLeft       = rRemLeft
        , rsRRight      = rRemRight
        , rsRSplitInto  = nrSplitTo
        , rsRAffected   = rOrigin
        , rsRsUnchanged = rsRest
        , rsRsRemainder = catMaybes [rRemLeft, rRemRight]
        }
  where
    removeNth n l = let (a, b) = splitAt n l in a ++ tail b
    errBotRx = "The range " <!> nrSplitTo <!> " contains no qubit!"
    errImproperRx =
      "The range " <!> nrSplitTo <!> " is not a part of the partition " <!> s <!> "!"
    matched (⊑??) = listToMaybe -- logically, there should be at most one partition!
      [ ( Normalized (Range y yl rstL)
        , nrSplitTo
        , Normalized (Range y rstR yr)
        , nrRef, idx)
      | (nrRef@(Normalized (Range y yl yr)), idx) <- zip (nranges p) [0..]
        -- ^ choose a range in the environment
      , to == y
        -- ^ must be of the same register label!
      , (denorm nrSplitTo) ⊑?? (denorm nrRef)
        -- ^ must be a sub-interval
      ]


-- | Cast a partition of type 'qt1' to 'qt2' and perform a split before the
-- casting if needed.  return 'Nothing' if no cast is required.
splitThenCastScheme
  :: ( Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has (State TState) sig m
     , Has Trace sig m
     , Has (Reader IEnv) sig m
     , Has (Error SCError) sig m
     )
  => Locus
  -> QTy
  -> Range
  -> m ( Locus              -- the finally resolved partition
       , Maybe SplitScheme  -- May split if cast or not
       , Maybe CastScheme   -- May cast or not
       )
splitThenCastScheme s'@Locus{ loc, part, qty=qt1, degrees } qt2 rSplitTo =
  failureAsSCError . errTrace "`splitThenCastScheme`" $
  case (qt1, qt2) of
    (_, _) | isEn qt1 && qt1 == qt2 -> do
      -- same type therefore no cast needed,
      -- do check to see if a split in range is also not needed
      RangeSplits { rsRAffected
                  , rsRsRemainder
                  } <- getRangeSplits s' nrSplitTo
      case rsRsRemainder of
        [] -> return (s', Nothing, Nothing)
        __ -> throwError $ SplitENError
          s' rSplitTo (denorm rsRAffected) (denorm <$> rsRsRemainder)
    (_ , _) | not (isEn qt1) && isEn qt2 -> do
      -- casting a smaller type to a larger type
      (sSplit, maySchemeS) <- splitScheme s' rSplitTo
      (sCast, maySchemeC) <- castScheme sSplit qt2
      return (sCast, maySchemeS, maySchemeC)
    (_, _) | qt1 == qt2 -> do
      -- the same type, therefore, only try to compute a split scheme
      (sSplit, maySchemeS) <- splitScheme s' rSplitTo
      return (sSplit, maySchemeS, Nothing)
    _ ->
      throwError' errUndef
   where
     nrSplitTo = normalize rSplitTo
     errUndef =
       "split-then-cast-scheme is undefined for"
       <+> qt1 <+> "to" <+> qt2 <+> "type"

--------------------------------------------------------------------------------
-- * Aux Typing
--------------------------------------------------------------------------------

-- | Extract relevant partitions from guard expression and resolve its type.
typingGuard
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => GuardExp -> m (Locus, Partition)
typingGuard (GEPartition s' _) = resolvePartition s' <&> (,s')

-- | Type check if the given partition exists in the context
typingPartitionQTy
  :: ( Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Partition -> QTy -> m Locus
typingPartitionQTy s qt = do
  st <- resolvePartition s
  when (qt /= qty st) $ throwError' (errTypeMismatch st)
  return st
  where
    errTypeMismatch st = vsep
      [ "The partition" <+> s <+> "is not of type" <+> qt <!> "."
      , "Resolved:" <+> st ]

typingPartition
  :: ( Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Partition -> m Locus
typingPartition s = do
  st@Locus{part=pResolved, qty=qtResolved} <- resolvePartition s
  when (List.sort (nranges pResolved) /= List.sort (nranges (normalize s))) $
    throwError' $ errIncompletePartition st
  return st
  where
    errIncompletePartition st@(Locus{part=p}) =
      "The partition " <!> s <!> " is a sub-partition of " <!> p <!> ".\nResolved: " <!> st

--------------------------------------------------------------------------------
-- * Subtyping
--------------------------------------------------------------------------------
checkTypeEq
  :: Has (Error Builder) sig m
  => Ty -> Ty -> m ()
checkTypeEq t1 t2 =
  unless (t1 == t2) $ throwError
    ("Type mismatch: `" <!> t1 <!> "` is not a subtype of `" <!> t2 <!> "`")

checkSubtype
  :: Has (Error Builder) sig m
  => Ty -> Ty -> m ()
checkSubtype t1 t2 =
  unless (sub t1 t2) $ throwError
    ("Type mismatch: `" <!> t1 <!> "` is not a subtype of `" <!> t2 <!> "`")
checkSubtype2
  :: Has (Error Builder) sig m
  => (Ty, Ty, Ty) -> Ty -> Ty -> m Ty
checkSubtype2 (top1, top2, tret) t1 t2 =
  do checkSubtype top1 t1
     checkSubtype top2 t2
     return tret

sub :: Ty -> Ty -> Bool
sub = (==)

--------------------------------------------------------------------------------
-- | QSubtyping
--------------------------------------------------------------------------------
subQ :: QTy -> QTy -> Bool
subQ _    TEn   = True
subQ THad THad  = True
subQ THad TEn01 = True
subQ TNor TEn01 = True
subQ TNor TNor  = True
subQ _     _    = False

checkSubtypeQ
  :: Has (Error Builder) sig m
  => QTy -> QTy -> m ()
checkSubtypeQ t1 t2 =
  unless (subQ t1 t2) . throwError $
    "Type mismatch: `" <!> t1 <!> "` is not a subtype of `" <!> t2 <!> "`"

-------------------------------------------------------------------------------
-- | Type Manipulation
--------------------------------------------------------------------------------
-- | Cast the type of a partition to a given qtype, modify the typing state and
-- allocate emit variables.
castScheme
  :: GensymEmitterWithStateError sig m
  => Locus -> QTy -> m (Locus, Maybe CastScheme)
castScheme locus@Locus{loc=locS, part=sResolved, qty, degrees} qtNow =
  case castLocus locus qtNow of
    Left ErrNoCast -> return (locus, Nothing)
    Left ErrInvalidCast ->
      throwError' $ qty <!> "cannot be casted into" <!> qtNow <!> "."
    Right locus' -> (locus,) . Just <$> castSchemeUnchecked locus locus'

-- | Calculate the cast scheme between two loci.
-- The cast will happen for sure even if there's actually no cast relation
-- between those two Loci.
castSchemeUnchecked
  :: GensymEmitterWithStateError sig m
  => Locus -> Locus ->m CastScheme
castSchemeUnchecked locus@Locus{qty=schQtFrom} newLocus@Locus{qty=schQtTo} = do
  updateMetaStByLocus newLocus
  schEdsFrom <- findEmsByLocus locus
  schEdsTo   <- regenEmStByLocus locus newLocus
  return CastScheme{schEdsFrom,schEdsTo,schQtFrom,schQtTo}

-- | The same as 'castScheme', for compatibility
retypePartition
  :: ( Has (Error Builder) sig m
     , Has (State TState) sig m
     , Has (Gensym Emitter) sig m
     )
  => Locus -> QTy -> m (Maybe CastScheme)
retypePartition = (snd <$>) .: castScheme

--------------------------------------------------------------------------------
-- * Merge Typing
--------------------------------------------------------------------------------
-- | Given two states and two lists of loci, return matched LoucsEmitData assuming
-- there's a one-to-one correspondence between those two lists of loci.
matchLocusEmitData
  :: Has (Error Builder) sig m
  => EmitState -> EmitState -> [(Locus, Locus)] -> m [(LocusEmitData, LocusEmitData)]
matchLocusEmitData esInit esLoop lociInitLoop =
  go `mapM` lociInitLoop
  where
    go (locusInit, locusLoop) =
      (,)
      <$> findEmsByLocusInEmitState esInit locusInit
      <*> findEmsByLocusInEmitState esLoop locusLoop

-- | Match loci in two typing states by their partition.
matchLociInTState :: TState -> TState -> [(Locus, Locus)]
matchLociInTState TState{_sSt=st1} TState{_sSt=st2} =
  Map.elems $ Map.intersectionWithKey merge (mkTree st1) (mkTree st2)
  where
    merge part t1 t2 = both (uncurry3 (`Locus` part)) (t1, t2)
    mkTree = Map.foldlWithKey' go Map.empty
    go mp l (p, (qt, ds)) = Map.insert p (l, qt, ds) mp

-- | Match Locus emit data from two typing state by their partition.
matchLocusEmitDataFromTStates
  :: Has (Error Builder) sig m
  => TState -> TState -> m [(LocusEmitData, LocusEmitData)]
matchLocusEmitDataFromTStates t1 t2 =
  uncurry matchLocusEmitData emSts mLoci
  where
    emSts = both _emitSt (t1, t2)
    mLoci = matchLociInTState t1 t2

-- | Take 2 type states, match emit variables by their ranges and output
-- merge scheme for each of them.
mergeMatchedTState :: (MayFail sig m, Has Trace sig m) => TState -> TState -> m [MergeScheme]
mergeMatchedTState ts1 ts2 = do
  tracep $ vsep [ pp "(mergeMatchedTState) States:"
                , incr4 (vsep [ts1, ts2])]
  let matchedLoci  = matchLociInTState ts1 ts2
      modifiedLoci = filter ensureEn matchedLoci
  matchedLocusData <- matchLocusEmitData (_emitSt ts1) (_emitSt ts2) modifiedLoci
  pure . pure . MEqual . EqualStrategy $ matchedLocusData
  where
    -- FIXME: raise a warning if any of them is not En typed
    -- FIXME: Use move strategy for non-En types
    ensureEn (l1, l2) = isEn (qty l1) && isEn (qty l2)

-- | Merge the second Locus into the first one
-- FIXME: Check if phase type is correct!
mergeScheme
  :: ( HasResolution sig m
     , GensymEmitterWithStateError sig m
     )
  => Locus -- ^ Main
  -> Locus -- ^ Servent
  -> m [MergeScheme]
mergeScheme
  stM'@Locus{loc=locMain, qty=qtMain, degrees=ptysMain}
  stA@Locus{loc=locAux, part=sAux@(NPartition nrsAux), qty=qtAux, degrees=ptysAuz} = do

  sSt %= (`Map.withoutKeys` Set.singleton locAux) -- GC aux's loc
  forM nrsAux $ \nrAux@(Normalized (Range _ _ erAux)) -> do
    -- fetch the latest 'rsMain'
    let fetchMain = uses sSt (^. at locMain)
    nrsMain <- rethrowMaybe fetchMain "WTF?" <&>
      \(NPartition nrsMain, _) -> nrsMain
    -- decide how to merge based on the if there's an adjacent range match
    rsMergeTo <- lookupAdjacentRange (denorm <$> nrsMain) (denorm nrAux)
    case rsMergeTo of
      [] -> do
        -- | Merge the standalone range into the Main partition and update range
        -- reference record in 'xSt' and the partition informaton of Main in
        -- 'sSt'
        let pM = npart $ nrAux : nrsMain
        xSt %= Map.map redirectX
        sSt %= (at locMain ?~ (pM, (qtMain, ptysMain))) -- update main's partition
        return MMove
      [nrCandidate@(Normalized (Range x elCandidate _))] -> do
        -- | Merge the range into an existing range
        -- Everyting but the range is preserved.
        -- We know that the upperbound of the candidate is the same as the
        -- lowerbound of the aux range
        let nrNew = normalize (Range x elCandidate erAux)
        let pM = npart $ (\nr -> bool nr nrNew (nr == nrCandidate)) <$> nrsMain
        xSt %= Map.map (revampX nrCandidate nrAux nrNew)
        sSt %= (at locMain ?~ (pM, (qtMain, ptysMain)))

        edlMain <- findEm (inj locMain)
        edlAux  <- findEm (inj locAux)

        jsLedMain   <- liftEd nrCandidate edlMain <$> findEm (inj nrCandidate)
        jsLedMerged <- liftEd nrAux       edlAux  <$> findEm (inj nrAux)
        jsLedInto   <- liftEd nrCandidate edlMain <$> genEmStByRange qtMain nrNew

        -- remove everything that will not show up in Into
        deleteEms [inj locAux, inj nrAux, inj nrCandidate]
        
        return $ MJoin JoinStrategy
          { jsLedMain, jsLedMerged, jsLedInto
          , jsQtMain   = qtMain
          , jsQtMerged = qtAux
          }
      _ -> throwError' "Whoops! A lot of candidates to go, which one to go?"
  where
    liftEd nr edl edr = (edl, (nr, edr))
    redirectX rAndLoc = do
      (r, locR) <- oneOf rAndLoc
      let locRNew = if locR == locAux then locMain else locR
      return (r, locRNew)
    revampX rCandidate rAux rNew rAndLoc = do
      self@(r, locR) <- oneOf rAndLoc
      return $ if r == rCandidate || r == rAux
               then (rNew, locMain)
               else self

-- | check if there exists a range that's adjacent to another.
lookupAdjacentRange
  :: ( Has (Reader IEnv) sig m
     )
  => [Range] -> Range -> m [Normalized Range]
lookupAdjacentRange rs r@(Range _ el er) = do
  (≡?) <- (allI .:) <$> liftIEnv2 (≡)
  return  [ normalize r' | r'@(Range x' el' er') <- rs, er' ≡? el ]

-- | Given two Loci in EN type where the first is for the body partition and
-- the other one is for the guard partition.
--
mergeLociHadEN
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => Locus -> Locus -> m ()
mergeLociHadEN
  stM@Locus{loc=locMain, part=(NPartition nrsMain), qty=qtMain, degrees=ptysMain}
  stA@Locus{loc=locAux , part=(NPartition nrsAux)  , qty=qtAux , degrees=ptysAux} =
  do
    -- Sanity Check
    unless (qtMain == qtAux && qtAux == TEn) $
      throwError $ stM <+> "and" <+> stA <!> "have different Q types!"

    -- start merge
    let newPartition = npart $ nrsMain ++ nrsAux
    xSt %= Map.map
      (\rLoc -> [ (nr, loc')
                | (nr, loc) <- rLoc,
                  let loc' = if loc == locAux then locMain else loc])
    sSt %=
      (`Map.withoutKeys` Set.singleton locAux) . -- GC aux's loc
      (at locMain ?~ (newPartition, (TEn, ptysMain))) -- update main's state
    return ()


--------------------------------------------------------------------------------
-- | Constraints
--------------------------------------------------------------------------------

-- | Collect constraints from specification expressions. Return collected
-- variables as well as interval information associated with.
collectConstraints
  :: ( Has (Error Builder) sig m
     , Has (Reader TEnv) sig m
     )
  => [Exp'] -> m (Map.Map Var (Interval Exp'))
collectConstraints es = (Map.mapMaybe glb1 <$>) . execState Map.empty $
  forM normalizedEs collectIntv
  where
    collectIntv e@(op, v1, e2) = do
      intv :: Interval Exp' <- case op of
            OLt -> pure $ Interval 0 e2
            OLe -> pure $ Interval 0 (e2 + 1)
            OGt -> pure $ Interval (e2 + 1) maxE
            OGe -> pure $ Interval e2 maxE
            _   -> failUninterp e
      ty <- typingExp (EVar v1)
      when (ty /= TNat) $ throwError' (errNotScalar v1 ty)
      modify $ Map.insertWith (++) v1 [intv]

    errNotScalar v ty =  v <!> " : " <!> ty <+> "is not a scalar variable."
    -- pick 100 here to avoid overflow in computation
    maxE = fromInteger $ toInteger (maxBound @Int - 100)

    failUninterp e = throwError' $
      "(" <!> viaShow e <!> ") is left uninterpreted"

    normalize e@(EOp2 op (EVar v1) e2) = pure (op , v1, e2)
    normalize (EOp2 op e2 (EVar v1))   = (, v1, e2) <$> flipLOp op
    normalize _                        = Nothing

    normalizedEs = mapMaybe normalize es

    flipLOp OLt = pure OGt
    flipLOp OLe = pure OGe
    flipLOp OGe = pure OLe
    flipLOp OGt = pure OLt
    flipLOp _   = Nothing


-- | Given an Locus for a fragment of a Had guard, match in the current
-- environment for a EN partition to be merged with this Locus.
mergeCandidateHad
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Locus -> m Locus
mergeCandidateHad st@(Locus{part=Normalized(Partition [r]), qty=THad, degrees}) = do
  ps <- use sSt <&> Map.elems
  matched <- catMaybes <$> forM ps matchMergeable
  case matched of
    [p'] -> resolvePartition (denorm p')
    _    -> throwError' $ ambiguousCandidates matched
  where
    matchMergeable (p'@(Normalized (Partition rs)), (TEn, ptysEN)) = do
      nrsAdjacent <- lookupAdjacentRange rs r
      pure $ case nrsAdjacent of
        [] -> Nothing
        _  -> Just p'
    matchMergeable _ = pure Nothing
    ambiguousCandidates matched = vsep
      [ "There're more than one merge candidate for " <!> st <!> "."
      , incr4 (vsep matched) ]
mergeCandidateHad st =
  throwError' $ st <!> " may not be a Had guard partition."

--------------------------------------------------------------------------------
-- | Helpers
--------------------------------------------------------------------------------
appkEnvWithBds :: Bindings () -> TEnv -> TEnv
appkEnvWithBds bds = kEnv %~ appBds
  where appBds = Map.union $ Map.fromList [(v, inj t) | Binding v t <- bds]

bdTypes :: Bindings () -> [Ty]
bdTypes b = [t | Binding _ t <- b]


-- TODO: Refactor this function to use a locus
-- | Construct from a scratch a new TState containing the given partitons.
tStateFromPartitionQTys
  :: ( Has (Gensym Emitter) sig m
     , Has (Gensym Var) sig m
     , Has (Error Builder) sig m
     , Has Trace sig m
     )
  => [(Partition, QTy, [Int])] -> m (TState, [Locus])
tStateFromPartitionQTys pqts = runState initTState $ do
  forM pqts ((fst <$>) . uncurry3 extendState')

-- | Extend the typing state with a partition and its type, generate emit
-- symbols for every range in the partition and return all emit data
-- the same order as those ranges.
extendState'
  :: ( GensymEmitterWithStateError sig m
     , GensymMeta sig m
     , Has Trace sig m
     )
  => Partition -> QTy -> [Int] -> m (Locus, LocusEmitData)
extendState' p' qt dgrs = do
  trace "* extendState"
  sLoc <- gensymLoc "receiver"
  -- "receive" a new locus
  sSt %= (at sLoc ?~ (np, (qt, dgrs)))
  -- update range state
  let xMap = [ (v, [(nr, sLoc)]) | nr@(Normalized (Range v _ _)) <- nranges ]
  xSt %= Map.unionWith (++) (Map.fromListWith (++) xMap)
  let sLocus = Locus{loc=sLoc, qty=qt, part=np, degrees=dgrs}
  (sLocus, ) <$> genEmStFromLocus sLocus
  where
    np@(NPartition nranges) = normalize p'

extendState
  :: ( GensymEmitterWithStateError sig m
     , GensymMeta sig m
     , Has Trace sig m
     )
  => Partition -> QTy -> [Int] -> m LocusEmitData
extendState p q d = snd <$> extendState' p q d
