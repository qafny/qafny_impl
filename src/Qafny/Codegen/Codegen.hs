{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiWayIf
  , NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
  #-}

module Qafny.Codegen.Codegen (codegenAST) where
-- | Code Generation through Fused Effects

-- Effects
import qualified Control.Carrier.Error.Church as ErrC
import qualified Control.Carrier.Error.Either as ErrE
import           Control.Carrier.Reader
    (runReader)
import           Control.Carrier.State.Strict
    (runState)
import           Control.Effect.Lens
import           Qafny.Effect

-- Handlers
import qualified Carrier.Gensym.Emit          as GEmit
import           Qafny.Gensym
    (resumeGensym)

-- Utils
import           Control.Lens
    (at, non, (%~), (?~), (^.))
import           Control.Monad
    (forM_, void)
import           Qafny.Utils.Common

import           Data.List.NonEmpty
    (NonEmpty (..))
import qualified Data.Map.Strict              as Map
import           Data.Maybe
    (catMaybes, mapMaybe)
import qualified Data.Sum                     as Sum
import           Text.Printf
    (printf)

-- Qafny
import           Qafny.Codegen.Utils
    (runWithCallStack)

import           Qafny.Analysis.Interval
    (Interval (Interval))
import           Qafny.Analysis.Partial
    (Reducible (reduce))
import           Qafny.Config
import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Typing
    (appkEnvWithBds, castScheme, checkSubtype, collectConstraints,
    mergeCandidateHad, mergeMatchedTState, mergeScheme, removeTStateByLocus,
    resolvePartition, resolvePartitions, splitScheme, splitSchemePartition,
    splitThenCastScheme, tStateFromPartitionQTys, typingExp, typingGuard,
    typingPartition)
import           Qafny.Typing.Utils
    (emitTypeFromDegree, isEn)

import           Data.Sum
    (Injection (inj))
import           Data.Tuple
    (swap)
import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Codegen.Common
    (codegenAssignEmitData, codegenAssignEmitDataF)
import           Qafny.Codegen.Had
    (codegenNorToHad)
import           Qafny.Codegen.Lambda
import           Qafny.Codegen.Merge
    (codegenMergeScheme)
import           Qafny.Codegen.Method
    (codegenMethodParams, codegenMethodReturns, genEmitSt)
import           Qafny.Codegen.Phase
    (codegenApplyQft)
import           Qafny.Codegen.Predicates
    (codegenAssertion, codegenEnsures, codegenRequires)
import           Qafny.Codegen.SplitCast      hiding
    (throwError')
import           Qafny.Syntax.Subst
import           Qafny.Typing.Error
import           Qafny.Typing.Method
    (collectMethodTypes, resolveMethodApplicationArgs,
    resolveMethodApplicationRets)
import           Qafny.Typing.Predicates
    (dropSignatureSpecs, wfSignatureFromPredicates)
import           Qafny.Typing.Typing
    (matchLocusEmitData, matchLocusEmitDataFromTStates)
import           Qafny.Utils.EmitBinding
import           Qafny.Utils.TraceF
    (Traceable (tracef))
import           Qafny.Utils.Utils
    (both, bothM, dumpSt, gensymLoc, getMethodType, tracep)

--------------------------------------------------------------------------------
-- * Introduction
-- $doc
-- Qafny-to-Dafny translation is organized into two stages:
--
--   * Type Inference/Checking
--   * Code Generation
--
-- The 'Qafny.Codegen' module implements the translation and is responsible of
-- calling typing functions to provide hints and perform type checkings.
-- $doc
--------------------------------------------------------------------------------
throwError'
  :: ( Has (Error Builder) sig m, DafnyPrinter s )
  => s -> m a
throwError' = throwError . ("[Codegen]" <+>)

--------------------------------------------------------------------------------
-- * Codegen
--------------------------------------------------------------------------------
codegenAST
  :: ( Has (Reader Configs) sig m
     , Has (Reader TEnv) sig m
     , Has Trace sig m
     )
  => AST
  -> m ([((Var, TState), Either Builder Toplevel')], Either Builder AST)
codegenAST ast = ErrC.runError handleASTError return  $ do
  Configs { stdlibPath=libPath } <- ask @Configs
  let path = libPath
  let prelude = (mkIncludes path <$> includes) ++ imports
  methodMap <- collectMethodTypes ast
  stTops <- local (kEnv %~ Map.union (Sum.inj <$> methodMap)) $
    mapM codegenToplevel ast
  let methodOutcomes = mapMaybe methodOnly stTops
  let (_, mainMayFail) = unzip stTops
  let main :: Either Builder AST = sequence mainMayFail
  let astGened = (injQDafny prelude ++) <$> main <&> (++ injQDafny finale)
  return (methodOutcomes, astGened)
  where
    handleASTError = return . ([],) . Left

    methodOnly (qOrM, a) = qOrM <&> (, a)

    injQDafny = (Sum.inj <$>)
    mkIncludes path s =
      QDafny $ "include \"" ++ path ++ "/" ++ s ++ "\""
    includes =
      [ "QPreludeUntyped.dfy"
      , "libraries/src/Collections/Sequences/Seq.dfy"
      -- , "libraries/src/Collections/Sequences/LittleEndianNat.dfy"
      , "libraries/src/NonlinearArithmetic/Power2.dfy"
      , "libraries/src/NonlinearArithmetic/Power.dfy"
      ]
    imports =
      [ QDafny ""
      , QDafny "// target Dafny version: 4.6.0"
      , QDafny "abstract module QafnyDefault {"
      , QDafny "import opened QPreludeUntyped"
      -- , QDafny "import opened LittleEndianNat"
      , QDafny "import opened Seq"
      , QDafny "import opened Power2"
      , QDafny "import opened Power"
      , QDafny "import opened DivMod"
      , QDafny ""
      ]
    finale = [ QDafny "}" ]

codegenToplevel
  :: ( Has (Reader TEnv) sig m
     , Has Trace sig m
     )
  => Toplevel'
  -> m (Maybe (Var, TState), Either Builder Toplevel')
codegenToplevel t = case unTop t of
  Sum.Inl q@(QMethod idm _ _ _ _ _ ) ->
    bimap (Just . (idm, )) (Sum.inj <$>)  <$> codegenMethod q
  Sum.Inr q -> return (Nothing, pure . Sum.inj $ q)
  where
    codegenMethod =
      runState initTState .
      ErrE.runError .
      codegenToplevel'Method

codegenToplevel'Method
  :: ( Has (Reader TEnv) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has Trace sig m
     )
  => QMethod ()
  -> m (QMethod ())
-- | The method calling convention is defined as followed.
--
-- [params] __qreg__ variables are replaced in place in the parameter list by
-- the ranges
--
-- [returns] all emitted symbols present in the final emit state will be
-- appended to the original __returns__ list in the order the same as the
-- presence of those in the parameter list
--
-- [rest] do forward declarations in the method body
--
-- TODO: to keep emitted symbols for the same __qreg__ variable in a unique
-- order, sorting those variables should also be the part of the calling
-- convention
codegenToplevel'Method q@(QMethod vMethod bds rts rqs ens Nothing) = return q
codegenToplevel'Method q@(QMethod vMethod bds rts rqs ens (Just block)) = runWithCallStack q $ do
  -- gather constraints from "requires" clauses
  boundConstraints <- local (appkEnvWithBds bds) $
    collectConstraints rqs

  -- construct the Interval environment
  let iEnv = Map.foldMapWithKey vIntv2IEnv boundConstraints
  trace . showEmit0 $
    "Constraint Sets:" <!> line <!> boundConstraints

  -- construct requires, parameters and the method body
  mty <- getMethodType vMethod
  (countMeta, (countEmit, (rbdvsEmitR', rbdvsEmitB), (appetizer, mainCourse))) <-
    local (appkEnvWithBds bds) . runReader iEnv
    $ codegenRequiresAndParams mty `resumeGensym` codegenMethodBody block

  let (rqsCG, params, stmtsMatchParams) = appetizer
  let blockCG = mainCourse
  dumpSt "Main Course"
  (stmtsMatchEnsures, returns) <- GEmit.evalGensymEmitWith @Emitter countEmit $
    runReader iEnv . codegenMethodReturns $ mty

  ensCG <- codegenEnsures ens

  -- tracep $ "**" <!> align (vsep (first viaShow <$> rbdvsEmitB))
  -- Gensym symbols are in the reverse order!
  let stmtsDeclare = fDecls rbdvsEmitB
  -- let stmtsDeclare = undefined
  let blockStmts = concat
        [ stmtsMatchParams
        , return $ qComment "Forward Declaration"
        , stmtsDeclare
        , [ qComment "Revealing"
          , SDafny "reveal Map();"
          , SDafny "reveal Pow2();"
          -- , SDafny "reveal LittleEndianNat.ToNatRight();"
          ] -- TODO: any optimization can be done here?
        , [ SDafny "" , qComment "Method Definition"]
        , inBlock blockCG
        , stmtsMatchEnsures
        ]
  return $ QMethod vMethod params returns rqsCG ensCG (Just . Block $ blockStmts)
  where

    vIntv2IEnv v' (Interval l r) = [(v', l :| [r])]

    codegenRequiresAndParams mty = do
      trace "* codegenRequiresAndParams"
      (es, eds) <- codegenRequires rqs
      stmtsCopy <- genEmitSt
      dumpSt "genEmitSt"
      return (es, codegenMethodParams mty eds, stmtsCopy)

    codegenMethodBody =
      runReader TEn .                -- | resolve λ to EN on default
      runReader True .
      (dumpSt "begin block" >>) .
      codegenBlock

    -- | Compile the foward declaration of all variables produced in compiling
    -- the body
    fDecls = mapMaybe $ uncurry fDecl


    fDecl :: Emitter -> Var -> Maybe Stmt'
    fDecl (EmBaseSeq _ ty) v' =
      Just $ mkSVar v' ty
    fDecl (EmPhaseSeq _ i) v' =
      emitTypeFromDegree i <&> mkSVar v'
    fDecl (EmPhaseBase _) v' =
      Just $ mkSVar v' TNat
    fDecl (EmAnyBinding v' t) _ =
      Just $ mkSVar v' t
    fDecl (EmAmplitude _ _) v' =
      Just $ mkSVar v' TSeqReal

    mkSVar :: Var -> Ty -> Stmt'
    mkSVar v ty = SVar (Binding v ty) Nothing

codegenBlock
  :: ( Has (Reader TEnv) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader Bool) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has Trace sig m
     )
  => Block ()
  -> m (Block ())
codegenBlock (Block stmts) =
  Block <$> codegenStmts stmts

codegenStmts
  :: ( Has (Reader TEnv) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Reader Bool) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     , Has (Reader QTy) sig m -- hints for λ type resolution
     )
  => [Stmt']
  -> m [Stmt']
codegenStmts [] = return []
codegenStmts (stmt : stmts) = do
  stmts' <- codegenStmt stmt
  (stmts' ++) <$>
    case stmt of
      SVar (Binding v t) eM -> do
        local (kEnv %~ at v ?~ Sum.inj t) $ codegenStmts stmts
      _ -> do
        codegenStmts stmts


codegenStmt
  :: ( Has (Reader TEnv) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (Reader Bool) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has Trace sig m
     )
  => Stmt'
  -> m [Stmt']
codegenStmt s = runWithCallStack s (codegenStmt' s)

codegenStmt'
  :: ( Has (Reader TEnv) sig m
     , Has (Reader Bool) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has Trace sig m
     )
  => Stmt'
  -> m [Stmt']
codegenStmt' s@(SVar (Binding v t) Nothing)  = return [s]
codegenStmt' s@(SVar (Binding v t) (Just e)) = do
  te <- typingExp e
  -- check if `t` agrees with the type of `e`
  checkSubtype t te
  codegenAlloc v e t <&> (: [])

codegenStmt' s@(_ :*=: _) =
  codegenStmt'Apply s

codegenStmt' (SIf e seps b) = do
  -- resolve the type of the guard
  (stG'@Locus{qty=qtG, degrees=ptysG}, pG) <- typingGuard e
  -- perform a split to separate the focused guard range from parition
  (stG, maySplit) <- splitSchemePartition stG' pG
  let stmtsSplit = codegenSplitEmitMaybe maySplit
  -- resolve and collect partitions in the body of the IfStmt
  -- analogous to what we do in SepLogic
  stB'@Locus{part=sB, qty=qtB, degrees=ptysB} <-
    resolvePartitions . leftPartitions . inBlock $ b
  let annotateCastB = qComment $
        printf "Cast Body Partition %s => %s" (show qtB) (show TEn)
  (stmtsCastB, stB) <- case qtB of
    TEn -> return ([], stB')
    _   ->
      (,) . (annotateCastB :)
      <$> castPartitionEN stB' <*> resolvePartition (denorm sB)
  -- act based on the type of the guard
  stmts <- case qtG of
    THad    -> codegenStmt'If'Had stG stB b
    _nothad -> undefined
  return $ stmtsSplit ++ stmtsCastB ++ stmts

codegenStmt' s@(SFor {}) =
  codegenStmt'For s

codegenStmt' (SAssert e) =
  (SAssert <$>) <$> codegenAssertion e

-- TODO: Handle arguments in the method call in one pass to codegen Repr.
codegenStmt' (SCall x eargs) = do
  mtyMaybe <- asks @TEnv (^. kEnv . at x) <&> (>>= projMethodTy)
  mty <- maybe errNoAMethod return mtyMaybe
  (rMap, pureArgs, qArgs, schemes, loci) <-
    resolveMethodApplicationArgs eargs mty
  stmtsSC <- forM schemes $ uncurry codegenSplitThenCastEmit
  -- because locis has been called, their type are no longer accurate. the
  -- actual type is then extracted from the method's `ensures` clauses
  forM_ loci removeTStateByLocus
  rets <- resolveMethodApplicationRets rMap mty
  pure $ concat stmtsSC ++
    [SEmit (fsts rets :*:=: [EEmit (ECall x (pureArgs ++ qArgs))])]
  where
    errNoAMethod = throwError' $
      "The variable"<+>x<+>"is not referring to a method."

codegenStmt' s@(SDafny {}) = return [s]

codegenStmt' s = error $ "Unimplemented:\n\t" ++ show s ++ "\n"


--------------------------------------------------------------------------------
-- * Application
--------------------------------------------------------------------------------
-- TODO: make this typesafe by using DataKinds
codegenStmt'Apply
  :: ( Has (Reader TEnv) sig m
     , Has (Reader Bool) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has Trace sig m
     )
  => Stmt'
  -> m [Stmt']
codegenStmt'Apply (s@Partition{ranges=[rApplied]} :*=: EHad) = do
  st@Locus{qty, part} <- resolvePartition s
  go st
  where
    go l@Locus{qty=TNor} = do
      (lSplit, splitMaybe) <- splitScheme l rApplied
      (lCasted, castMaybe) <- castScheme lSplit THad
      cast <- maybe
        (throwError' "Applying H over a Had locus is unimplemented.")
        pure castMaybe
      stmtsCast <- codegenNorToHad cast
      return $ codegenSplitEmitMaybe splitMaybe ++ stmtsCast
    go l = throwError' $
      "H may not be applied to locus" <+> l
codegenStmt'Apply (s :*=: EHad) =
  throwError' $ showEmit0 ("H can only be applied to one range given" <+> s)


codegenStmt'Apply stmt@(s@(Partition{}) :*=: (ELambda lam)) =
  codegenLambda s lam
codegenStmt'Apply (s :*=: (EQft b)) = codegenApplyQft s
codegenStmt'Apply _ = throwError' "What could possibly go wrong?"

--------------------------------------------------------------------------------
-- * Conditional
--------------------------------------------------------------------------------
codegenStmt'If
  :: ( Has (Reader TEnv) sig m
     , Has (Reader Bool) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has Trace sig m
     )
  => Stmt'
  -> m [Stmt']

codegenStmt'If (SIf e _ b) = do
  -- resolve the type of the guard
  (stG@Locus{qty=qtG}, pG) <- typingGuard e
  trace $ printf "Partitions collected from %s:\n%s" (showEmit0 e) (showEmit0 pG)
  -- resolve the body partition
  stB'@Locus{part=sB, qty=qtB} <- resolvePartitions . leftPartitions . inBlock $ b
  let annotateCastB = qComment $
        printf "Cast Body Partition %s => %s" (show qtB) (show TEn)
  (stmtsCastB, stB) <- case qtB of
    TEn -> return ([], stB')
    _   -> (,) . (annotateCastB :)
      <$> castPartitionEN stB' <*> resolvePartition (denorm sB)
  -- act based on the type of the guard
  stmts <- case qtG of
    THad -> codegenStmt'If'Had stG stB b
    _    -> undefined
  return $ stmtsCastB ++ stmts

codegenStmt'If _ = throwError' "What could go wrong?"

-- | Code Generation of an `If` statement with a Had partition
codegenStmt'If'Had
  :: ( Has (Reader TEnv) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (Reader Bool) sig m
     , Has Trace sig m
     )
  => Locus -> Locus -> Block'
  -> m [Stmt']
codegenStmt'If'Had stG stB' b = do
  -- 0. extract partition, this will not be changed
  -- 1. duplicate the body partition
  (stmtsDupB, corrNowFresh) <- dupState stB'
  -- 2. codegen the body
  stmtB <- SEmit . SBlock <$> codegenBlock b
  -- TODO: left vs right merge strategy
  cardsNowFresh <- codegenEnReprCard2 corrNowFresh
  stmtsSaveCards <-
    bothM saveCard `mapM` cardsNowFresh
  -- 3. merge duplicated body partitions and merge the body with the guard
  stB <- resolvePartition (denorm (part stG))
  -- stmtsG <- mergeHadGuard stG stB cardMain cardStash
  -- let stmtsMerge = mergeEmitted corr []
  -- return $ stmtsDupB ++ [stmtB] ++ [stmtCard, stmtStash] ++ stmtsMerge ++ stmtsG
  undefined
  where
    saveCard e = do
      v <- gensym "card"
      let stmt :: Stmt' = SVar (Binding v TNat) (Just e)
      pure (stmt, EVar v)


--------------------------------------------------------------------------------
-- * Loop
--------------------------------------------------------------------------------
codegenStmt'For
  :: ( Has (Reader TEnv) sig m
     , Has (Reader Bool) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has Trace sig m
     )
  => Stmt'
  -> m [Stmt']
codegenStmt'For (SFor idx boundl boundr eG invs (Just seps) body) = do
  -- statePreLoop: the state before the loop starts
  -- stateLoop:    the state for each iteration
  tracep "* enter"
  tracep "codegenAssertion"
  infoInv <- wfSignatureFromPredicates invs
  tracep "wfSignatureFromPredicates"
  let infoInvPre = substInfoPre infoInv
  tracep $ "partition on entry:"<+>list ((\(x,y,z,_)->(x,y,z)) <$> infoInvPre)
  (lsPreCasted, stmtsPreGuard, statePreLoop, stateLoop) <-
    codegenInit (dropSignatureSpecs infoInv) (dropSignatureSpecs infoInvPre)
  put stateLoop

  -- generate loop invariants
  stmtsBody <- localExtendInv $ do
    ask @IEnv >>= trace . printf "Augmented IENV: %s" . show
    tracep $ "new invariants:"<!>line<!>incr2 (align (vsep invs))
    newInvs <- forM invs codegenAssertion
    tracep $ "new invariants (extracted):"
      <!>line<!>incr2 (align (vsep (concat newInvs)))
    stSep <- typingPartition seps -- check if `seps` clause is valid
    stmtsBody <- codegenFor'Body idx boundl boundr eG body stSep (concat newInvs)

    -- update the post-loop typing state
    -- trace $ "stateLoop:\n" ++ show stateLoop
    codegenFinish stateLoop
    -- get @TState >>= trace . ("staetLoop (subst):\n" ++) . show
    pure stmtsBody

  return $ stmtsPreGuard ++ stmtsBody
  where
    -- IEnv for loop constraints
    localExtendInv = local (++ substEnv)
    substEnv = [(idx, boundl :| [boundr - 1])]

    substInfoPre info = go <$> info
      where
        subst' :: forall a . Substitutable a => a -> a
        subst' = subst [(idx, boundl)]
        go (p, qt, dgr, specs) = (subst' p, qt, dgr, subst' <$> specs)

    -- | the postcondtion of one iteration can be computed bys setting the upper
    -- bound to be the index plus one
    -- invPQtsPost = first (reduce . substP [(idx, EVar idx + 1)]) <$> invPQts

    -- | Generate code and state for the precondition after which
    -- | we only need to concern those mentioned by the loop invariants
    codegenInit infoInv infoInvPre = runWithCallStack
                                     (vsep [ pp "Init", incr4 $ vsep
                                             [ list infoInvPre
                                             , list infoInv ]]) $ do
      (lociPreLoop, stmtsPreGuard) <- unzip <$> infoInvPre `forM` \(sInv, qtInv, _) -> do
        lInv <- resolvePartition sInv
        case sInv of
          Partition [rInv] -> do
            stInv <- resolvePartition sInv
            (sInvSplit, maySplit, mayCast) <- hdlSCError $
              splitThenCastScheme lInv qtInv rInv
            (sInvSplit, ) <$> codegenSplitThenCastEmit maySplit mayCast
          _unlikelyToSplit -> pure (lInv, []) -- See [Note: CodegenInit]
      -- | This is important because we will generate a new state from the loop
      -- invariant and perform both typing and codegen in the new state!
      statePreLoop <- get @TState

      -- | Do type inference with the loop invariant typing state
      (stateLoop, lociLoop) <- tStateFromPartitionQTys infoInv
      -- | pass preLoop variables to loop ones
      matchedLocusEds <- matchLocusEmitData
         (_emitSt stateLoop) (_emitSt statePreLoop) (zip lociLoop lociPreLoop)
      stmtsEquiv <- codegenAssignEmitData True
        =<< eraseMatchedRanges matchedLocusEds
      return ( lociPreLoop
             , concat stmtsPreGuard ++ stmtsEquiv
             , statePreLoop
             , stateLoop)

    -- update the typing state by plugging the right bound into the index
    -- parameter

    -- | [ASSIGN A TICKET HERE]
    -- FIXME: there's a bug here though, if we have something like
    --
    --   for i := 0 to 10
    --      invariant i != 10 ==> <some partition>
    --
    -- This logically valid predicate will cause a problem in generating the
    -- post-condition of the entire loop statement because I only blindly
    -- collect all partitions mentioned by the invariants without considering
    -- the semantics of the logic.
    --
    -- Type states depend solely on those invariant partitions to work
    -- correctly.
    --
    -- Here's an (unimplemented) workaround
    -- - SpecExpressions cannot be mixed with &&, || or not
    -- - SpecExpressions can only appear on the positive position in logical
    --   implication
    -- - The negative position must be a simple propositions about only linear
    --   arithmetic
    --
    -- With those restrictions, I can collect negative position propositions and
    -- solve and decide when to include predicates using the PE engine.
    --
    codegenFinish tsLoop = do
      trace $ show (idx, boundr)
      put $ subst [(idx, boundr)] tsLoop

codegenStmt'For _ = throwError' "What could possibly go wrong?"



-- | Code generation of the body statements of a for loop construct
codegenFor'Body
  :: ( Has (Reader TEnv) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (Reader Bool) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has Trace sig m
     )
  => Var   -- ^ index variable
  -> Exp'   -- ^ lower bound
  -> Exp'   -- ^ upper bound
  -> GuardExp   -- ^ guard expression
  -> Block' -- ^ body
  -> Locus -- ^ the partition to be merged into
  -> [Exp'] -- ^ emitted invariants
  -> m [Stmt']
codegenFor'Body idx boundl boundr eG body stSep@(Locus{qty=qtSep}) newInvs = do
  let psBody = leftPartitions . inBlock $ body
  -- FIXME: what's the difference between pG and sG here?
  (stG@Locus{part=sG, qty=qtG }, pG) <- typingGuard eG
  trace $ printf "The guard partition collected from %s is %s" (showEmit0 eG) (showEmit0 pG)
  trace $ printf "From invariant typing, the guard partition is %s from %s" (showEmit0 pG) (show stG)

  -- ^ FIXME: these two handling of guard state seems ungrounded

  (stmtsPrelude, stmtsBody) <- case qtG of
    THad -> do
      stB'@Locus{part=sB, qty=qtB} <- resolvePartitions psBody

      -- It seems that castEN semantics maybe unnecessary with invariant typing?
      (stmtsCastB, stB) <- case qtB of
        _ | isEn qtB -> return ([], stB')
        _            ->
          (,) <$> castPartitionEN stB' <*> resolvePartition (denorm (part stB'))

      -- what to do with the guard partition is unsure...
      -- TODO: This comment seems out-of-date
      -- (stmtsDupG, gCorr) <- dupState sG
      ------------------------------------------------------------
      -- Begin compiling the body
      ------------------------------------------------------------
      stateIterBegin <- get @TState
      dumpSt "the beginning of Had for loop"

      let rG = head (unpackPart pG)
      (stGSplited, maySplit) <- splitScheme stG rG
      let stmtsSplitG = codegenSplitEmitMaybe maySplit

      -- Splitting the guard maintains the invariant of the Had part. It remains
      -- to find a merge candidate of this split guard parition at the end of
      -- the loop.

      -- schemeC <- retypePartition stGSplited TEn
      -- let CastScheme { schVsNewEmit=vsEmitG } = schemeC
      -- let vEmitG = head vsEmitG
      -- let cardVEmitG = EEmit . ECard . EVar $ vEmitG
      -- let stmtsInitG =
      --       [ qComment "Retype from Had to EN and initialize with 0"
      --       , (::=:) vEmitG $
      --           EEmit $ EMakeSeq TNat cardVEmitG $ constExp $ ENum 0 ]
      stG' <- resolvePartition (denorm (part stGSplited))
      stmtsCGHad <- codegenStmt'For'Had stB stG' idx body
      dumpSt "the end of Had for loop"
      stmtsMatchLoopBeginEnd <-
        codegenMatchLoopBeginEnd (inferTsLoopEnd stateIterBegin)

      return ( stmtsCastB -- ++ stmtsDupG -- ++ stmtsInitG
             , stmtsSplitG ++ stmtsCGHad ++ stmtsMatchLoopBeginEnd
             )

    TEn01 -> do
      eSep <- case eG of
        GEPartition _ (Just eSep) -> return eSep
        _                         -> throwError' errNoSep
      dumpSt "(For EN01 InterBegin)"
      stateIterBegin <- get @TState

      -- 1. save the "false" part to a new variable
      -- FIXME: write general function to replicate EmitData from Locus instead.
      ledsdFalseNSaved@(_, (edSavedL, edsSavedR)) <- findThenGenLocus stG
      stmtsSaveFalse <-
        -- leftJoin semantics is disabled because it is a bug if needed.
        codegenAssignEmitData False =<< eraseMatchedRanges [swap ledsdFalseNSaved]
      -- let stmtsFocusTrue = stmtAssignSelfRest eSep <$> fsts vsEmitG

      -- save the current emit symbol table for

      -- SOLVEm?
      -- TODO: I need one way to duplicate generated emit symbols in the split
      -- and cast semantics so that executing the computation above twice will
      -- solve the problem.
      vsEmitSep <-  findEmitBasesByRanges nrsSep

      -- compile for the 'false' branch with non-essential statements suppressed
      (tsFalse, (stmtsFalse, vsFalse)) <- runState stateIterBegin $ do
        void $ installEmits ((inj (loc stG), edSavedL) : (first inj <$> edsSavedR))
        local (const False) $ codegenHalf psBody

      -- compile the for body for the 'true' branch
      (tsTrue, (stmtsTrue, vsTrue))  <- runState stateIterBegin $ do
        codegenHalf psBody

      -- compute what's needed to merge those 2 copies
      mSchemes <- mergeMatchedTState tsFalse tsTrue
      stmtsVarsMergeMatched <- codegenMergeScheme mSchemes

      -- I need a way to merge two typing state here.
      put tsFalse

      stmtsMatchLoopBeginEnd <-
        codegenMatchLoopBeginEnd (inferTsLoopEnd stateIterBegin)

      -- 4. put stashed part back
      return ( []
             , concat [ stmtsSaveFalse
                      -- , stmtsFocusTrue
                      , [qComment "begin false"]
                      , stmtsFalse
                      , [qComment "end false"]
                      , [qComment "begin true"]
                      , stmtsTrue
                      , [qComment "end true"]
                      , [qComment "begin true-false"]
                      , fst <$> stmtsVarsMergeMatched
                      , [qComment "end true-false"]
                      , [ qComment "Match Begin-End"]
                      , stmtsMatchLoopBeginEnd
                      ]
             )
    _    -> throwError' $ ""<+>qtG<+>"is not a supported guard type!"
  tracep $ "[codegen/for]" <!>line<!> incr4 (vsep newInvs)
  let innerFor = SEmit $ SForEmit idx boundl boundr newInvs $ Block stmtsBody
  return $ stmtsPrelude ++ [innerFor]
  where
    installEmits = mapM (uncurry extendEmSt)
    nrsSep = nranges (part stSep)
    inferTsLoopEnd =  subst [(idx, EVar idx + 1)]
    codegenHalf psBody = do
      -- 2. generate the body statements with the hint that lambda should
      -- resolve to EN01 now
      stmtsBody <- local (const TNor) $ codegenBlock body

      -- 3. Perform merge with merge scheme
      stmtsAndVsMerge <- (concat <$>) $ forM psBody $ \p -> do
        stP <- resolvePartition p
        dumpSt "premerge"
        schemes <- mergeScheme stSep stP
        dumpSt "postmerge"
        codegenMergeScheme schemes
      let (stmtsMerge, vsEmitMerge) = unzip stmtsAndVsMerge

      return (inBlock stmtsBody ++ stmtsMerge, vsEmitMerge)

    codegenMatchLoopBeginEnd stBegin = do
      -- | Next, I need to collect ranges/partitions from the invariant with
      --   [ i := i + 1 ]
      -- Match those ranges with those in [ i := i ] case to compute the
      -- corresponding emitted variables
      --
      -- Use collected partitions and merge it with the absorbed typing state.
      stEnd <- get @TState
      tracep $ vsep [ pp "(codegenMatchLoopBeginEnd) States:"
                    , incr4 (vsep [stBegin, stEnd])]
      matchedleds <- matchLocusEmitDataFromTStates stBegin stEnd
      tracep $ vsep [ pp "(codegenMatchLoopBeginEnd) Matched:"
                    , incr4 (list (both LocusEmitData' <$> matchedleds))]
      -- leftJoin because it's safe for the loop invariants to over-approximate
      -- the resulting state at the end of each iteration.
      codegenAssignEmitData True =<< eraseMatchedRanges matchedleds


    errNoSep = "Insufficient knowledge to perform a separation for a EN01 partition "


-- | Code Generation of a `For` statement with a Had partition
codegenStmt'For'Had
  :: ( Has (Reader TEnv) sig m
     , Has (Reader IEnv) sig m
     , Has (Reader QTy) sig m
     , Has (Reader Bool) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has (Gensym String) sig m
     , Has (Gensym Emitter) sig m
     , Has Trace sig m
     )
  => Locus -> Locus -> Var -> Block'
  -> m [Stmt']
codegenStmt'For'Had stB stG vIdx b = do
  -- 0. extract partition, this will not be changed
  -- 1. duplicate the body
  (stmtsDupB, corrNowFresh) <- dupState stB
  () <- tracef ">>>>> \n%s" (showEmitI 4 (vsep stmtsDupB))
  -- 3. codegen the body
  stmtB <- SEmit . SBlock <$> codegenBlock b

  -- 5. (Proposed) compute the value for the had ket from the counter and the
  -- body cardinality
  --
  -- (cardMain, cardStash) <- cardStatesCorr corrB
  -- let stmtsUpdateG =
  --       hadGuardMergeExp vEmitG tEmitG cardMain cardStash (EVar vEmitCounter)
  --
  -- TODO: in the current implementation, if the number of kets is changed in
  -- the body, this strategy is incorrect!

  -- 5. (Compromised) double the counter
  stToBeMerged <- mergeCandidateHad stG
  mayMerge <- mergeScheme stToBeMerged stG
  (stmtsMergeG, vsSkip) <- unzip <$> codegenMergeScheme mayMerge

  -- 4. merge the main body with the stashed
  stmtsMergeB <- mergeEmitted corrNowFresh vsSkip

  return $ stmtsDupB ++ [stmtB] ++ stmtsMergeB ++ stmtsMergeG

mergeHadGuard
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Locus -> Locus -> Exp' -> Exp' -> m [Stmt']
mergeHadGuard = mergeHadGuardWith (ENum 0)


-- | Assume `stG` is a Had guard, cast it into `EN` type and merge it with
-- the partition in`stB`.
--
-- The merge is done by concatenating the representation of generated and the
-- stashed states. Therefore, the number of kets in the generated states depends
-- on the number of kets in the body and that in the stashed body.
--
--
--
mergeHadGuardWith
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Gensym Emitter) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Exp' -> Locus -> Locus -> Exp' -> Exp' -> m [Stmt']
mergeHadGuardWith eBase stG' stB cardBody cardStashed =
  castScheme stG' TEn >>= undefined -- maybe (return []) go
  -- where
    -- go (_, _, vGENow, tGENow) = do
    --   stG <- resolvePartition (part stG')
    --   mergeLociHadEN stB stG
    --   return $ hadGuardMergeExp vGENow tGENow cardBody cardStashed eBase

hadGuardMergeExp :: Var -> Ty -> Exp' -> Exp' -> Exp' -> [Stmt']
hadGuardMergeExp vEmit tEmit cardMain cardStash eBase =
  let ~(TSeq tInSeq) = tEmit
  in [ qComment "Merge: Body partition + the Guard partition."
     , (vEmit ::=:) $
       (EEmit . EMakeSeq tInSeq cardStash $ constLambda eBase) +
       (EEmit . EMakeSeq tInSeq cardMain $
         constLambda $ reduce $ eBase >+ (1 :: Exp'))
     ]

-- codegenMergePhase
--   :: ( Has (Gensym Emitter) sig m
--      , Has (Error Builder) sig m
--      )
--   => PhaseTy -> PhaseTy -> m (PhaseTy, [Stmt'])
-- codegenMergePhase p PT0 = return (p, [])
-- codegenMergePhase PT0 p = return (p, [])
-- codegenMergePhase (PTN n1 pr1) (PTN n2 pr2) = do
--    when (n1 /= n2) $
--      throwError' "I don't know how to merge two phases of different degrees."
--    if | prBase pr1 == prBase pr2 -> do
--           let vRepr1 = prRepr pr1
--               vRepr2 = prRepr pr2
--           return (PTN n1 pr1, [ vRepr1 ::=: (EVar vRepr1 + EVar vRepr2) ])
--       | otherwise -> do
--           throwError' "Merging phase types of differnt types is unimplemented. "

-- | Emit two expressions representing the number of kets in two states in
-- correspondence
codegenEnReprCard2
  :: ( Has (Error Builder) sig m )
  => [(EmitData, EmitData)]
  -> m [(Exp', Exp')]
codegenEnReprCard2 =
  (catMaybes <$>) . codegenAssignEmitDataF False go
  where
    go (v1, TSeq _) (v2, TSeq _) = Just (mkCard v1, mkCard v2)
    go _ _                       = Nothing
-- codegenEnReprCard2 a =
--   throwError' "State cardinality of an empty correspondence is undefined!"

-- Merge the two partitions in correspondence
mergeEmitted :: MayFail sig m => [(EmitData, EmitData)] -> [Var] -> m [Stmt']
mergeEmitted corrNowFresh excluded =
  catMaybes <$> codegenAssignEmitDataF False go corrNowFresh
  where
    go (v1, t1) (v2, t2) | v1 `notElem` excluded =  Just $
      v1 ::=: EOp2 OAdd (EVar v2) (EVar v1)
                         | otherwise  = Nothing

-- | Generate statements that allocate qubits if it's Nor; otherwise, keep the
-- source statement as is.
codegenAlloc
  :: ( Has (Gensym Emitter) sig m
     , Has (Gensym String) sig m
     , Has (State TState)  sig m
     , Has (Error Builder) sig m
     )
  => Var -> Exp' -> Ty -> m Stmt'
codegenAlloc v e@(EOp2 ONor e1 e2) t@(TQReg _) = do
  let eEmit = EEmit $ EMakeSeq TNat e1 $ constLambda e2
  let nrV@(Normalized rV) = normalize $ Range v (ENum 0) e1
      partI = Normalized (partition1 rV)
      part = normalize $ Partition [rV]
  loc <- gensymLoc v
  xSt %= (at v . non [] %~ ((nrV, loc) :))
  sSt %= (at loc ?~ (part, (TNor, [])))
  let locus = Locus{ loc, part=partI, qty=TNor, degrees=[] }
  (edL, Identity (_, edR)) <- genEmStFromLocus locus
  (vEmit, _) <- visitEmBasis edR
  return $ vEmit ::=: eEmit
codegenAlloc v e@(EOp2 ONor _ _) _ =
  throwError' "Internal: Attempt to create a Nor partition that's not of nor type"
codegenAlloc v e _ = return $ (::=:) v e


-- * Notes
-- ** Note: CodegenInit
-- Q: What is a reasonable split if there are 2 ranges? Would this have
--    a slightly different meaning? For example, consider a EN partition
--
--       { q[0..i], p[0..i] } ↦ { Σ_i . ket(i, i) }
--
--    In this case, split will also requires a non-trivial merge?
--    How to do this?
--
-- A: In this case, I don't want to do casts or splits at this
-- moment. But this will be a serious limitation given that I haven't
-- allow explicit casts/splits yet. There's a way to do it by using
-- λ. However, this falls into the category of undocumented, probably
-- undefined tricks.
--
-- But why would one split and cast a real partition?
-- If we have a partition made up of two ranges, it's likely that
-- both ranges are in entanglement, therefore, there's no reasonable
-- way to split them. To entangle two partitions into one, one should
-- use a single If statement to perform the initialization.
--
-- ------------------------------------------------------------------
-- Here, I simply allow this by performing NO cast/split.
