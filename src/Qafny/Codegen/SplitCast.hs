{-# LANGUAGE
    PartialTypeSignatures
  , TypeFamilies
  #-}

module Qafny.Codegen.SplitCast where
import           Control.Arrow
    (Arrow (second, first))
import           Control.Monad
    (liftM2)
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
    (maybeToList)
import           Qafny.Codegen.Amplitude
    (ampFromRepr)
import           Qafny.Effect
import           Qafny.Analysis.Partial
    (Reducible (reduce))
import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Typing
    (castScheme, resolvePartition)
import           Qafny.Utils.EmitBinding
import           Qafny.Utils.Utils
    (both)
import Qafny.Codegen.Common (codegenAssignEmitData, codegenAssignEmitData')
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Tuple (swap)

throwError'
  :: ( Has (Error Builder) sig m )
  => Builder -> m a
throwError' = throwError . ("[Codgen|SplitCast]" <+>)

--------------------------------------------------------------------------------
-- * Split Semantics
--------------------------------------------------------------------------------
codegenSplitEmitMaybe :: Maybe SplitScheme -> [Stmt']
codegenSplitEmitMaybe = maybe [] codegenSplitEmit

-- | Generate emit variables and split operations from a given split scheme.
codegenSplitEmit :: SplitScheme -> [Stmt']
codegenSplitEmit
  SplitScheme { schEdAffected=( nrAffected@(Normalized (Range _ elAff erAff))
                              , edAffL
                              , edAffR )
              , schEdSplit=(rSplit ,edSplitR)
              , schEdRemainders
              } =
  concatMap stmtsSplitRem $ NE.toList schEdRemainders ++ [(rSplit, edAffL, edSplitR)]
  where
    stmtsSplitRem (Normalized (Range _ elRem erRem), edRemL, edRemR) =
      let off  = reduce (elRem - elAff)
          size = reduce (erRem - elRem)
      in codegenSplitEd edRemL edAffL off size ++
         codegenSplitEd edRemR edAffR off size


codegenSplitEd :: EmitData -> EmitData -> Exp' -> Exp' -> [Stmt']
codegenSplitEd ed1 ed2 offset size =
  liftC splitSimple base
  ++ liftC splitSimple amp
  ++ concat (liftC splitPhase refs)
  where
    liftC f a = maybeToList $ uncurry (liftM2 f) a
    bothFst f = both ((fst <$>) . f)
    base  = bothFst evBasis (ed1, ed2)
    amp   = bothFst evAmp (ed1, ed2)
    refs  = bothFst evPhaseRef (ed1, ed2)

    splitPhase :: PhaseRef -> PhaseRef -> [Stmt']
    splitPhase pr1 pr2 =
      [ splitSimple (prRepr pr1) (prRepr pr2)
      , prBase pr1 ::=: EVar (prRepr pr2)
      ]

    splitSimple :: Var -> Var -> Stmt'
    splitSimple v1 v2 =
      v1 ::=: v2 >:@@: (offset, offset + size)


--------------------------------------------------------------------------------
-- * Split & Cast Semantics
--------------------------------------------------------------------------------
codegenSplitThenCastEmit
  :: ( Has (Error Builder) sig m
     , Has Trace sig m
     )
  => Maybe SplitScheme
  -> Maybe CastScheme
  -> m [Stmt']
codegenSplitThenCastEmit sS sC = do
  trace "* codegenSplitThenCastEmit"
  (codegenSplitEmitMaybe sS ++) <$> codegenCastEmitMaybe sC


--------------------------------------------------------------------------------
-- * Cast Semantics
--------------------------------------------------------------------------------
-- | Mutate the typing state and generate the expressions to cast
codegenCast
  :: GensymEmitterWithStateError sig m
  => Locus -> QTy -> m [Stmt']
codegenCast locus qtyInto = do
  (locus', mayScheme) <- castScheme locus qtyInto
  codegenCastEmitMaybe mayScheme

codegenCastEmitMaybe
  :: ( Has (Error Builder) sig m)
  => Maybe CastScheme -> m [Stmt']
codegenCastEmitMaybe =
  maybe (return []) codegenCastEmit



-- | Generate statements to cast the type of a locus into a given one.
--
-- A cast doesn't change the meaning of the representation while a promotion
-- does.
codegenCastEmit
  :: ( Has (Error Builder) sig m)
  => CastScheme -> m [Stmt']
codegenCastEmit
  CastScheme{ schEdsFrom=edsFrom@(lEdFrom, nrsEdFrom)
            , schEdsTo  =edsTo@(lEdTo, nrsEdTo)
            , schQtFrom
            , schQtTo
            } =
  case rules schQtFrom schQtTo of
    Nothing -> throwError' $ vsep
      [ pp $ schQtFrom <+>"cannot be casted into"<+>schQtTo
      , incr4 (second vsep edsFrom)
      , incr4 (second vsep edsTo)
      ]
    Just s  -> return s
  where
    rsEdFrom = first denorm <$> nrsEdFrom
    rsEdTo   = first denorm <$> nrsEdTo

    rules :: QTy -> QTy -> Maybe [Stmt']
    -- "nor < *"
    rules TNor TEn = do
      -- | Cast Nor to 0th degree En
      (vAmpE, TSeqReal)  <- evAmp lEdTo
      [(vKetN, TSeqNat)] <- mapM (evBasis . snd) rsEdFrom
      [(vKetE, TSeqNat)] <- mapM (evBasis . snd) rsEdTo
      return [ vKetE ::=: ("CastNorEn_Ket" >$ vKetN)
             , vAmpE ::=: ampFromRepr vKetE ]
    rules TNor TEn01 = do
      -- | Cast Nor to 0th degree En
      (vAmpE, TSeqReal)     <- evAmp lEdTo
      [(vKetN, TSeqNat)]    <- mapM (evBasis . snd) rsEdFrom
      [(vKetE, TSeqSeqNat)] <- mapM (evBasis . snd) rsEdTo
      return [ vKetE ::=: ("CastNorEn01_Ket" >$ vKetN)
             , vAmpE ::=: ampFromRepr vKetE ]

    -- "had < *"
    rules THad TEn = do
      -- | Cast a 0th/1st degree Had to 0th/1st degree En
      -- No amplitude is involved
      phaseStmts <- case (evPhaseRef lEdFrom, evPhaseRef lEdTo) of
        (Nothing, Nothing) -> Just []
        (Just (PhaseRef pvBaseH pvReprH , TSeqNat),
         Just (PhaseRef pvBaseE pvReprE , TSeqNat)) -> Just
          [ pvBaseE >::=: pvBaseH
          , pvReprE  ::=: ("CastHadEn_Phase_1st" >$* [pvReprH, pvBaseH])]
        _ -> Nothing
      [(Range _ left right, rEdTo)] <- return rsEdTo
      (vKetE, TSeqNat)              <- evBasis rEdTo
      (vAmpE, TSeqReal)             <- evAmp lEdTo
      return $
        phaseStmts ++
        [ vKetE ::=: ("CastHadEn_Ket" >$ right - left)
        , vAmpE ::=: ampFromRepr vKetE ]

    rules THad TEn01 = do
      -- | Cast a 0th/1st degree Had to 0th/1st degree En
      phaseStmts <- case (evPhaseRef lEdFrom, evPhaseRef lEdTo) of
        (Nothing, Nothing) -> Just []
        (Just (PhaseRef pvBaseH pvReprH , TSeqNat),
         Just (PhaseRef pvBaseE pvReprE, TSeqNat)) -> Just
          [ pvBaseE >::=: pvBaseH
          , pvReprE  ::=: ("CastHadEn_Phase_1st" >$* [pvReprH, pvBaseH]) ]
        _ -> Nothing
      [(Range _ left right, rEdTo)] <- return rsEdTo
      (vKetE, TSeqSeqNat)           <- evBasis rEdTo
      (vAmpE, TSeqReal)             <- evAmp lEdTo
      let card = reduce $ right - left
      let app :: Exp' = if card == 1
            -- specialized instance
            then EEmit $ "CastHadEn01_Ket1" `ECall` []
            else "CastHadEn01_Ket" >$ right - left
      return $
        phaseStmts ++
        [ vKetE ::=: app
        , vAmpE ::=: ampFromRepr vKetE ]
    rules _ _ = Nothing


-- | Convert quantum type of `s` to `newTy` and emit a cast statement with a
-- provided `op`
castWithOp
  :: GensymEmitterWithStateError sig m
  => String -> Locus -> QTy -> m (Locus, [Stmt'])
castWithOp op s newTy = do
  (newLocus, maySchemeC) <- castScheme s newTy
  (newLocus, ) <$> case maySchemeC of
       Nothing      -> return []
       Just schemeC -> go schemeC
  where
    go = undefined
    -- go CastScheme{ schVsOldEmit=vsOldEmits, schVsNewEmit=vsNewEmit} = do
    --   let partitionTy = (qty s, degrees s)
    --   -- assemble the emitted terms
    --   return . concat $
    --     [ [ qComment $ "Cast " ++ show partitionTy ++ " ==> " ++ show newTy
    --       , (::=:) vNew $ EEmit (op `ECall` [EEmit $ EDafnyVar vOld])
    --       ]
    --     | ((vOld, _), (vNew, _)) <- zip vsOldEmits vsNewEmit ]


-- | Cast the given partition to EN type!
castPartitionEN
  :: GensymEmitterWithStateError sig m
  => Locus -> m [Stmt']
castPartitionEN = (snd <$>) . castPartitionEN'

-- | Cast the given partition to EN type!
castPartitionEN'
  :: GensymEmitterWithStateError sig m
  => Locus -> m (Locus, [Stmt'])
castPartitionEN' st@Locus{loc=locS, part=s, qty=qtS} = do
  case qtS of
    TNor -> castWithOp "CastNorEN" st TEn
    THad -> castWithOp "CastHadEN" st TEn
    TEn -> throwError' $
      "Partition `"<!>st<!>"` is already of EN type."
    TEn01 -> throwError' $
      "Casting"<+>qtS<+>"to TEn is expensive therefore not advised!"
    TQft -> throwError' $
      pp "It is impossible to cast into Qft type."

-- | Duplicate the data, i.e. sequences to be emitted, by generating statements
-- that duplicates the data as well as the correspondence between the range
-- bindings, emitted variables from the fresh copy and the original emitted
-- varaibles.
--
-- However, this does not add the generated symbols to the typing environment or
-- modifying the existing bindings!
--
dupState
  :: ( GensymEmitterWithStateError sig m )
  => Locus -> m ([Stmt'], [(EmitData, EmitData)])
dupState l = do
  -- l@Locus{loc=locS, part=s, qty=qtS, degrees=ptys} <- resolvePartition s'
  ledNow <- findEmsByLocus l
  ledDup <- genEmFromLocus l
  let comm = qComment "Duplicate"
  matchedEds <- runIdentity <$> eraseMatchedRanges' (Identity (ledNow, ledDup))
  stmts <- codegenAssignEmitData False (swap <$> matchedEds)
  return (comm : stmts, matchedEds)

-- | Assemble a partition collected from the guard with bounds and emit a
-- verification time assertions that checks the partition is indeed within the
-- bound.
--
-- Precondition: Split has been performed at the subtyping stage so that it's
-- guaranteed that only one range can be in the partition
--
makeLoopRange
  :: Has (Error Builder) sig m
  => Partition -> Exp' -> Exp' -> m (Range, Exp')
makeLoopRange (Partition [Range r sl sh]) l h =
  return
    ( Range r l h
    , EEmit (EOpChained l [(OLe, sl), (OLt, sh), (OLe, h)])
    )
makeLoopRange s _ _ =
  throwError $
    "Partition `"
    <!> s
    <!> "` contains more than 1 range, this should be resolved at the typing stage"



