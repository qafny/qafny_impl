--------------------------------------------------------------------------------
-- |
-- Code generation of quantum state predicates and specifications.
--------------------------------------------------------------------------------

{-# LANGUAGE
    LambdaCase
  , MultiWayIf
  , TypeFamilies
  #-}

module Qafny.Codegen.Predicates(
  codegenAssertion, codegenRequires, codegenEnsures
  ) where


import           Prelude                  hiding
    (pred)

-- Effects
import           Data.Maybe

-- Qafny
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.Emit
import           Qafny.Syntax.IR
import           Qafny.Typing.Typing
    (extendState, resolvePartition, typingPartitionQTy)

import           Control.Arrow
    (Arrow (second))
import           Control.Carrier.Reader
    (runReader)
import           Control.Monad
    (forM, when)
import           Data.Foldable
    (Foldable (toList), foldrM)
import           Data.Functor
    ((<&>))
import           Data.List
    (sort)
import           Data.Sum
    (Injection (inj))
import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Analysis.Partial
    (Reducible (reduce))
import           Qafny.Codegen.Utils
    (runWithCallStack)
import           Qafny.Syntax.EmitBinding
import           Qafny.Typing.Partial
    (instLubIntv)
import           Qafny.Typing.Predicates
    (wfQTySpecs, wfSignatureFromPredicate')
import           Qafny.Utils.EmitBinding
import           Qafny.Utils.Partial
    (gatherDynBoundsFromSpec)
import           Qafny.Utils.Utils
    (onlyOne, tracep)


throwError'
  :: ( Has (Error Builder) sig m )
  => Builder -> m a
throwError' = throwError . ("[Codegen/Predicates]" <%>)


-- | Take in an /assertional/ expression, perform type check and emit
-- corresponding expressions
codegenAssertion
  :: ( HasResolution sig m
     , GenConditionally sig m
     )
  => Exp' -> m [Exp']
codegenAssertion e = runWithCallStack e (codegenAssertion' e)

codegenAssertion'
  :: ( HasResolution sig m
     , GenConditionally sig m
     )
  => Exp' -> m [Exp']
codegenAssertion' (ESpec s' qt espec) = do
  st@Locus{part, degrees=dgrs} <- typingPartitionQTy s' qt
  -- FIXME: do something seriously when (part /= s)
  when (sort (nranges part) /= sort (nranges s)) $
    throwError' ("Assertion:"<+>part<+>"is inconsistent with"<+>s<+>".")
  led <- findEmsByLocus st
  tracep . vsep $
    [ pp "codegenAssertion'"
    , incr4 $ vsep [ pp qt, list espec ]
    ]
  wfRels <- wfQTySpecs qt espec
  codegenSpecExp led wfRels
  where
    s = normalize s'
codegenAssertion' e = return [e]
  -- FIXME: what to do if [e] contains an assertion?



-- | Take in the emit variable corresponding to each range in the partition and the
-- partition type; with which, generate expressions (predicates)
codegenSpecExp
  :: forall sig m .
     ( MayFail sig m, HasPContext sig m )
  => LocusEmitData -> SRel -> m [Exp']
codegenSpecExp (locusEd, rangesEd) srel = runWithCallStack srel $ do
  lub1' <- instLubIntv
  go lub1' srel
  where
    go :: ([Intv] -> Maybe Intv) -> SRel -> m [Exp']
    go _ (RNor specs) = do
      (nr, ed) <- onlyOne throwError' rangesEd
      vKet <- fst <$> visitEmBasis ed
      return $
        assertReprLengthIsRangeSize vKet nr
        : (codegenSpecExpNor vKet (predFromRange nr) <$> specs)
    go _ (RHad specs) = do
      (range, _) <- onlyOne throwError' rangesEd
      return $ case evPhaseRef locusEd of
        Nothing -> []
        Just (ref@PhaseRef{prRepr}, _) ->
          assertReprLengthIsRangeSize prRepr range
          : concatMap (codegenSpecExpHad ref (predFromRange range)) specs
    go lub1' (REn specs) = do
      (vAmp, _) <- visitEm evAmp locusEd
      let refMaybe = fst <$> evPhaseRef locusEd
          rvKets = second ((fst <$>) . evBasis) <$> rangesEd
          vKetRs = mapMaybe ((fst <$>) . evBasis . snd) rangesEd
          dynBounds = gatherDynBoundsFromSpec srel
      dBLub <- maybe (errUndecidableDynBound dynBounds) pure (lub1' dynBounds)
      return $
        assertReprLengthIsDyn dBLub vAmp
        : (assertReprLengthIsDyn dBLub <$> vKetRs)
        ++ maybeToList (assertReprLengthIsDyn dBLub . prRepr <$> refMaybe)
        ++ concatMap (codegenSpecExpEn vAmp refMaybe rvKets) specs
    go lub1' (REn01 specs) = do
      (vAmp, _) <- visitEm evAmp locusEd
      let refMaybe  = fst <$> evPhaseRef locusEd
          rvKets    = second ((fst <$>) . evBasis) <$> rangesEd
          vKetRs    = mapMaybe ((fst <$>) . evBasis . snd) rangesEd
          dynBounds = gatherDynBoundsFromSpec srel
      dBLub <- maybe (errUndecidableDynBound dynBounds) pure (lub1' dynBounds)
      return $
        assertReprLengthIsDyn dBLub vAmp
        : (assertReprLengthIsDyn dBLub <$> vKetRs)
        ++ maybeToList (assertReprLengthIsDyn dBLub . prRepr <$> refMaybe)
        ++ concatMap (codegenSpecExpEn01 vAmp refMaybe rvKets) specs
    go _ RWild =
      throwError' (pp "Unexpected wildcard specification.")

    assertReprLengthIsDyn (Intv _ er) vKet =
      mkCard vKet `eEq` reduce er

    assertReprLengthIsRangeSize vKet r =
      mkCard vKet `eEq` rangeSize (denorm r)

    predFromRange r =
      let bound = Intv 0 (rangeSize (denorm r))
      in predFromIntv  bound

    errUndecidableDynBound bs = throwError' $ vsep
      [ pp "Cannot decide the least upperbound of the following intervals."
      , incr4 . align . list $ bs
      ]



codegenSpecExpNor :: Var -> (Var -> Exp') -> SpecNor -> Exp'
codegenSpecExpNor vKet pred SpecNorF{norVar, norKet} =
  mkForallEq norVar pred vKet norKet

codegenSpecExpHad :: PhaseRef -> (Var -> Exp') -> SpecHad -> [Exp']
codegenSpecExpHad pr pred SpecHadF{hadVar, hadPhase} =
  codegenPhaseSpec pr hadVar pred hadPhase

codegenSpecExpEn
  :: Var -> Maybe PhaseRef -> [(r, Maybe Var)] -> SpecEn -> [Exp']
codegenSpecExpEn vAmp prMaybe rvKets
  SpecEnF{enVarSup, enIntvSup, enAmpCoef, enPhaseCoef, enKets} =
  ampPred ++ phasePreds ++ ketPreds
  where
    predIntv = predFromIntv enIntvSup

    ampPred = codegenAmpExp vAmp enVarSup predIntv enAmpCoef
    phasePreds = concat $ prMaybe <&> \pr ->
      codegenPhaseSpec pr enVarSup predIntv enPhaseCoef
    ketPreds = -- TODO: generate mod using Range
      zipWith perKetExp rvKets enKets

    perKetExp (_, v) EWildcard = EBool True
    perKetExp (_, Nothing) _   = EBool False -- EMMMMM, I should warn instead
    perKetExp (_, Just v) eKet = mkForallEq enVarSup predIntv v eKet

codegenSpecExpEn01
  :: Var -> Maybe PhaseRef -> [(Normalized Range, Maybe Var)] -> SpecEn01 -> [Exp']
codegenSpecExpEn01 vAmp prMaybe rvKets
  SpecEn01F{en01VarSup, en01IntvSup, en01AmpCoef, en01PhaseCoef
           ,en01VarQbit, en01Kets, en01IntvQbit} =
  ampPred ++ phasePreds ++ ketPreds
  where
    predIntv = predFromIntv en01IntvSup
    predIntvQ = predFromIntv en01IntvQbit

    ampPred = codegenAmpExp vAmp en01VarSup predIntv en01AmpCoef
    -- ampPred = [ vAmp `eEq` codegenAmpExp en01AmpCoef]
    phasePreds = concatMap goPhasePreds prMaybe
    goPhasePreds pr = codegenPhaseSpec pr en01VarSup predIntv en01PhaseCoef
    ketPreds = -- TODO: geneorate mod using Range
      concat $ zipWith perKetExp rvKets en01Kets

    perKetExp (_, v)       EWildcard =
      pure $ EBool True
    perKetExp (_, Nothing) _    =
      pure $ EBool False -- EMMMMM, I should warn instead
    perKetExp (nr, Just v)  eKet =
      [ mkForallCardEq en01VarSup predIntv v (rangeSize (denorm nr))
      , mkForallEq2 en01VarSup predIntv en01VarQbit predIntvQ v eKet
      ]

codegenAmpExp :: Var -> Var -> (Var -> Exp') -> AmpExp -> [Exp']
codegenAmpExp _ _ _ ADefault = []
codegenAmpExp vAmp vIdx predIdx aexp =
  [mkForallEq vIdx predIdx vAmp eA]
  where
    eA = case aexp of
      (AISqrt en ed) -> en >// ed
      (ASin e)       -> "sin" >$ e
      (ACos e)       -> "cos" >$ e

-- | Generate a predicates over phases based on the phase type
codegenPhaseSpec :: PhaseRef -> Var -> (Var -> Exp') -> PhaseExp -> [Exp']
codegenPhaseSpec PhaseRef{prRepr, prBase} vIdx predIdx = go
  where
  go PhaseWildCard = [EBool True]
  go PhaseZ = [EBool True]
  go (PhaseOmega e n) =
    [ prBase `eEq` n
    , mkForallEq vIdx predIdx prRepr e ]
  go (PhaseSumOmega (Range x l r) e n) =
    [ prBase `eEq` n
    , mkForallEq2 vIdx predIdx x pred' prRepr e ]
    where
      pred' = predFromIntv (Intv l r)

-- | Generate predicates for require clauses.
--
-- This introduces constraints and knowledges into the current context.
codegenRequires
  :: forall m sig .
     ( GensymEmitterWithStateError sig m
     , HasPContext sig m
     , GensymMeta sig m
     , Has Trace sig m
     )
  => [Exp'] -> m ([Exp'], [EmitData])
codegenRequires rqs = do
  trace "* codegenRequires"
  sigs <- catMaybes <$> forM rqs wfSignatureFromPredicate'
  foldrM go ([], []) sigs
  where
    go :: (Partition, QTy, Maybe Int, [SpecExp]) -> ([Exp'], [EmitData])
       -> m ([Exp'], [EmitData])
    go (p, qt, ds, specs) (retEs, retEds)= do
      eds <- extendState p qt (toList ds)
      wfRels <- wfQTySpecs qt specs
      es <- runReader True $ codegenSpecExp eds wfRels
      return (es ++ retEs, eraseRanges eds ++ retEds)

codegenEnsures
  :: ( Has (State TState)  sig m
     , Has (Error Builder) sig m
     , Has Trace sig m
     )
  => [Exp'] -> m [Exp']
codegenEnsures ens =
  concat <$> forM ens (runReader initIEnv . runReader True  . codegenAssertion)

-- | Find the representation of the given range
codegenRangeRepr
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has (Reader IEnv) sig m
     , Has Trace sig m
     )
  => Range -> m (Var, Ty)
codegenRangeRepr r =
  resolvePartition (Partition [r])
  >> findEm (inj (normalize r))
  >>= visitEmBasis
