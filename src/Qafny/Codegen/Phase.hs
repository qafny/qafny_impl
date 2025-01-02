{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , IncoherentInstances
  , MultiWayIf
  , RecordWildCards
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
  #-}

module Qafny.Codegen.Phase where

import           Control.Monad
    (forM, liftM2, when)

import           Data.List
    (nub, uncons)
import           Data.Maybe
    (fromMaybe, maybeToList)

import           Data.Sum
    (Injection (inj))

import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.Subst
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Typing.Utils
import           Qafny.Typing
    (Promotion (..), PromotionScheme (..), castScheme, queryPhaseRef,
    resolvePartition')
import           Qafny.Typing.Qft
    (typingQft)
import           Qafny.Utils.EmitBinding
import           Qafny.Utils.Utils
    (onlyOne)



-- | Phase-Related Code Generation


throwError'
  :: ( Has (Error Builder) sig m )
  => Builder -> m a
throwError' = throwError . ("[Codegen]" <+>)

-- * Generating Phase Promotion

codegenPromotionMaybe
  :: ( Has (Gensym Emitter) sig m
     , Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => Maybe PromotionScheme -> m [Stmt']
codegenPromotionMaybe = (concat <$>) . mapM codegenPromotion . maybeToList

-- | Generate code for a given PromotionScheme
-- This function doesn't mutate the Emitter state.
codegenPromotion
  :: ( Has (Gensym Emitter) sig m
     , Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => PromotionScheme -> m [Stmt']
codegenPromotion
  PromotionScheme { psPrefs=prefs
                  , psPromotion=promotion
                  } =
  case promotion of
    Promote'0'1 (i, n) rs qt ->
      codegenPromote'0'1 qt rs prefs (i, n)

-- | Promote a 0th-degree phase to 1st-degree phase
-- This function doesn't mutate the Emitter state.
codegenPromote'0'1
  :: ( Has (Gensym Emitter) sig m
     , Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => QTy -> [Normalized Range] -> [PhaseRef] -> (Exp', Exp') -> m [Stmt']
codegenPromote'0'1 qt rs prefs (i, n) = do
  vRs <- fsts <$> findVisitEms evBasis (inj <$> rs)
  let eCardVRs = mkCard <$> vRs
      -- use 0 here because `EMakeSeq` add an extra layer of `seq`
      ty = typingPhaseEmitReprN 0
  return . concat $
    [ [ vRepr ::=: (EEmit . EMakeSeq ty eCard . constLambda $ i)
      , vBase ::=: n
      ]
    | (pref, eCard) <- zip prefs eCardVRs
    , let vRepr = prRepr pref
          vBase = prBase pref
    ]

--------------------------------------------------------------------------------
-- * Generating PhaseLambda
--------------------------------------------------------------------------------
codegenPhaseLambda
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => Locus -> PhaseBinder -> PhaseExp -> m [Stmt']
codegenPhaseLambda _ PhaseWildCard PhaseWildCard =
  return []
codegenPhaseLambda st@Locus{degrees} pb pe = do
  prefs <- queryPhaseRef st
  dgrSt <- onlyOne throwError' $ nub degrees
  concat <$> forM prefs (go dgrSt pb pe)
  where
    go 1 (PhaseOmega bi bBase) (PhaseOmega ei eBase)
      (Just (PhaseRef { prRepr=vRepr, prBase=vBase}, tyPhase)) =
      let substBase = subst [(bBase, EVar vBase)] in
        return [ vRepr ::=: callMap (simpleLambda bi (substBase ei)) vRepr
               , vBase ::=: subst [(bBase, EVar vBase)] eBase
               ]
    go dgr _ _ _ = throwError' $
      "At least one of the binder"<+>pb<+>"and the specficiation"
      <+>pe<+>"is not of degree"<+>dgr<+>"."

-- * Quantum Fourier Transformation
codegenApplyQft
  :: ( GensymEmitterWithStateError sig m
     , HasResolution sig m
     )
  => Partition -> m [Stmt']
codegenApplyQft s = do
  (locus, rsMap) <- resolvePartition' s
  case rsMap of
    [(r1, r2)] -> if r1 == r2 then go locus r1
                  else throwError' (errIncompleteRange r1 r2)
    _          ->
      throwError' (pp "Qft may only be applied to exactly one range.")
  where
    go locusS r = do
      -- ensures that bases is `EN`
      (locusS, stmtCast) <- castScheme locusS TEn
      -- ensures that phases is in 1st degree
      when (degrees locusS /= [1]) $
        throwError' (pp "Degree not 1")
      newLocus <- typingQft r locusS
      codegenQft locusS newLocus
    errIncompleteRange r1 r2 = 
      "The range"<+>r1<+>"is a proper subrange of"<+>r2<+>"."


-- | Generate statements for Qft and update Emit states given the original locus
-- and the expected locus promoted by Qft.
--
codegenQft
  :: GensymEmitterWithStateError sig m
  => Locus -> Locus -> m [Stmt']
codegenQft locusEn locusQft = do
  edLocus <- findEm iloc      -- for amp+phase purpose
  (edRApplied, edRsRest) <-   -- for kets purpose
    ensuresNonEmptyEms <$> findEms (inj <$> nranges)
  ((vIdxK, _), (vIdxI, _)) <-            -- indices
    liftM2 (,) (gensymBinding "k" TNat) (gensymBinding "i" TNat)

  vKetApplied <- fst <$> visitEmBasis edRApplied
  vsKetRest   <- (fst <$>) <$> visitEmsBasis edRsRest
  -- find phase variables from the En-typed Locus
  (PhaseRef{prRepr=vpEn, prBase=vbEn}, tpEn) <- visitEm evPhaseRef edLocus

  -- update phase ed
  edLocus' <- genEmStUpdatePhase (qty locusQft) qftDegree loc
  -- find phase variables from generated phases
  (PhaseRef{prRepr=vpFresh, prBase=vbFresh}, tpFresh) <-
    visitEm evPhaseRef edLocus'

  vsKetFresh <-
    if qty locusEn == qty locusQft
    then return vsKetRest
    else genEmStUpdateKets (qty locusQft) nranges

  return $ codegenQftPure
    (vpEn, vpFresh) vKetApplied
    (vsKetRest, vsKetFresh) vIdxK vIdxI nQft
    (vbEn, vbFresh)
  where
    Locus {loc, part=NPartition{nranges}, degrees=qftDegrees} = locusQft
    iloc = inj loc              -- inject to RangeOrLoc
    qftDegree = head qftDegrees -- safe to ignore the rest
    nQft =                      -- compute Qft
      let Normalized (Range _ rl rr) = head nranges
      in mkPow2(rr - rl)
    ensuresNonEmptyEms eds = fromMaybe (error "unreachable") $ uncons eds


-- | Given a 1st degree phase repr 'vPhase' and an En typed base repr,
-- Mutate the phase representation of the En base and generate a new sequence of
-- basis kets for the tailing range.
--
-- See Proposal [hoet] for transformation.
codegenQftPure
  :: (Var, Var)     -- old (1st-degree) and new (2nd-degree) phase variables
  -> Var            -- ket variable for the applied range is re-used
  -> ([Var], [Var]) -- old and new rest variables (En -> Qft)
  -> Var            -- bind for the Qft ket "k"
  -> Var            -- bind for the inner En index "i"
  -> Exp'           -- Size of Qft
  -> (Var, Var) -- old and new variable for the phase base
  -> [Stmt']
codegenQftPure
  (vPhase1, vPhaseFresh)
  vbApplied
  (vsbRest, vsbRestFresh)
  vIdxK vIdxI eSizeK
  (vpBase1, vpBaseFresh) =
  [ SEmit $ lhs :*:=: rhs
  , SEmit $ [vpBaseFresh] :*:=: [eNewBase]
  , SAssert $ vpBase1 `eEq` eNewBase 
  ]
  -- TODO: suppress the dynamic check and compute the gcd.
  where
    lhs = [vPhaseFresh, vbApplied] ++ vsbRestFresh
    rhs = [eNewPhase  , eNewBase ] ++ ebRestLiftByK
    -- vPhase -> seq<nat>(n, ... )
    -- vBasis -> seq<nat>(n, ... )
    -- eSizeK -> N

    -- | "seq<nat>(n, i => vPhase[i] + vBasis[i] * k)"
    eNewPhaseWithFreeK =
      natSeqLike vbApplied (vPhase1 >:@: vIdxI + (vbApplied >:@: vIdxK) >* vIdxK)
    -- | "seq<seq<nat>>(N, k => _sequence_with_free_k_)"
    eNewPhase = injAst $ EMakeSeq tySn eSizeK eNewPhaseWithFreeK
    -- | "seq<nat>(N, k => k)"
    eNewBase = injAst $ EMakeSeq TNat eSizeK (EVar vIdxI)
    -- | lift the superp of kets to the superp of kets _indexed by_ `k`
    ebRestLiftByK = mkSeqConst tySn eSizeK <$> vsbRest
