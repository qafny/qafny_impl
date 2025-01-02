{-# LANGUAGE
    NamedFieldPuns
  , TypeApplications
  , TypeFamilies
  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Qafny.Codegen.Lambda(codegenLambda) where

import           Control.Exception
    (assert)
import           Data.Sum
    (Injection (inj))
import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Codegen.Merge
    (codegenMergeScheme)
import           Qafny.Codegen.Phase
    (codegenPhaseLambda, codegenPromotion)
import           Qafny.Codegen.SplitCast
    (castPartitionEN, castPartitionEN', codegenSplitThenCastEmit)
import           Qafny.Codegen.Utils
    (putOpt)
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
    (EmitData (evPhaseRef))
import           Qafny.Syntax.IR
import           Qafny.Syntax.Subst
import           Qafny.Typing
    (castScheme, checkSubtypeQ, mergeScheme, promotionScheme, resolvePartition',
    splitThenCastScheme)
import           Qafny.Typing.Error
import           Qafny.Typing.Lambda
import           Qafny.Typing.Range
    (areRangesEquiv)
import           Qafny.Typing.Typing
    (resolveRangesStrictIntoLoci)
import           Qafny.Utils
    (findEmitBasesByRanges, findEmitBasisByRange, findVisitEm, fsts,
    gensymBinding, haveSameLength, visitEm, tracep)
import           Qafny.Utils.Common
import           Text.Printf
    (printf)


throwError'
  :: ( Has (Error Builder) sig m , DafnyPrinter  s)
  => s -> m a
throwError' = throwError . ("[Codegen|Lambda]" <+>)

--------------------------------------------------------------------------------
codegenLambda
  :: ( GensymMeta sig m
     , GenConditionally sig m
     , GensymEmitterWithStateError sig m
     , HasResolution sig m
     , Has (Reader QTy) sig m
     , Has Trace sig m
     )
  => Partition -> Lambda -> m [Stmt']
codegenLambda s@Partition{ranges} lam@LambdaF{bBases} = do
  haveSameLength "codegenLambda" ranges bBases
  rangesAndLoci <- resolveRangesStrictIntoLoci ranges
  lambdaTy <- analyzeLambdaType rangesAndLoci lam

  case lambdaTy of
    LamUnary (rLhs, rResolved, locusS) -> do
      qtLambda <- ask
      -- FIXME : qt' & qtLambda stuff should be turned into something better
      checkSubtypeQ (qty locusS) qtLambda
      codegenUnaryLambda rLhs rResolved locusS qtLambda lam
    LamBinary (r1Lhs, r1Resolved, locus1) (r2Lhs, r2Resolved, locus2) -> do
      (l1, sScheme1, cScheme1) <- hdlSCError $
        splitThenCastScheme locus1 TEn r1Lhs
      (l2, sScheme2, cScheme2) <- hdlSCError $
        splitThenCastScheme locus2 TEn r2Lhs
      tracep "HERE>>>>>>>>>>>>>>>"
      stmts1 <- codegenSplitThenCastEmit sScheme1 cScheme1
      stmts2 <- codegenSplitThenCastEmit sScheme2 cScheme2
      mScheme <- mergeScheme l1 l2
      stmtsM <- codegenMergeScheme mScheme
      ((stmts1 ++ stmts2 ++ fsts stmtsM) ++) <$>
        codegenLambdaEntangle ranges lam
    LamPhaseKickback ( hadEntry@((_, _, lHad), vHad, eKick)
                     , enEntry@((rLhs, _, lEn), _, _)) -> do
      (hadPhase, _) <- findVisitEm evPhaseRef (inj (loc lHad))
      (lHad', stmtCastHad) <- castPartitionEN' lHad
      (lEn', sScheme, cScheme) <- hdlSCError $
        splitThenCastScheme lEn TEn rLhs
      (enPhase, _) <- findVisitEm evPhaseRef (inj (loc lHad))
      stmtCastEn <- codegenSplitThenCastEmit sScheme cScheme
      mScheme <- mergeScheme lEn' lHad'
      stmtsM <- codegenMergeScheme mScheme
      (nowPhase, _) <- findVisitEm evPhaseRef (inj (loc lHad))
      (idxPhase, _) <- gensymBinding "i" TNat
      let stmtsPhase = codegenPhaseKickback idxPhase (eKick - (EVar vHad))
            [(nowPhase, enPhase, hadPhase)]
      pure $ stmtCastHad ++ stmtCastEn ++ fsts stmtsM ++ stmtsPhase


--------------------------------------------------------------------------------

-- | Apply the oracle function to the singleton partition on the LHS.
-- The range in the partition may be a sub-range of the resolved one.
-- When the entanglement type permits, a split is inserted.
codegenUnaryLambda
  :: ( Has (Gensym String) sig m
     , GensymEmitterWithStateError sig m
     , GenConditionally sig m
     , HasResolution sig m
     , Has Trace sig m
     )
  => Range -> Range -> Locus -> QTy -> Lambda -> m [Stmt']
codegenUnaryLambda rLhs rResolved locus qtLambda
  lam@LambdaF{ePhase, bPhase, eBases, bBases} = do
  -- do the type cast and split first
  (stS@Locus{qty=qt}, maySplit, mayCast) <-
    hdlSCError $ splitThenCastScheme locus qtLambda rLhs
  stmts <- codegenSplitThenCastEmit maySplit mayCast

  -- resolve again for consistency, debugging only
  dbgAssertConsistency rLhs stS

  -- handle promotions in phases
  stmtsPhase <- do
    promoteMaybe <- promotionScheme stS bPhase ePhase
    case promoteMaybe of
      Just promote -> codegenPromotion promote
      Nothing      -> codegenPhaseLambda stS bPhase ePhase

  -- after promotion, the locus queried before must be staled
  -- dbgAssertConsistency rLhs stS


  -- | It's important not to use the fully resolved `s` here because the OP
  -- should only be applied to the sub-partition specified in the annotation.
  (vEmit, _) <- findEmitBasisByRange (normalize rLhs)
  ((stmts ++) . (stmtsPhase ++) <$>) . putOpt $ case qtLambda of
    TEn -> return [ vEmit ::=: callMap lamSansPhase vEmit ]
    TEn01 -> do
      -- for example, we have
      --  - rLhs x[3 .. 6]
      --  - rRsv x[2 .. 8]
      -- then,
      --  - offset = 1
      --  - elFrom0 = offset
      --  - erFrom0 = eLhsUpper-eLhsLower + offset
      --            = 6 - 3 + 1 = 4
      (vInner, _) <- gensymBinding "i" TNat
      return $
        let offset = eLhsLower - eRsvLower
            (elFrom0, erFrom0) = (offset, offset + eLhsUpper-eLhsLower)
            lambda = lambdaSplit vInner elFrom0 erFrom0
        in  [ vEmit ::=: callMap lambda vEmit ]
    TNor -> return $
      let offset = eLhsLower - eRsvLower
          (elFrom0, erFrom0) = (offset, offset + eLhsUpper-eLhsLower)
      in [ vEmit ::=: bodyOnly vEmit elFrom0 erFrom0 lamSansPhase ]
    _    -> throwError' "I have no idea what to do in this case ..."

  where
    (Range _ eLhsLower eLhsUpper, Range _ _ eRsvLower) = (rLhs, rResolved)

    dbgAssertConsistency r stS = do
      (stS', _) <- resolvePartition' (Partition [r])
      assert (stS == stS') $ trace "asesrtion passed!"

    -- a function to be applied to a map that manipulates a sequence of
    -- sequences.
    bodyOnly v el er f = -- v[0..el] + Map(f, v[el..er]) + v[er..]
      (v >:@@: (0 :: Exp', el)) + callMap f (v >:@@: (el, er)) + (v >:@@: (er, mkCard v))

    -- for the EN01 case
    lambdaSplit v el er =
      simpleLambda v $ bodyOnly v el er lamSansPhase

    lamSansPhase = lambdaUnphase lam

-- | Codegen lambda that takes multiple inputs.
codegenLambdaEntangle
  :: GensymEmitterWithStateError sig m => [Range] -> Lambda -> m [Stmt']
codegenLambdaEntangle rs (LambdaF{ bBases, eBases }) = do
  vReprs <- fsts <$> findEmitBasesByRanges (normalize <$> rs)
  unless (lenBbases == lenEbases && length vReprs == lenEbases) $
    throwError' errInconsistentLength
  (iVar, _) <- gensymBinding "i" TNat
  return $ codegenApplyLambdaMany iVar vReprs bBases eBases
  where
    lenBbases = length bBases
    lenEbases = length eBases
    errInconsistentLength =
      "The numbers of lambda binders, expressions and ranges don't match with each other."

codegenApplyLambdaMany :: Var -> [Var] -> [Var] -> [Exp'] -> [Stmt']
codegenApplyLambdaMany iVar vReprs vBinders eBodies =
  [ SEmit $ vReprs :*:=: newSeqs ]
  where
    vEnv = aenvWithIndex (EVar iVar) vBinders vReprs
    substWvenv = subst vEnv
    newSeq vRepr eBody = natSeqLike vRepr (substWvenv eBody)
    newSeqs = zipWith newSeq vReprs eBodies

-- Make an `AEnv` that maps each `binder` to the `index` position of `repr`.
aenvWithIndex :: Exp' -> [Var] -> [Var] -> AEnv
aenvWithIndex idx = zipWith go
  where go binder repr = (binder, repr >:@: idx)


-- | Phase Kickback semantics for (Had, En) Locus
-- Given an En-typed locus formed by entangling a Had locus with a En locus and
-- an oracle function of shape
-- @
--   f(x, y) = (x, y + f(x))
-- @
-- where `y` points to the prior Had locus.
--
-- The function takes the ket variable to the non-Had range, the phase reference
-- to the En-like state and kickback, and a reference to the 1-qubit Had state.
--
-- Assumption: All PhaseRef must be in the first degree.
--
-- See [proposal/phaseful-had] for the math.
codegenPhaseKickback
  :: Foldable t
  => Var -> Exp' -> t (PhaseRef, PhaseRef, PhaseRef) -> [Stmt']
codegenPhaseKickback idV eFx = concatMap go
  where
    go (prNow, prEn, prHad) =
      [ prRepr prNow ::=:
        funAdd0 (prRepr prEn) + funAdd1 (prRepr prEn)
      , SAssert $ prBase prEn `eEq` prBase prHad
      , prBase prNow ::=: EVar (prBase prEn)
      ]
      where
        -- Phase of the original Had
        k = prRepr prHad >:@: (0 :: Exp')
        funAdd0 = callMap (simpleLambda idV (idV >+ (k * eFx)))
        funAdd1 = callMap (simpleLambda idV (idV >+ (k * (1 - eFx))))
