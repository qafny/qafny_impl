{-# LANGUAGE
    TupleSections
  , LambdaCase
  #-}
module Qafny.Typing.Utils where

-- | Pure utility functions related to types

import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.IR


-- | Get the amplitude representation type given its entanglement type. 
tyAmp :: QTy -> Maybe Ty
tyAmp TNor  = Nothing
tyAmp THad  = Nothing
tyAmp TEn   = Just tySr
tyAmp TEn01 = Just tySr
tyAmp TQft  = Just tySr
{-# INLINE tyAmp #-}

-- | Type of the emitted value corresponding to its original quantum type.
tyKetByQTy :: QTy -> Maybe Ty
tyKetByQTy TNor  = Just tySn
tyKetByQTy THad  = Nothing
tyKetByQTy TEn   = Just tySn
tyKetByQTy TEn01 = Just tySsn
tyKetByQTy TQft  = Just tySsn
{-# INLINE tyKetByQTy #-}

-- | Type of an emitted phase variable
-- typingPhaseEmit :: PhaseTy -> Maybe (Ty, Ty)
-- typingPhaseEmit PT0       = Nothing -- default phase is evident
-- typingPhaseEmit (PTN n _) = Just . (TNat,) . typingPhaseEmitReprN $ n


-- | Return the Repr Type for a collection of phases based on
-- the given degree of phases
typingPhaseEmitReprN :: Int -> Ty
typingPhaseEmitReprN n =
  foldr ($) TNat (replicate n TSeq)

-- the given degree of phases
emitTypeFromDegree :: Int -> Maybe Ty
emitTypeFromDegree 0 = Nothing
emitTypeFromDegree n =
  Just $ typingPhaseEmitReprN n

-- | Check if the given type is an 'EN'-like type.
isEn :: QTy -> Bool
isEn TEn01 = True
isEn TEn   = True
isEn TQft  = True
isEn _     = False


-- | STuple
modifyPty :: ([Int] -> [Int]) -> Locus -> Locus
modifyPty f st@Locus{degrees} = st{degrees=f degrees}

-- bindingsFromPtys :: [PhaseTy] -> [Binding ()]
-- bindingsFromPtys ptys = concat
--   [ [Binding vRepr ty, Binding vBase TNat]
--   | (n, PhaseRef {prRepr=vRepr, prBase=vBase}) <- getPhaseRefN ptys
--   , let ty = typingPhaseEmitReprN n
--   ]

-- | Check if the given partiton of given entanglement type is well-formed
-- w.r.t. the partition arity 
assertPartQTyArity :: Partition -> QTy -> Bool
assertPartQTyArity Partition{ranges} = \case
  TNor  -> len == 1
  THad  -> len == 1
  TEn   -> len >= 1
  TEn01 -> len >= 1
  TQft  -> len >= 2
  where
    len = length ranges
