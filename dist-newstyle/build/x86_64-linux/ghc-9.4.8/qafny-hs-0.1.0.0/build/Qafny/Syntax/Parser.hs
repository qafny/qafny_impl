{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , NamedFieldPuns

  #-}


module Qafny.Syntax.Parser(scanAndParse) where
import qualified Qafny.Syntax.Lexer as L
import           Qafny.Syntax.ParserUtils
import           Qafny.Syntax.AST
-- import           Qafny.Codegen.NCodegen
import           Control.Monad
import           Data.Sum
import           Data.Maybe
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ([QMethod])
happyIn5 :: ([QMethod]) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (QMethod)
happyIn6 :: (QMethod) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ([Binding])
happyIn7 :: ([Binding]) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ([ PredExp ], [PredExp])
happyIn10 :: ([ PredExp ], [PredExp]) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (Ty)
happyIn11 :: (Ty) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Intv)
happyIn12 :: (Intv) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (QTy)
happyIn13 :: (QTy) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: t14 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ([Range])
happyIn15 :: ([Range]) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: t16 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (PredExp)
happyIn17 :: (PredExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: t18 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: t19 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: t20 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: t21 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (AExp)
happyIn22 :: (AExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (SpecExp)
happyIn23 :: (SpecExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (DGuardExp)
happyIn24 :: (DGuardExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (DGuardExp)
happyIn25 :: (DGuardExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (DGuardExp)
happyIn26 :: (DGuardExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (DGuardExp)
happyIn27 :: (DGuardExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: t28 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (AExp)
happyIn29 :: (AExp) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (Op2)
happyIn30 :: (Op2) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (Op2)
happyIn31 :: (Op2) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: t32 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ((Var, AExp))
happyIn33 :: ((Var, AExp)) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: t34 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: t35 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: t36 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: t37 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (Stmt)
happyIn38 :: (Stmt) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: t39 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: t40 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: t41 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: t42 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: t43 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: t44 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: t45 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: t46 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: t47 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: t48 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: t49 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: t50 -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> t50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyInTok :: (L.SToken) -> (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t8 t9 t14 t16 t18 t19 t20 t21 t28 t32 t34 t35 t36 t37 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50) -> (L.SToken)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x02\x20\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x0b\x80\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x02\x20\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x04\x00\x80\x43\x08\x30\x28\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x00\x00\x0e\x21\xc0\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x40\x00\x00\x38\x84\x00\x83\x02\x00\x00\x00\x00\x00\x00\x00\x00\x80\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\xfc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x04\x00\x80\x01\x10\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x10\x00\x00\x06\x40\x00\x02\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x40\x00\x00\x38\x84\x00\x83\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x81\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x42\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x38\x04\x00\x03\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x38\x04\x00\x03\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x01\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x04\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x62\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x62\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x01\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x38\x04\x00\x03\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x81\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x20\x00\x04\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x62\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x01\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x28\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x62\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x88\x11\x00\x00\x00\x00\x00\x00\x00\x01\x04\x00\x80\x01\x10\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x40\x31\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x80\x43\x00\x30\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x38\x04\x00\x03\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xe0\x10\x00\x0c\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x40\x00\x00\x18\x00\x01\x08\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x0e\x01\xc0\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_runParser","AST","toplevels","toplevel","returns","bindings","binding","conds","ty","intv","qty","baseTy","partition","range","spec","symT","symS","ketexpr","maySum","qstate","qspec","logicImply","logicOrExp","logicAndExp","logicNotExp","chainBExp","arithExpr","addarith","arith","arithAtomic","arithInd","expr","qbool","bexp","stmts","stmt","alt__\"\931\"__'S'__","alt__\"\8855\"__'T'__","many__stmt__","many__toplevel__","manyComma__arithExpr__","manyComma__binding__","manyComma___id__","manyComma___range__","mayket__arithExpr__","mayket__qstate__","manyComma___arithExpr__","manyComma___binding__","'_'","'1'","'S'","'T'","'o'","'O'","namedW","digits","dafny","\"method\"","\"function\"","\"ensures\"","\"requires\"","\"separates\"","\"invariant\"","\"with\"","\"at\"","\"split\"","\"for\"","\"returns\"","\"not\"","\"nat\"","\"real\"","\"int\"","\"in\"","\"bool\"","\"seq\"","\"nor\"","\"had\"","\"H\"","\"Qft\"","\"iQft\"","\"repr\"","\"measure\"","\"measured\"","\"en\"","\"taa\"","\"Q\"","\"ena\"","\"var\"","\"if\"","\"sqrt\"","\"sin\"","\"cos\"","\"\955\"","\"\931\"","\"\8853\"","\"\8855\"","\"\969\"","\"\8712\"","\"\10217\"","\"\8614\"","\"assert\"","\"forall\"","\"||\"","\"&&\"","'+'","'/'","'-'","'*'","'^'","'\\%'","'|'","'('","')'","'<'","'>'","'['","']'","'{'","'}'","id","','","\"::\"","':'","'.'","';'","\"==\"","'->'","\"=>\"","\"==>\"","\">=\"","\"<=\"","\":=\"","\"*=\"","'~'","'\\@'","%eof"]
        bit_start = st Prelude.* 138
        bit_end = (st Prelude.+ 1) Prelude.* 138
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..137]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xff\xff\xff\xff\x00\x00\xff\xff\x00\x00\xd7\xff\xea\xff\xec\xff\x00\x00\x0c\x00\x06\x00\x19\x00\x00\x00\x00\x00\x1d\x00\xf0\xff\x25\x00\x6b\x00\xd7\x01\x4a\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\xf0\xff\x6f\x00\x46\x00\xf0\xff\x53\x00\x57\x00\xf9\xff\xf9\xff\xf9\xff\xd7\x01\x00\x00\x69\x00\x67\x00\x83\x00\x00\x00\x9f\x00\x05\x01\x00\x00\x00\x00\x21\x00\x7c\x00\x8c\x00\x99\x00\xaa\x00\x7b\x00\x46\x00\x46\x00\x7d\x00\x0f\x00\xd7\x01\xd7\x01\x09\x00\x8d\x00\x00\x00\xb9\x00\x3c\x00\x00\x00\x46\x00\x46\x00\x00\x00\x00\x00\xaf\x00\xb6\x00\xcc\x00\x09\x00\x00\x00\xcb\x00\x00\x00\x00\x00\xe5\x00\xed\x00\xfa\x00\xf9\xff\xde\x00\x34\x00\x00\x00\x00\x00\x46\x00\xf0\x00\x0e\x01\x19\x01\x1c\x01\x22\x01\x46\x00\x46\x00\x46\x00\x46\x00\x00\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x46\x00\x46\x00\x46\x00\x21\x00\x21\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x00\x00\x00\x9f\x00\x00\x00\x9f\x00\x00\x00\x9f\x00\x00\x00\x23\x01\x2c\x01\x34\x01\x2d\x01\x21\x00\x00\x00\x00\x00\x46\x00\xde\x00\x33\x01\x45\x01\x00\x00\x46\x00\x1b\x01\x46\x00\x46\x01\x00\x00\x00\x00\x64\x01\x00\x00\x5e\x01\x2b\x00\xc7\xff\x99\x01\x9e\x01\x00\x00\x00\x00\x71\x01\xed\xff\x82\x01\x8d\x01\x00\x00\x00\x00\x88\x01\x00\x00\x00\x00\x00\x00\x9d\x01\x00\x00\xa2\x01\xa3\x01\x00\x00\x46\x00\x00\x00\xa5\x00\xb3\x00\x00\x00\xa7\x01\x2b\x00\x00\x00\x46\x00\xa4\x01\xa0\x01\x00\x00\xa5\x01\xa9\x01\x5a\x00\x46\x00\xb5\x01\x97\x01\xaa\x01\x46\x00\x00\x00\x00\x00\x00\x00\xae\x01\x3b\x00\x00\x00\x46\x00\x1a\x00\x00\x00\xab\x01\x00\x00\xac\x01\xb0\x01\x00\x00\xb1\x01\x46\x00\x46\x00\xb2\x01\x46\x00\xb8\x01\xb9\x01\x00\x00\x9b\x01\xb3\x01\xba\x01\x00\x00\x7f\x00\x85\x00\x09\x00\x00\x00\x00\x00\x00\x00\xbc\x01\xbe\x01\x46\x00\xb7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xc1\x01\x00\x00\x00\x00\x00\x00\xbf\x01\x00\x00\x00\x00\x00\x00\xc6\x01\xc3\x01\x2d\x00\xc0\x01\xc4\x01\xc4\x01\x46\x00\xc5\x01\xd2\xff\xc7\x01\xc9\x01\x46\x00\x00\x00\xca\x01\x00\x00\xcb\x01\x00\x00\x00\x00\xcc\x01\xce\x01\xcd\x01\x00\x00\xcf\x01\x00\x00\xc8\x01\x46\x00\x00\x00\xd0\x01\xd7\x01\xe0\x01\x46\x00\x00\x00\xd1\x01\xd6\x01\xd2\x01\xd8\x01\x9c\x01\x02\x00\x00\x00\x00\x00\x46\x00\x00\x00\x09\x00\x00\x00\x46\x00\xd5\x01\xd4\x01\xdc\x01\x00\x00\x00\x00\xd4\xff\xdd\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x17\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\xfd\xff\xed\x01\xf0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x01\x00\x00\x8a\x00\x76\x01\x59\x00\x00\x00\xf3\x00\x00\x01\x0d\x01\xf4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x00\x00\x00\x00\x00\x73\x01\x00\x00\x00\x00\x00\x00\x00\x00\xee\x01\xcf\x00\xe0\x00\x04\x00\x00\x00\xf6\x01\xf7\x01\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x02\x00\x00\x1a\x01\x10\x02\x00\x00\x00\x00\x00\x00\xc7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x0f\x01\x8a\x01\x8c\x01\x00\x00\x00\x00\x91\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x75\x01\x7b\x01\x7d\x01\x6c\x01\x40\x01\x49\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x01\x00\x00\x00\x00\x93\x01\x16\x02\x00\x00\x00\x00\x00\x00\xc9\x00\xf8\x01\x98\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x02\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x62\x01\x00\x00\x9f\x01\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xad\x00\x00\x00\x00\x00\x00\x00\xa1\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\xa6\x01\xb1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x01\x85\x01\x00\x00\xa8\x01\x63\x00\xf9\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xad\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\x01\x00\x00\x00\x00\x00\x00\x06\x02\x07\x02\xaf\x01\x00\x00\xfb\x01\x00\x00\x00\x00\xb4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x02\xfc\x01\x21\x02\x00\x00\xfe\x01\x00\x00\x00\x00\xb6\x01\x00\x00\x00\x00\x25\x02\x00\x00\xbb\x01\x00\x00\x00\x00\x01\x02\x00\x00\x03\x02\x00\x00\x81\x00\x00\x00\x00\x00\xbd\x01\x00\x00\x94\x00\x00\x00\xc2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x8f\xff\x00\x00\xfe\xff\x8f\xff\xfd\xff\x00\x00\x00\x00\x00\x00\x8e\xff\x8a\xff\x00\x00\x7e\xff\xf9\xff\x8b\xff\x00\x00\x00\x00\x00\x00\xfb\xff\xf7\xff\x00\x00\x7f\xff\xf8\xff\xf3\xff\xea\xff\xe9\xff\xe8\xff\xe7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf7\xff\xdf\xff\xd3\xff\xd0\xff\xce\xff\xcb\xff\x00\x00\xc1\xff\xc0\xff\xb7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\xf7\xff\xf7\xff\x91\xff\x00\x00\xf2\xff\x00\x00\xb6\xff\xe6\xff\x8c\xff\x00\x00\xe5\xff\xfa\xff\x00\x00\x86\xff\x00\x00\x91\xff\xa1\xff\x00\x00\xe4\xff\xa0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\xff\xf5\xff\xf6\xff\x8c\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\xbd\xff\x00\x00\xb8\xff\xbf\xff\xbc\xff\xbe\xff\xbb\xff\xb9\xff\xba\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xd2\xff\xd1\xff\xcf\xff\xc4\xff\xc8\xff\xc3\xff\xc7\xff\xc6\xff\xca\xff\xc5\xff\xc9\xff\xc2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\xb5\xff\x00\x00\x00\x00\x80\xff\x00\x00\x8d\xff\x8c\xff\x00\x00\x00\x00\x00\x00\xef\xff\xee\xff\xed\xff\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\xff\xfc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\xae\xff\x00\x00\xad\xff\xac\xff\xab\xff\x00\x00\x87\xff\x00\x00\x00\x00\x9d\xff\x00\x00\xa3\xff\x00\x00\xa7\xff\xa2\xff\x00\x00\x00\x00\x9f\xff\x00\x00\x00\x00\x00\x00\x89\xff\x88\xff\x00\x00\xcc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\xff\xb2\xff\xb0\xff\x00\x00\x00\x00\xe2\xff\x00\x00\x00\x00\x81\xff\x00\x00\x9b\xff\x00\x00\x00\x00\xa4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\x00\x00\x00\x00\x00\x00\x9c\xff\xca\xff\xc9\xff\x91\xff\xec\xff\x9e\xff\x96\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xde\xff\xd5\xff\x94\xff\x92\xff\x95\xff\x93\xff\x00\x00\x00\x00\xe0\xff\xaf\xff\xe3\xff\x00\x00\xd9\xff\xd8\xff\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\xff\x00\x00\xa6\xff\x00\x00\xa5\xff\x98\xff\x00\x00\x00\x00\x00\x00\xd4\xff\x83\xff\x82\xff\x00\x00\x00\x00\xd7\xff\x00\x00\xf7\xff\x00\x00\x00\x00\xa9\xff\x00\x00\x85\xff\x00\x00\x00\x00\x00\x00\xdb\xff\xda\xff\xf0\xff\x00\x00\xd6\xff\x91\xff\x84\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf1\xff\x97\xff\x00\x00\x00\x00\xa8\xff\xaa\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x08\x00\x05\x00\x31\x00\x08\x00\x03\x00\x16\x00\x17\x00\x18\x00\x0a\x00\x1a\x00\x1e\x00\x1f\x00\x20\x00\x15\x00\x0b\x00\x0c\x00\x3f\x00\x09\x00\x3f\x00\x4d\x00\x41\x00\x26\x00\x00\x00\x01\x00\x02\x00\x2d\x00\x54\x00\x13\x00\x03\x00\x04\x00\x48\x00\x01\x00\x02\x00\x02\x00\x2a\x00\x2b\x00\x2c\x00\x2a\x00\x2b\x00\x2c\x00\x08\x00\x31\x00\x2e\x00\x40\x00\x31\x00\x2a\x00\x36\x00\x2e\x00\x28\x00\x29\x00\x08\x00\x44\x00\x39\x00\x15\x00\x3b\x00\x3f\x00\x40\x00\x0c\x00\x3f\x00\x40\x00\x26\x00\x35\x00\x46\x00\x15\x00\x48\x00\x58\x00\x08\x00\x48\x00\x26\x00\x26\x00\x41\x00\x2e\x00\x40\x00\x30\x00\x2a\x00\x2b\x00\x2c\x00\x08\x00\x40\x00\x15\x00\x48\x00\x31\x00\x44\x00\x48\x00\x2a\x00\x2b\x00\x2c\x00\x2a\x00\x3f\x00\x04\x00\x05\x00\x31\x00\x04\x00\x05\x00\x32\x00\x3f\x00\x40\x00\x49\x00\x0b\x00\x0c\x00\x2a\x00\x2b\x00\x2c\x00\x4b\x00\x48\x00\x3f\x00\x40\x00\x31\x00\x48\x00\x0b\x00\x0c\x00\x2a\x00\x2b\x00\x2c\x00\x48\x00\x40\x00\x1a\x00\x1b\x00\x31\x00\x44\x00\x4c\x00\x3f\x00\x40\x00\x40\x00\x49\x00\x28\x00\x14\x00\x44\x00\x28\x00\x2a\x00\x48\x00\x2e\x00\x3f\x00\x40\x00\x2e\x00\x54\x00\x2f\x00\x40\x00\x0b\x00\x0c\x00\x2a\x00\x48\x00\x4f\x00\x0f\x00\x44\x00\x11\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x0b\x00\x0c\x00\x48\x00\x42\x00\x46\x00\x37\x00\x0b\x00\x0c\x00\x21\x00\x22\x00\x19\x00\x23\x00\x25\x00\x1c\x00\x1d\x00\x4e\x00\x29\x00\x2a\x00\x07\x00\x52\x00\x53\x00\x0a\x00\x21\x00\x22\x00\x0b\x00\x0c\x00\x25\x00\x45\x00\x21\x00\x22\x00\x29\x00\x2a\x00\x25\x00\x51\x00\x38\x00\x40\x00\x29\x00\x2a\x00\x0e\x00\x0f\x00\x42\x00\x11\x00\x48\x00\x13\x00\x48\x00\x19\x00\x42\x00\x22\x00\x1c\x00\x1d\x00\x25\x00\x40\x00\x4e\x00\x41\x00\x29\x00\x2a\x00\x52\x00\x53\x00\x4e\x00\x23\x00\x24\x00\x57\x00\x52\x00\x53\x00\x40\x00\x2d\x00\x19\x00\x57\x00\x2c\x00\x1c\x00\x1d\x00\x19\x00\x42\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x42\x00\x19\x00\x27\x00\x40\x00\x1c\x00\x1d\x00\x4e\x00\x27\x00\x2d\x00\x27\x00\x52\x00\x53\x00\x4e\x00\x2d\x00\x42\x00\x2d\x00\x52\x00\x53\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x45\x00\x49\x00\x0d\x00\x4e\x00\x24\x00\x25\x00\x55\x00\x52\x00\x53\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x0d\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x47\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x0d\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x55\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x0d\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x48\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x2f\x00\x48\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x40\x00\x4b\x00\x1c\x00\x1d\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x12\x00\x07\x00\x1c\x00\x1d\x00\x0a\x00\x1f\x00\x20\x00\x19\x00\x1a\x00\x44\x00\x1c\x00\x1d\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x41\x00\x3f\x00\x1c\x00\x1d\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x48\x00\x41\x00\x1c\x00\x1d\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x4a\x00\x41\x00\x1c\x00\x1d\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x41\x00\x49\x00\x1c\x00\x1d\x00\x17\x00\x18\x00\x19\x00\x49\x00\x07\x00\x1c\x00\x1d\x00\x0a\x00\x1f\x00\x16\x00\x17\x00\x18\x00\x19\x00\x41\x00\x41\x00\x1c\x00\x1d\x00\x17\x00\x18\x00\x19\x00\x18\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x18\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x18\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x40\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x4d\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x19\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x48\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x22\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x45\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x41\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x4d\x00\x19\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x19\x00\x45\x00\x40\x00\x1c\x00\x1d\x00\x49\x00\x45\x00\x40\x00\x0c\x00\x0d\x00\x49\x00\x0f\x00\x44\x00\x41\x00\x34\x00\x41\x00\x50\x00\x48\x00\x4d\x00\x49\x00\x41\x00\x03\x00\x41\x00\x33\x00\x05\x00\x41\x00\x32\x00\x06\x00\x46\x00\x4d\x00\x4d\x00\x06\x00\x51\x00\x06\x00\x06\x00\x47\x00\x4d\x00\x48\x00\x48\x00\x41\x00\x49\x00\x48\x00\x3f\x00\x48\x00\x47\x00\x41\x00\x40\x00\x05\x00\x41\x00\x48\x00\x3f\x00\x3f\x00\x44\x00\x44\x00\x44\x00\x4d\x00\x33\x00\x4c\x00\x3f\x00\x41\x00\x3f\x00\x46\x00\x09\x00\x49\x00\x47\x00\x4c\x00\x41\x00\x41\x00\x09\x00\x1e\x00\x29\x00\x29\x00\x1d\x00\x1d\x00\x2b\x00\x2b\x00\x08\x00\x2c\x00\x08\x00\x2c\x00\x06\x00\x2b\x00\xff\xff\xff\xff\x2c\x00\xff\xff\xff\xff\xff\xff\x2b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x2f\x00\x0b\x00\xfa\x00\x2f\x00\xe3\x00\x18\x00\x19\x00\x1a\x00\x06\x00\x1b\x00\xa0\x00\xa1\x00\xa2\x00\x30\x00\x56\x00\x46\x00\xfb\x00\x4d\x00\xfb\x00\xa7\x00\x24\x01\x1c\x00\x06\x00\x02\x00\x03\x00\xa3\x00\xa8\x00\x4e\x00\xe3\x00\xe4\x00\x08\x00\x02\x00\x03\x00\x03\x00\x31\x00\x32\x00\x33\x00\x31\x00\x32\x00\x33\x00\x2f\x00\x34\x00\x14\x00\x0a\x00\x34\x00\x4b\x00\x35\x00\xe5\x00\x4f\x00\x50\x00\x2f\x00\x1d\x00\x64\x00\x30\x00\x66\x00\x36\x00\x37\x00\x46\x00\x36\x00\x37\x00\x04\x00\x51\x00\x38\x00\xae\x00\x39\x00\xff\xff\x2f\x00\x40\x00\x04\x00\x08\x00\x12\x00\xe5\x00\x52\x00\xe6\x00\x31\x00\x32\x00\x33\x00\x2f\x00\x56\x00\x30\x00\x53\x00\x34\x00\x43\x00\x0f\x00\x31\x00\x32\x00\x33\x00\xa3\x00\xe7\x00\x0a\x00\x0b\x00\x34\x00\x3c\x00\x0b\x00\x02\x01\x36\x00\x37\x00\x11\x00\xc6\x00\x46\x00\x31\x00\x32\x00\x33\x00\x10\x00\x39\x00\x36\x00\x37\x00\x34\x00\x0f\x00\xd2\x00\x46\x00\x31\x00\x32\x00\x33\x00\x39\x00\x8a\x00\x60\x00\x61\x00\x34\x00\x85\x00\x03\x01\x36\x00\x37\x00\x42\x00\x8b\x00\x0c\x00\x14\x00\x43\x00\x0c\x00\x4b\x00\x39\x00\x0d\x00\x36\x00\x37\x00\x0d\x00\x8c\x00\xb4\xff\x21\x00\x45\x00\x46\x00\x4b\x00\x40\x00\x20\x00\xdc\x00\x1f\x00\x15\x01\xb4\xff\xb4\xff\xb4\xff\xb4\xff\xb4\xff\xb4\xff\x45\x00\x46\x00\x0f\x00\xb4\xff\x3c\x00\x6f\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\xdf\x00\x49\x00\x2c\x00\x2d\x00\xb4\xff\x4a\x00\x4b\x00\x15\x00\xb4\xff\xb4\xff\x16\x00\xf2\x00\x48\x00\x45\x00\x46\x00\x49\x00\x41\x00\x1d\x01\x48\x00\x4a\x00\x4b\x00\x49\x00\x70\x00\x6e\x00\x5f\x00\x4a\x00\x4b\x00\xdb\x00\xdc\x00\x6a\x00\xdd\x00\x0f\x00\xde\x00\x58\x00\x86\x00\x6a\x00\x48\x00\x2c\x00\x2d\x00\x96\x00\x5e\x00\x6b\x00\x45\x00\x4a\x00\x4b\x00\x6c\x00\x6d\x00\x6b\x00\xdf\x00\xe0\x00\xf5\x00\x6c\x00\x6d\x00\x5d\x00\xc3\x00\x86\x00\xf4\x00\xe1\x00\x2c\x00\x2d\x00\x86\x00\x6a\x00\x86\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\xcb\x00\x59\x00\x9b\x00\x5c\x00\x2c\x00\x2d\x00\x6b\x00\x87\x00\x88\x00\xb4\x00\x6c\x00\x6d\x00\xcc\x00\x88\x00\xc0\xff\x88\x00\x6c\x00\x6d\x00\x58\x00\x8e\x00\x8f\x00\x2c\x00\x2d\x00\x44\x00\x99\x00\x3a\x00\xc0\xff\x90\x00\x91\x00\x9a\x00\xc0\xff\xc0\xff\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x39\x00\x9a\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x98\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x25\x00\x80\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x96\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x91\x00\x7f\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x95\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x63\x00\x0f\x00\x2c\x00\x2d\x00\x7a\x00\x7b\x00\x93\x00\x86\x00\x2c\x00\x2d\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\xa8\x00\x28\x00\x29\x00\x2a\x00\xa9\x00\xeb\x00\x1d\x00\x2c\x00\xaa\x00\x16\x00\xab\x00\xac\x00\xec\x00\xed\x00\x85\x00\x2c\x00\x2d\x00\x72\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x84\x00\x83\x00\x2c\x00\x2d\x00\x71\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\xb4\x00\xbe\x00\x2c\x00\x2d\x00\xb9\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x82\x00\xbd\x00\x2c\x00\x2d\x00\xe8\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\xbc\x00\xbb\x00\x2c\x00\x2d\x00\x5f\x00\x2a\x00\xa9\x00\xb7\x00\x3d\x00\x2c\x00\xaa\x00\x16\x00\xc8\x00\x73\x00\x29\x00\x2a\x00\x2b\x00\xb6\x00\xb1\x00\x2c\x00\x2d\x00\x5f\x00\x2a\x00\x2b\x00\x78\x00\x79\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x76\x00\x77\x00\x74\x00\x75\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x7a\x00\xd6\x00\x78\x00\xd5\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x7e\x00\xb0\x00\x7d\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x7c\x00\xaf\x00\xb8\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\xb1\x00\xa6\x00\xcc\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\xc7\x00\x58\x00\xbe\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\xe7\x00\xa5\x00\xd3\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\xef\x00\x9e\x00\xfc\x00\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x0c\x01\x9d\x00\x13\x01\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x0f\x01\xd1\x00\x1e\x01\x2c\x00\x2d\x00\x2c\x00\x2d\x00\x1c\x01\xc1\x00\xd0\x00\x2c\x00\x2d\x00\xc2\x00\x17\x01\xcf\x00\x23\x00\x24\x00\x18\x01\x25\x00\xce\x00\xca\x00\xc3\x00\xc5\x00\xf8\x00\x58\x00\xc6\x00\x8b\x00\xea\x00\x12\x00\xd9\x00\x06\x01\x5a\x00\xf7\x00\x04\x01\x21\x00\xd8\x00\xdb\x00\xda\x00\x70\x00\xc0\x00\x54\x00\x53\x00\xef\x00\xd5\x00\x58\x00\xb4\x00\xeb\x00\xf6\x00\xf2\x00\xfb\x00\xf1\x00\x01\x01\x0f\x01\x0e\x01\x93\x00\x0c\x01\xff\x00\xe7\x00\xe7\x00\x43\x00\x09\x01\x09\x01\xfc\x00\x11\x01\x15\x01\xfb\x00\x22\x01\xe7\x00\x1a\x01\x8c\x00\x1c\x01\x21\x01\x13\x01\x20\x01\x25\x01\xb7\x00\x9e\x00\xb2\x00\xd1\x00\xff\x00\xfd\x00\x04\x01\xf8\x00\x0a\x01\x09\x01\x07\x01\x06\x01\x11\x01\x1a\x01\x00\x00\x00\x00\x18\x01\x00\x00\x00\x00\x00\x00\x22\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 129) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129)
	]

happy_n_terms = 89 :: Prelude.Int
happy_n_nonterms = 47 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn4
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happyReduce 10# 2# happyReduction_3
happyReduction_3 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (( _, L.TId happy_var_2     )) -> 
	case happyOut8 happy_x_4 of { happy_var_4 -> 
	case happyOut7 happy_x_6 of { (HappyWrap7 happy_var_6) -> 
	case happyOut10 happy_x_7 of { (HappyWrap10 happy_var_7) -> 
	case happyOut37 happy_x_9 of { happy_var_9 -> 
	happyIn6
		 (QMethod happy_var_2 happy_var_4 happy_var_6 (fst happy_var_7) (snd happy_var_7) happy_var_9
	) `HappyStk` happyRest}}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_0  3# happyReduction_4
happyReduction_4  =  happyIn7
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happyReduce 4# 3# happyReduction_5
happyReduction_5 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happySpecReduce_1  4# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happySpecReduce_3  5# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn9
		 (Binding happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_0  6# happyReduction_8
happyReduction_8  =  happyIn10
		 (([],[])
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  6# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 (((happy_var_2 : (fst happy_var_3)), (snd happy_var_3))
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_3  6# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 (((fst happy_var_3), happy_var_2 : (snd happy_var_3))
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_3  6# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 (((happy_var_2 : (fst happy_var_3)), (snd happy_var_3))
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happySpecReduce_1  7# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_3  7# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn11
		 (TArrow [happy_var_1] happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happyReduce 5# 8# happyReduction_14
happyReduction_14 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut29 happy_x_4 of { (HappyWrap29 happy_var_4) -> 
	happyIn12
		 (Intv happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_3  8# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn12
		 (Intv happy_var_2 (AOp2 OAdd happy_var_2 (ANat 1))
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_1  9# happyReduction_16
happyReduction_16 happy_x_1
	 =  happyIn13
		 (TNor
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_1  9# happyReduction_17
happyReduction_17 happy_x_1
	 =  happyIn13
		 (THad
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_1  9# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn13
		 (TEn   (ANat 0)
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happyReduce 4# 9# happyReduction_19
happyReduction_19 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn13
		 (TEn   happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_1  9# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn13
		 (TAA
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_1  10# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn14
		 (TNat
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_1  10# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn14
		 (TReal
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_1  10# happyReduction_23
happyReduction_23 happy_x_1
	 =  happyIn14
		 (TInt
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_1  10# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn14
		 (TBool
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_3  10# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	happyIn14
		 (TSeq happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happyReduce 4# 10# happyReduction_26
happyReduction_26 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn14
		 (TQReg happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_1  11# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happyReduce 6# 12# happyReduction_28
happyReduction_28 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	case happyOut29 happy_x_5 of { (HappyWrap29 happy_var_5) -> 
	happyIn16
		 (Range happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happyReduce 4# 12# happyReduction_29
happyReduction_29 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn16
		 (Range happy_var_1 happy_var_3 (AOp2 OAdd happy_var_3 (ANat 1))
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happyReduce 7# 13# happyReduction_30
happyReduction_30 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	case happyOut13 happy_x_4 of { (HappyWrap13 happy_var_4) -> 
	case happyOut23 happy_x_6 of { (HappyWrap23 happy_var_6) -> 
	happyIn17
		 (ESpec happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happyReduce 6# 13# happyReduction_31
happyReduction_31 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { (HappyWrap25 happy_var_4) -> 
	case happyOut25 happy_x_6 of { (HappyWrap25 happy_var_6) -> 
	happyIn17
		 (EForall happy_var_2 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happySpecReduce_1  13# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn17
		 (DPred happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happySpecReduce_1  14# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_1  15# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happySpecReduce_1  16# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn20
		 (
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_36 = happyReduce 5# 17# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (( _, L.TId happy_var_2     )) -> 
	case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	happyIn21
		 ([(happy_var_2,happy_var_4)]
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_37 = happyReduce 6# 17# happyReduction_37
happyReduction_37 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (( _, L.TId happy_var_2     )) -> 
	case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	case happyOut21 happy_x_6 of { happy_var_6 -> 
	happyIn21
		 ((happy_var_2,happy_var_4):happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_38 = happySpecReduce_1  18# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn22
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_39 = happySpecReduce_1  18# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn22
		 (if happy_var_1 == OAdd then AHad True else AHad False
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_40 = happyReduce 4# 19# happyReduction_40
happyReduction_40 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (( _, L.TId happy_var_2     )) -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 (SESpecTen happy_var_2 Nothing happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_41 = happyReduce 6# 19# happyReduction_41
happyReduction_41 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (( _, L.TId happy_var_2     )) -> 
	case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
	case happyOut48 happy_x_6 of { happy_var_6 -> 
	happyIn23
		 (SESpecTen happy_var_2 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_42 = happySpecReduce_1  19# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (SESpecTen "null" Nothing happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_43 = happyMonadReduce 3# 19# happyReduction_43
happyReduction_43 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	( omegaCase happy_var_1 happy_var_2 happy_var_3)}}})
	) (\r -> happyReturn (happyIn23 r))

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_44 = happySpecReduce_1  20# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn24
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_45 = happySpecReduce_3  20# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut25 happy_x_3 of { (HappyWrap25 happy_var_3) -> 
	happyIn24
		 (DImply happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_46 = happySpecReduce_3  21# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	case happyOut25 happy_x_3 of { (HappyWrap25 happy_var_3) -> 
	happyIn25
		 (DOr happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_47 = happySpecReduce_1  21# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn25
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_48 = happySpecReduce_3  22# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn26
		 (DAnd happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_49 = happySpecReduce_1  22# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn26
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_50 = happySpecReduce_2  23# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { (HappyWrap27 happy_var_2) -> 
	happyIn27
		 (DNeg happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_51 = happyReduce 4# 23# happyReduction_51
happyReduction_51 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (DFun happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_52 = happySpecReduce_1  23# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_53 = happySpecReduce_3  24# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn28
		 (DEq happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_54 = happySpecReduce_3  24# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn28
		 (DLt happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_55 = happySpecReduce_3  24# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn28
		 (DLe happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_56 = happySpecReduce_3  24# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn28
		 (DLe happy_var_3 happy_var_1
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_57 = happySpecReduce_3  24# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (DAnd (DEq happy_var_1 (getFirst happy_var_3)) happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_58 = happySpecReduce_3  24# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (DAnd (DLt happy_var_1 (getFirst happy_var_3)) happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_59 = happySpecReduce_3  24# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (DAnd (DLe happy_var_1 (getFirst happy_var_3)) happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_60 = happySpecReduce_3  24# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (DAnd (DLe (getFirst happy_var_3) happy_var_1) happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_61 = happySpecReduce_3  25# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn29
		 (AOp2 happy_var_2 happy_var_1 happy_var_3
	)}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_62 = happySpecReduce_1  25# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_63 = happySpecReduce_1  25# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	happyIn29
		 (AOp2 OInd (AVar (fst happy_var_1)) (snd happy_var_1)
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_64 = happySpecReduce_1  26# happyReduction_64
happyReduction_64 happy_x_1
	 =  happyIn30
		 (OAdd
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_65 = happySpecReduce_1  26# happyReduction_65
happyReduction_65 happy_x_1
	 =  happyIn30
		 (OSub
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_66 = happySpecReduce_1  27# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn31
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_67 = happySpecReduce_1  27# happyReduction_67
happyReduction_67 happy_x_1
	 =  happyIn31
		 (ODiv
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_68 = happySpecReduce_1  27# happyReduction_68
happyReduction_68 happy_x_1
	 =  happyIn31
		 (OMul
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_69 = happySpecReduce_1  27# happyReduction_69
happyReduction_69 happy_x_1
	 =  happyIn31
		 (OMod
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_70 = happySpecReduce_1  27# happyReduction_70
happyReduction_70 happy_x_1
	 =  happyIn31
		 (OPow
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_71 = happySpecReduce_1  27# happyReduction_71
happyReduction_71 happy_x_1
	 =  happyIn31
		 (OXor
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_72 = happySpecReduce_1  28# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOutTok happy_x_1 of { (( _, L.TLitInt happy_var_1 )) -> 
	happyIn32
		 (ANat happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_73 = happySpecReduce_1  28# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	happyIn32
		 (AVar happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_74 = happySpecReduce_3  28# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn32
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_75 = happyReduce 4# 28# happyReduction_75
happyReduction_75 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (AFun happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_76 = happySpecReduce_3  28# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn32
		 (AOp1 OLen happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_77 = happyReduce 4# 28# happyReduction_77
happyReduction_77 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn32
		 (AOp1 OSin happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_78 = happyReduce 4# 28# happyReduction_78
happyReduction_78 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn32
		 (AOp1 OCos happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_79 = happyReduce 4# 28# happyReduction_79
happyReduction_79 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn32
		 (AOp1 OSqrt happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_80 = happyReduce 6# 28# happyReduction_80
happyReduction_80 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	case happyOut29 happy_x_5 of { (HappyWrap29 happy_var_5) -> 
	happyIn32
		 (AOp2 Omega happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_81 = happyReduce 4# 29# happyReduction_81
happyReduction_81 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn33
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_82 = happySpecReduce_1  30# happyReduction_82
happyReduction_82 happy_x_1
	 =  happyIn34
		 (EHad
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_83 = happySpecReduce_1  30# happyReduction_83
happyReduction_83 happy_x_1
	 =  happyIn34
		 (EQft False
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_84 = happySpecReduce_1  30# happyReduction_84
happyReduction_84 happy_x_1
	 =  happyIn34
		 (EQft True
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_85 = happyReduce 12# 30# happyReduction_85
happyReduction_85 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_3 of { happy_var_3 -> 
	case happyOut29 happy_x_7 of { (HappyWrap29 happy_var_7) -> 
	case happyOut29 happy_x_9 of { (HappyWrap29 happy_var_9) -> 
	case happyOut47 happy_x_11 of { happy_var_11 -> 
	happyIn34
		 (ELambda happy_var_3 (Just (AOp2 Omega happy_var_7 happy_var_9)) happy_var_11
	) `HappyStk` happyRest}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_86 = happyReduce 6# 30# happyReduction_86
happyReduction_86 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_3 of { happy_var_3 -> 
	case happyOut47 happy_x_5 of { happy_var_5 -> 
	happyIn34
		 (ELambda happy_var_3 Nothing happy_var_5
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_87 = happyReduce 11# 30# happyReduction_87
happyReduction_87 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_3 of { happy_var_3 -> 
	case happyOut29 happy_x_7 of { (HappyWrap29 happy_var_7) -> 
	case happyOut29 happy_x_9 of { (HappyWrap29 happy_var_9) -> 
	happyIn34
		 (ELambda happy_var_3 (Just (AOp2 Omega happy_var_7 happy_var_9)) []
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_88 = happySpecReduce_1  31# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	happyIn35
		 (GEPar(fst happy_var_1) (snd happy_var_1)
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_89 = happyReduce 5# 31# happyReduction_89
happyReduction_89 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	case happyOut33 happy_x_5 of { (HappyWrap33 happy_var_5) -> 
	happyIn35
		 (GEq happy_var_1 happy_var_3 (fst happy_var_5) (snd happy_var_5)
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_90 = happyReduce 5# 31# happyReduction_90
happyReduction_90 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	case happyOut33 happy_x_5 of { (HappyWrap33 happy_var_5) -> 
	happyIn35
		 (GLt happy_var_1 happy_var_3 (fst happy_var_5) (snd happy_var_5)
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_91 = happySpecReduce_2  31# happyReduction_91
happyReduction_91 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (GNeg happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_92 = happySpecReduce_1  32# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn36
		 (BE happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_93 = happySpecReduce_1  32# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (QE happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_94 = happySpecReduce_1  33# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_95 = happySpecReduce_1  34# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOutTok happy_x_1 of { (( _, L.TDafny happy_var_1  )) -> 
	happyIn38
		 (SDafny happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_96 = happySpecReduce_3  34# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn38
		 (SAssert happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_97 = happyReduce 5# 34# happyReduction_97
happyReduction_97 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
	case happyOut15 happy_x_4 of { (HappyWrap15 happy_var_4) -> 
	happyIn38
		 (SCast happy_var_4 happy_var_2
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_98 = happySpecReduce_3  34# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (SVar happy_var_2 Nothing
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_99 = happyReduce 5# 34# happyReduction_99
happyReduction_99 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { (HappyWrap29 happy_var_4) -> 
	happyIn38
		 (SVar happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_100 = happyReduce 4# 34# happyReduction_100
happyReduction_100 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn38
		 (happy_var_1 ::=: happy_var_3
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_101 = happyReduce 7# 34# happyReduction_101
happyReduction_101 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_5 of { (HappyWrap15 happy_var_5) -> 
	happyIn38
		 (SMea (head happy_var_1) (tail happy_var_1) happy_var_5
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_102 = happyReduce 4# 34# happyReduction_102
happyReduction_102 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (happy_var_1 :*=: happy_var_3
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_103 = happyMonadReduce 7# 34# happyReduction_103
happyReduction_103 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen ((case happyOut36 happy_x_3 of { happy_var_3 -> 
	case happyOut37 happy_x_6 of { happy_var_6 -> 
	( ifcase happy_var_3 happy_var_6)}})
	) (\r -> happyReturn (happyIn38 r))

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_104 = happyReduce 12# 34# happyReduction_104
happyReduction_104 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (( _, L.TId happy_var_2     )) -> 
	case happyOut29 happy_x_5 of { (HappyWrap29 happy_var_5) -> 
	case happyOut29 happy_x_7 of { (HappyWrap29 happy_var_7) -> 
	case happyOut10 happy_x_9 of { (HappyWrap10 happy_var_9) -> 
	case happyOut37 happy_x_11 of { happy_var_11 -> 
	happyIn38
		 (SFor happy_var_2 happy_var_5 happy_var_7 (fst happy_var_9) happy_var_11
	) `HappyStk` happyRest}}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_105 = happyReduce 5# 34# happyReduction_105
happyReduction_105 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (SCall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_106 = happySpecReduce_1  35# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_107 = happySpecReduce_1  35# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_108 = happySpecReduce_1  36# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_109 = happySpecReduce_1  36# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_110 = happySpecReduce_0  37# happyReduction_110
happyReduction_110  =  happyIn41
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_111 = happySpecReduce_2  37# happyReduction_111
happyReduction_111 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut41 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (happy_var_1 : happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_112 = happySpecReduce_0  38# happyReduction_112
happyReduction_112  =  happyIn42
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_113 = happySpecReduce_2  38# happyReduction_113
happyReduction_113 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut42 happy_x_2 of { happy_var_2 -> 
	happyIn42
		 (happy_var_1 : happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_114 = happySpecReduce_1  39# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_115 = happySpecReduce_0  39# happyReduction_115
happyReduction_115  =  happyIn43
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_116 = happySpecReduce_1  40# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_117 = happySpecReduce_0  40# happyReduction_117
happyReduction_117  =  happyIn44
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_118 = happySpecReduce_3  41# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (happy_var_1 : happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_119 = happySpecReduce_1  41# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOutTok happy_x_1 of { (( _, L.TId happy_var_1     )) -> 
	happyIn45
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_120 = happySpecReduce_3  42# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (happy_var_1 : happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_121 = happySpecReduce_1  42# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_122 = happySpecReduce_3  43# happyReduction_122
happyReduction_122 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn47
		 ([happy_var_2]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_123 = happyReduce 4# 43# happyReduction_123
happyReduction_123 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut47 happy_x_4 of { happy_var_4 -> 
	happyIn47
		 (happy_var_2 : happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_124 = happySpecReduce_3  44# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	happyIn48
		 ([happy_var_2]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_125 = happyReduce 4# 44# happyReduction_125
happyReduction_125 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	happyIn48
		 (happy_var_2 : happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_126 = happySpecReduce_3  45# happyReduction_126
happyReduction_126 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (happy_var_1 : happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_127 = happySpecReduce_1  45# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn49
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_128 = happySpecReduce_3  46# happyReduction_128
happyReduction_128 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (happy_var_1 : happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_129 = happySpecReduce_1  46# happyReduction_129
happyReduction_129 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 ([happy_var_1]
	)}

happyNewToken action sts stk [] =
	happyDoAction 88# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	( _, L.TWildcardName ""  ) -> cont 1#;
	( _, L.TWildcardName "1" ) -> cont 2#;
	( _, L.TWildcardName "_S" ) -> cont 3#;
	( _, L.TWildcardName "_T" ) -> cont 4#;
	( _, L.TWildcardName "_o" ) -> cont 5#;
	( _, L.TWildcardName "_O" ) -> cont 6#;
	( _, L.TWildcardName happy_dollar_dollar ) -> cont 7#;
	( _, L.TLitInt happy_dollar_dollar ) -> cont 8#;
	( _, L.TDafny happy_dollar_dollar  ) -> cont 9#;
	( _, L.TMethod    ) -> cont 10#;
	( _, L.TFunction  ) -> cont 11#;
	( _, L.TEnsures   ) -> cont 12#;
	( _, L.TRequires  ) -> cont 13#;
	( _, L.TSeparates ) -> cont 14#;
	( _, L.TInv       ) -> cont 15#;
	( _, L.TWith      ) -> cont 16#;
	( _, L.TAt        ) -> cont 17#;
	( _, L.TSplit     ) -> cont 18#;
	( _, L.TFor       ) -> cont 19#;
	( _, L.TReturns   ) -> cont 20#;
	( _, L.TNot       ) -> cont 21#;
	( _, L.TNat       ) -> cont 22#;
	( _, L.TReal      ) -> cont 23#;
	( _, L.TInt       ) -> cont 24#;
	( _, L.TIn        ) -> cont 25#;
	( _, L.TBool      ) -> cont 26#;
	( _, L.TSeq       ) -> cont 27#;
	( _, L.TNor       ) -> cont 28#;
	( _, L.THad       ) -> cont 29#;
	( _, L.THApp      ) -> cont 30#;
	( _, L.TQFT       ) -> cont 31#;
	( _, L.TRQFT      ) -> cont 32#;
	( _, L.TRepr      ) -> cont 33#;
	( _, L.TMeasure   ) -> cont 34#;
	( _, L.TMeasured  ) -> cont 35#;
	( _, L.TEn        ) -> cont 36#;
	( _, L.TAA        ) -> cont 37#;
	( _, L.TQReg      ) -> cont 38#;
	( _, L.TEn01      ) -> cont 39#;
	( _, L.TVar       ) -> cont 40#;
	( _, L.TIf        ) -> cont 41#;
	( _, L.TSqrt     ) -> cont 42#;
	( _, L.TSin       ) -> cont 43#;
	( _, L.TCos       ) -> cont 44#;
	( _, L.TCl            ) -> cont 45#;
	( _, L.TUnicodeSum    ) -> cont 46#;
	( _, L.TUnicodeOPlus ) -> cont 47#;
	( _, L.TUnicodeTensor ) -> cont 48#;
	( _, L.TUnicodeOmega  ) -> cont 49#;
	( _, L.TUnicodeIn     ) -> cont 50#;
	( _, L.TKet     ) -> cont 51#;
	( _, L.TUnicodeMap    ) -> cont 52#;
	( _, L.TAssert    ) -> cont 53#;
	( _, L.TForall    ) -> cont 54#;
	( _, L.TOr        ) -> cont 55#;
	( _, L.TAnd       ) -> cont 56#;
	( _, L.TAdd       ) -> cont 57#;
	( _, L.TDiv       ) -> cont 58#;
	( _, L.TSub       ) -> cont 59#;
	( _, L.TMul       ) -> cont 60#;
	( _, L.TPow       ) -> cont 61#;
	( _, L.TMod       ) -> cont 62#;
	( _, L.TBar       ) -> cont 63#;
	( _, L.TLPar      ) -> cont 64#;
	( _, L.TRPar      ) -> cont 65#;
	( _, L.TLAng      ) -> cont 66#;
	( _, L.TRAng      ) -> cont 67#;
	( _, L.TLBracket  ) -> cont 68#;
	( _, L.TRBracket  ) -> cont 69#;
	( _, L.TLBrace    ) -> cont 70#;
	( _, L.TRBrace    ) -> cont 71#;
	( _, L.TId happy_dollar_dollar     ) -> cont 72#;
	( _, L.TComma     ) -> cont 73#;
	( _, L.TDColon     ) -> cont 74#;
	( _, L.TColon     ) -> cont 75#;
	( _, L.TDot       ) -> cont 76#;
	( _, L.TSemi      ) -> cont 77#;
	( _, L.TEq        ) -> cont 78#;
	( _, L.TTyArrow   ) -> cont 79#;
	( _, L.TArrow     ) -> cont 80#;
	( _, L.TImply     ) -> cont 81#;
	( _, L.TGe        ) -> cont 82#;
	( _, L.TLe        ) -> cont 83#;
	( _, L.TAssign    ) -> cont 84#;
	( _, L.TApply     ) -> cont 85#;
	( _, L.TTilde     ) -> cont 86#;
	( _, L.TStore     ) -> cont 87#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 88# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Parser a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(L.SToken)], [Prelude.String]) -> Parser a
happyError' = parseError
runParser tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {x' = happyOut4 x} in x'))

happySeq = happyDontSeq


--- converts a string to tokens and then parses it into our AST
scanAndParse :: String -> Parser AST
scanAndParse = runParser <=< L.runScanner
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
