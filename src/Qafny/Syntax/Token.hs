{-# LANGUAGE
    DeriveDataTypeable
  , StrictData
  #-}
module Qafny.Syntax.Token where
import           Data.Data
import           Text.Printf
    (printf)

data Token
  -- Dafny Fragments
  = TDafny String

  -- Constants
  | TLitInt Int

  -- Assertions
  | TRequires | TEnsures | TAssert
  | TSeparates | TInv

  -- Keywords
  | TMethod | TFunction | TReturns
  | TAssign | TApply
  | TVar | TIf | TCl | TKet | TFor
  | TWith
  | TSplit | TAt | TStore
  | TMeasure | TRepr
  | TUnicodeMap
  | TUnicodeOPlus
  | TUnicodeTensor
  | TUnicodeOmega
  | TUnicodeSumOmega

  -- Delimiters
  | TLPar | TRPar | TTilde
  | TLAng | TRAng
  | TLBrace | TRBrace
  | TLBracket | TRBracket
  | TBar | TComma | TDColon | TColon | TSemi
  | TDots | TDot

  -- Quantifiers
  | TForall
  | TIn
  | TUnicodeIn
  | TUnicodeSum

  -- Types
  | TArrow | TImply | TTyArrow | TMeasured
  | TNat | TReal | TInt | TBool
  | TSeq | TNor  | THad | TEn | TAA | TEn01
  | TQReg

  -- Identifiers
  | TId           String
  | TWildcardName String

  -- Comparison
  | TEq | TLe | TGe

  -- Logical
  | TAnd | TOr | TNot

  -- Arithmetics
  | TMul | TAdd | TDiv | TMod | TSub | TPow

  -- Amplitudes
  | TSqrt | TSin | TCos

  -- Gates
  | THApp | TQFT | TRQFT

  -- EOF
  | TEOF
   deriving (Show, Eq)


data SrcLoc = SrcLoc
  { sLine   :: !Int
  , sColumn :: !Int
  }
  deriving (Typeable, Data)

instance Show SrcLoc where
  show s = printf "(%d:%d)" (sLine s) (sColumn s)

type SToken = (SrcLoc, Token)

