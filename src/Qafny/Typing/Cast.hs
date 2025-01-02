module Qafny.Typing.Cast where

import           Qafny.Syntax.AST
import           Qafny.Syntax.IR

data CastError
  = ErrNoCast
  | ErrInvalidCast

-- | Cast a Locus into another
castLocus :: Locus -> QTy -> Either CastError Locus
castLocus l@Locus{qty} qtyInto =
  case (qty, qtyInto) of
    (TNor, THad)  -> return l{qty=qtyInto, degrees=[1]}
    (TNor, TEn)   -> return l{qty=qtyInto, degrees=[0]}
    (TNor, TEn01) -> return l{qty=qtyInto, degrees=[0]}
    (THad, TEn01) -> return l{qty=qtyInto}
    (THad, TEn)   -> return l{qty=qtyInto}
    _             ->
      Left $ if qty == qtyInto then ErrNoCast else ErrInvalidCast
