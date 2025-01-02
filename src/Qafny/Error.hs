module Qafny.Error where

import qualified Data.Map.Strict   as Map

import           Qafny.Syntax.AST
    (Loc, Partition, Range, Var)
import           Qafny.Syntax.Emit
import           Qafny.Syntax.IR
    (MTy)

data QError = UnknownVariableError Var (Map.Map Var MTy)
            | UnknownPartitionError Partition
            | UnknownRangeError Range
            | UnknownLocError Loc
            | AmbiguousRange Range [(Range, Loc)]

instance DafnyPrinter QError where
  pp = debugOnly' . pp'
    where
      pp' (UnknownVariableError v env) = vsep
        [ "Variable" <+> v <+> "is not in the scope!"
        , pp "Current environment:"
        , indent 4 env
        ]
      pp' (UnknownPartitionError s) =
        "Partition [" <+> s <+> "] is not in the scope!"
      pp' (UnknownRangeError r) =
        "Range [" <+> r <+> "] is not in the scope!"
      pp' (UnknownLocError l) =
        "Loc [" <+> l <+> "] is not in the scope!"
      pp' (AmbiguousRange r rs) =
        "Range " <+> r <+> " is a subrange of multiple ranges: "
        <!> line <!> indent 4 (vsep rs)
