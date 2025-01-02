module Qafny.Typing.Range where
import Qafny.Syntax.AST (Range)

-- Utility functions for range related behaviors

areRangesEquiv :: [(Range, Range)] -> Bool
areRangesEquiv = all (uncurry (==))
