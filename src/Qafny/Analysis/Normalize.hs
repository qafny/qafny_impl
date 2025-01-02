{-# LANGUAGE
    DefaultSignatures
  #-}

module Qafny.Analysis.Normalize where

import           Data.List
    (sort)
import           Data.Sum
import           Qafny.Analysis.Partial
import           Qafny.Syntax.AST
import           Qafny.Syntax.IR

class Normalizable a where
  normalize :: a -> Normalized a


instance Normalizable Range where
  normalize = Normalized . reduce
  {-# NOINLINE[1] normalize #-}

instance Normalizable Partition where
  normalize Partition{ranges} =
    Normalized . Partition $ denorm . normalize <$> ranges
  {-# NOINLINE[1] normalize #-}

instance Normalizable (Range :+: Loc) where
  normalize (Inl r) = Normalized (Inl (reduce r))
  normalize inr     = Normalized inr
  {-# NOINLINE[1] normalize #-}

{-# RULES
  "normalize/denorm" [1]  forall a . normalize (denorm a) = a
  #-}
