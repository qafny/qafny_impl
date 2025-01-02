{-# LANGUAGE GADTs, KindSignatures #-}

module Effect.Gensym where

import           Control.Algebra
import           Data.Kind       (Type)

data Gensym s (m :: Type -> Type) k where
  Gensym :: s -> Gensym s m String

gensym :: Has (Gensym s) sig m => s -> m String
gensym = send . Gensym
{-# INLINE gensym #-}
