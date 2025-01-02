{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators,
             UndecidableInstances #-}

module Carrier.Gensym.Meta where

-- | A carrier for 'Gensym' effect, generating a unique variable on the
-- meta-level with provided prefix

import           Control.Algebra
import           Control.Carrier.State.Strict
import           Effect.Gensym
import           Qafny.Variable

newtype GensymC s m a = GensymC { runGensymC :: StateC Int m a }
  deriving (Applicative, Functor, Monad)

instance (Variable s, Algebra sig m) => Algebra (Gensym s :+: sig) (GensymC s m) where
  alg hdl sig ctx = GensymC $ case sig of
    L (Gensym s) -> state $ \i -> (i + 1 :: Int, variable (s, i) <$ ctx)
    R other      -> alg (runGensymC . hdl) (R other) ctx

runGensymMeta :: GensymC s m a -> m (Int, a)
runGensymMeta c = runState 0 $ runGensymC c
