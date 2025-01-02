{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TupleSections
  , TypeOperators
  , UndecidableInstances
  #-}

module Carrier.Cache.One where

-- | A carrier for 'Cache' effect, extract the cached value

import           Control.Algebra
import           Control.Carrier.State.Church
    (StateC, evalState, runState)
import           Control.Effect.Error
    (Error, throwError)
import           Control.Effect.State
    (get, put)
import           Data.Functor
    (($>), (<&>))
import           Data.Maybe
    (fromJust)
import           Effect.Cache

newtype CacheC s m a = CacheC { runCacheC :: StateC (Maybe s) m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Cache s :+: sig) (CacheC s m) where
  alg hdl sig ctx = CacheC $ case sig of
    L (Cache s) -> put (Just s) $> ctx
    L Draw      -> get <&> (<$ ctx)
    R other     -> alg (runCacheC . hdl) (R other) ctx

-- this requires `c` and `eval` has the same `m` (using exactily the same
-- effects!) which is too restrictive!
-- withCache :: Monad m => CacheC s m a -> m s -> m (s, a)
-- withCache c eval = do
--   (s, r) <- runState Nothing $ runCacheC c
--   (,r) <$> maybe eval return s

-- -- | execute the computation and returns both cached value and the answer
readCache
  :: forall s a sig m .
     (Has (Error String) sig m)
  => s -> CacheC s m a -> m (s, a)
readCache s' c = do
  (s, r) <- runState (curry return) (Just s') $ runCacheC c
  return (fromJust s, r)

-- | execute the computation but drop the cache
dropCache_
  :: forall s a m .
     Applicative m
  => CacheC s m a -> m a
dropCache_ = evalState Nothing . runCacheC

-- |
dropCache
  :: forall s a m .
     Applicative m
  => s -> CacheC s m a -> m a
dropCache s = evalState (Just s) . runCacheC

