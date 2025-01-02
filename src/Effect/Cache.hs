{-# LANGUAGE
    GADTs
  , KindSignatures
  , RankNTypes
  #-}

module Effect.Cache where

import           Control.Algebra
import           Control.Effect.Error
    (Error, throwError)
import           Data.Kind
    (Type)
import           Debug.Trace
    (traceStack)

-- | `Cache` effect: avoid carrying out extra computation by caching
--
-- Questions: How to make it safe on the type level? i.e. Drawing an uncached
-- value should be a static error?

data Cache v (m :: Type -> Type) k where
  Draw  :: Cache v m (Maybe v)
  Cache :: v -> Cache v m ()

cache :: Has (Cache v) sig m => v -> m ()
cache v = send $ Cache v
{-# INLINE cache #-}

draw :: forall v sig m . Has (Cache v) sig m => m (Maybe v)
draw = send Draw
{-# INLINE draw #-}

drawDefault :: Has (Cache v) sig m => m v -> m v
drawDefault e = do
  v <- draw
  maybe (do v' <- e; cache v'; return v') return v

cacheMiss :: Has (Error String) sig m => m a
cacheMiss = traceStack "" $ throwError "CacheMiss!"

drawErr
  :: forall v sig m .
     (Has (Cache v) sig m, Has (Error String) sig m)
  => m v
drawErr = draw >>= maybe cacheMiss return
