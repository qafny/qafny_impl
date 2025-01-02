{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RankNTypes
  , TypeOperators
  , UndecidableInstances
  #-}
module Carrier.Gensym.Emit where

-- | A carrier for 'Gensym' effect, generating a unique variable at the
-- emission-level with a (generic) prefix and return all generated variables.

import           Control.Algebra
import           Control.Carrier.State.Strict
import           Data.Functor                 ((<&>))
import           Effect.Gensym
import           Qafny.Variable

newtype GensymC s m a = GensymC { runGensymC :: StateC (Int, [(s, String)]) m a }
  deriving (Applicative, Functor, Monad)

instance (Variable s, Algebra sig m) => Algebra (Gensym s :+: sig) (GensymC s m) where
  alg hdl sig ctx = GensymC $ case sig of
    L (Gensym s) -> state $ \(i, w) ->
      let v = variable (s, i) ++ "__emit"
      in ((i + 1 :: Int, (s, v) : w), v <$ ctx)
    R other      -> alg (runGensymC . hdl) (R other) ctx

runGensymEmit :: Functor m => GensymC s m a -> m (Int, [(s, String)], a)
runGensymEmit c = do
  let ans = runState (0, []) $ runGensymC c
  ans <&> \((i, w), r) -> (i, w, r)


-- | Execute a 'Gensym' effect with the counter initialized to 'i'.
startGensymEmitWith
  :: Functor m
  => Int
  -> GensymC s m a
  -> m (Int, [(s, String)], a)
startGensymEmitWith i c = do
  let ans = runState (i, []) $ runGensymC c
  ans <&> \((i, w), r) -> (i, w, r)


evalGensymEmitWith
  :: forall s a m . Monad m
  => Int
  -> GensymC s m a
  -> m a
evalGensymEmitWith i c = do
  evalState (i, []) $ runGensymC c


