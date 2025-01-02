{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}

module Qafny.Interval where

import           Control.Applicative (Applicative (liftA2))
import           Control.Monad       (forM)
import           Data.Bool           (bool)
import           Qafny.Partial       (evalPStatic, hasResidue)
import           Qafny.Syntax.AST    (AEnv, Exp (..), Exp', Range (..), Var)
import           Text.Printf         (printf)

--------------------------------------------------------------------------------
-- * Partial Ordering
--------------------------------------------------------------------------------
class PartialOrd a where
  (⊑) :: a -> a -> Maybe Bool

-- | Anti-symmetry
(≡) :: PartialOrd a => a -> a -> Maybe Bool
a ≡ b = (&&) <$> a ⊑ b <*> b ⊑ a

--------------------------------------------------------------------------------
-- * Lattice
--------------------------------------------------------------------------------
class PartialOrd a => SemiLattice a where
  (⊔) :: a -> a -> Maybe a
  (⊓) :: a -> a -> Maybe a

simpleLub :: PartialOrd a => a -> a -> Maybe a
simpleLub a b = bool a b <$> a ⊑ b

simpleGlb :: PartialOrd a => a -> a -> Maybe a
simpleGlb a b = bool b a <$> a ⊑ b

class SemiLattice a => Lattice a where
  isTop :: a -> Maybe Bool
  isBot :: a -> Maybe Bool


lub1 :: SemiLattice a => [a] -> Maybe a
lub1 (a : as) = foldr (\x rst -> (x ⊔) =<< rst) (pure a) as
lub1 _        = Nothing

glb1 :: SemiLattice a => [a] -> Maybe a
glb1 (a : as) = foldr (\x rst -> (x ⊓) =<< rst) (pure a) as
glb1 _        = Nothing


--------------------------------------------------------------------------------
-- * Interval
--------------------------------------------------------------------------------
data Interval a = Interval a a

instance Show a => Show (Interval a) where
  show (Interval l r) = printf "[%s, %s]" (show l) (show r)

instance PartialOrd a => PartialOrd (Interval a) where
  (Interval a1 b1) ⊑ (Interval a2 b2) = (&&) <$> a2 ⊑ a1 <*> b1 ⊑ b2

instance SemiLattice a => SemiLattice (Interval a) where
  i1@(Interval a1 b1) ⊔ i2@(Interval a2 b2) =
    liftA2 Interval (a1 ⊓ a2) (b1 ⊔ b2)

  i1@(Interval a1 b1) ⊓ i2@(Interval a2 b2) =
    liftA2 Interval (a1 ⊔ a2) (b1 ⊓ b2)

--------------------------------------------------------------------------------
-- * Partial Evaluation Related
--------------------------------------------------------------------------------
instance PartialOrd (Exp ()) where
  e1 ⊑ e2 = (>= 0) <$> evalPStatic (e2 - e1)

instance SemiLattice (Exp ()) where
  (⊔) = simpleLub
  (⊓) = simpleGlb

instance PartialOrd Range where
  (⊑) = fRangeInterval (const (⊑))

instance SemiLattice Range where
  (⊓) = fRangeInterval (\x i1 i2 -> interval2Range x <$> (i1 ⊔ i2))
  (⊔) = fRangeInterval (\x i1 i2 -> interval2Range x <$> (i1 ⊓ i2))

fRangeInterval
  :: (Var -> Interval Exp' -> Interval Exp' -> Maybe c)
  -> Range
  -> Range
  -> Maybe c
fRangeInterval f (Range x xl xr) (Range y yl yr) =
  bool Nothing yes (x == y)
  where
    yes = f x (Interval xl xr)  (Interval yl yr)

interval2Range :: Var -> Interval Exp' -> Range
interval2Range x (Interval i1 i2) = Range x i1 i2

instance Lattice Range where
  isTop r@(Range _ _ e) = if hasResidue e then Nothing else Just False
  isBot r@(Range _ el er) = er ⊑ el
