{-# LANGUAGE
    FlexibleInstances
  #-}

module Qafny.Analysis.Domain where
import           Qafny.Analysis.Interval

-- * Nat Domain
data Nat
  = Inf
  | Mt
  | Nat Int

instance Show Nat where
  show Inf     = "∞"
  show Mt      = "⊥"
  show (Nat i) = show i

instance Num Nat where
  a + b =
    case a of
      Mt -> Mt
      Inf   -> Inf
      Nat n -> case b of
        Mt    -> Mt
        Inf   -> Inf
        Nat m -> Nat (n + m)

  a - b =
    case a of
      Mt -> Mt
      Inf   -> Inf
      Nat n -> case b of
        Mt    -> Mt
        Inf   -> Inf
        Nat m -> if n - m >= 0 then Nat (n - m) else Mt

  a * b =
    case a of
      Mt -> Mt
      Inf   -> case b of
        Mt    -> Mt
        Nat 0 -> Mt
        _     -> Inf
      Nat n -> case b of
        Mt    -> Mt
        Inf   -> if n == 0 then Mt else Inf
        Nat m -> Nat $ n * m

  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger a = Nat (fromInteger a)


instance PartialOrd Nat where
  _       ⊑ Inf     = return True
  Inf     ⊑ _       = return False
  Mt      ⊑ _       = return True
  _       ⊑ Mt      = return False
  (Nat n) ⊑ (Nat m) = return $ n <= m

instance SemiLattice Nat where
  (⊔) = simpleLub
  (⊓) = simpleGlb

instance Lattice Nat where
  isTop = (≡ Inf)
  isBot = (≡ Mt)


-- ** Nat Interval
type NatInterval = Interval Nat

instance Lattice (Interval Nat) where
  isTop (Interval (Nat 0) Inf) = return True
  isTop (Interval a b)         = return False
  isBot (Interval a b) = not <$> a ⊑ b
