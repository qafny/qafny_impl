{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , TupleSections
  , TypeFamilies
  , TypeOperators
  #-}

module Qafny.Analysis.Partial where
import           Data.Bool
    (bool)
import           Data.Functor.Foldable
    (Corecursive (embed), Recursive (project))
import qualified Data.Map.Strict       as Map
import           Data.Maybe
    (isJust)
import           Data.Sum              as Sum

import           Data.Bifunctor
    (Bifunctor (first))
import           Qafny.Syntax.AST
import           Qafny.Syntax.IR

--------------------------------------------------------------------------------
-- $doc
-- 'Qafny.Analysis.Partial' module implements the partial evaluation strategy of
-- expressions, (linear) arithmetic expressions and ranges in particular.
--
-- The goal is to reduce an expression with uninterpreted variables /as much as/
-- possible.
--
-- $doc
--------------------------------------------------------------------------------

-- | Result of performing partial evaluation
type PMap = Map.Map (Var Sum.:+: Exp') Int

type PResult = (PMap, Int)

class PEval a where
  evalP :: a -> PResult
  reflectP :: PResult -> a

instance PEval Exp' where
  evalP (ENum i) = (Map.empty, i)
  evalP (EVar v) = (Map.singleton (inj v) 1, 0)
  evalP (EOp1 ONeg e1) =
    let (m1, v1) = evalP e1 in (Map.map negate m1, negate v1)

  evalP (EOp2 op e1 e2) =
    let (m1, v1) = evalP e1
        (m2, v2) = evalP e2
    in case op of
      OAdd -> (evalResidue (+) m1 m2, v1 + v2)
      OSub -> (evalResidue (+) m1 (Map.map negate m2), v1 - v2)
      _    -> undefined
  evalP e = (Map.singleton (inj e) 1, 0)

  reflectP (m, i) =
    let m' = Map.filter (/= 0) m
    in case (Map.toList m', i) of
      ([], _)    -> ENum i
      (h : t, 0) -> foldr go' id t $ opChoice1 (snd h) (uncurry addExps h)
      (l, i)     -> foldr go' id l $ ENum i
    where
      opChoice :: Int -> Exp' -> Exp' -> Exp'
      opChoice cnt e eKont = EOp2 (if cnt >=0 then OAdd else OSub) eKont e

      opChoice1 :: Int -> Exp' -> Exp'
      opChoice1 cnt = if cnt >= 0 then id else EOp1 ONeg

      -- create an expression with a hole to be plugged in to for a larger expression
      go v cnt = opChoice cnt (addExps v cnt)

      go' pr = (uncurry go pr .)

      -- I really mean this instead of 'sum' because 'sum' will insert an
      -- undesired leading '0' term.
      addExps :: (Var Sum.:+: Exp') -> Int -> Exp'
      addExps v cnt = foldr1 (+) (replicate (abs cnt) (var' v))

      var' (Sum.Inl v) = EVar v
      var' (Sum.Inr e) = e

class Reducible a where
  reduce :: a -> a

(==&) :: (Eq a, Reducible a) => a -> a -> Bool
a ==& b = reduce a == reduce b


instance Reducible a => Reducible [a] where
  reduce = fmap reduce

instance Reducible Exp' where
  reduce = go
    where
      go :: Exp' -> Exp'
      go e@ENum{}          = red e
      go e@EVar{}          = red e
      go e@(EOp1 ONeg _)   = red e
      go e@(EOp2 OAdd _ _) = red e
      go e@(EOp2 OSub _ _) = red e
      go e                 = embed $ go <$> project e
      red = reflectP . evalP

instance Reducible Range where
  reduce (Range x l r) = Range x (reduce l) (reduce r)

instance Reducible Partition where
  reduce = Partition . reduce . unpackPart

instance Reducible a => Reducible (a :+: b) where
  reduce (Inl r) = inj $ reduce r
  reduce b       = b

-- | Union two residual maps with the given operator and remove zero-coefficient
-- variables
evalResidue
  :: (Int -> Int -> Int)
  -> PMap
  -> PMap
  -> PMap
evalResidue f s1 s2 = Map.filter (/= 0) $ Map.unionWith f s1 s2

staticValue :: PResult -> Maybe Int
staticValue (s, i) = bool Nothing (Just i) $ Map.null s

evalPStatic :: PEval a => a -> Maybe Int
evalPStatic = staticValue . evalP

hasResidue :: PEval a => a -> Bool
hasResidue = isJust . staticValue . evalP


--------------------------------------------------------------------------------
-- * Misc Evaluation
--------------------------------------------------------------------------------
-- newtype PExp = PExp { unPExp :: Exp }

sizeOfRangeP :: Range -> Maybe Int
sizeOfRangeP (Range _ el er) = evalPStatic (er - el)
