module Qafny.Syntax.Subst where

import           Data.Bifunctor
import           Data.Functor.Foldable
    (Base, Corecursive (embed), Recursive (cata, project))
import qualified Data.Map.Strict          as Map
import           Data.Maybe
    (fromMaybe)
import           Data.Sum

import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Syntax.AST
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR


-- | Perform expression subtitution

class Substitutable a where
  subst :: AEnv -> a -> a
  fVars :: a -> [Var]

instance Substitutable (Exp ()) where
  subst = substE
  fVars = cata go
    where
      go :: ExpF [Var] -> [Var]
      go (EVarF x) = [x]
      go fvs       = concat fvs

instance Substitutable Partition where
  subst = substP
  fVars = concatMap fVars . unpackPart

instance Substitutable Range where
  subst = substR
  fVars (Range _ e1 e2) = fVars e1 ++ fVars e2

instance Substitutable f => Substitutable (SpecExpF f) where
  subst = substF
  fVars = fVarsF

instance (Substitutable a) => Substitutable [a] where
  subst = substF
  fVars = fVarsF

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  subst a = bimap (subst a) (subst a)
  fVars = uncurry (++) . bimap fVars fVars

instance Substitutable a => Substitutable (a :+: b) where
  subst a (Inl r) = inj $ subst a r
  subst _ b       = b
  fVars (Inl r) = fVars r
  fVars _       = []


-- lift `subst` to Functor
substF :: (Substitutable a, Functor f) => AEnv -> f a -> f a
substF a = (subst a <$>)

fVarsF :: (Substitutable a, Foldable t) => t a -> [Var]
fVarsF = concatMap fVars


substMapKeys :: (Ord k, Substitutable k) => AEnv -> Map.Map k v -> Map.Map k v
substMapKeys a = Map.mapKeys (subst a)

fVarMapKeys :: Substitutable k => Map.Map k v -> [Var]
fVarMapKeys = fVars . Map.keys

substE :: AEnv -> Exp () -> Exp ()
substE [] = id
substE env = go
  where
    go :: Exp () -> Exp ()
    go (EVar x)      = EVar x `fromMaybe` lookup x env
    go (ESpec p q e) = ESpec (substP env p) q e
    go (ERange r)    = ERange (substR env r)
    go e             = embed $ go <$> project e

substP :: AEnv -> Partition -> Partition
substP [] = id
substP env =
  Partition . (substR env <$> ) . unpackPart


substR :: AEnv -> Range -> Range
substR [] r = r
substR env (Range x l r) =
  Range x (go l) (go r)
  where
    go = substE env

instance Substitutable EmitBinding where
  subst a (EmitBinding (Inl r, t)) = EmitBinding (inj (subst a r), t)
  subst a b                        = b

  fVars (EmitBinding (Inl r, _)) = fVars r
  fVars _                        = []

instance Substitutable (Map.Map (Range :+: Loc) EmitData) where
  subst = substMapKeys
  fVars = fVarMapKeys

instance (Substitutable a, Normalizable a) => Substitutable (Normalized a) where
  subst aenv = normalize . subst aenv . denorm
  fVars = fVars . denorm

instance Substitutable TState where
  subst a (TState{ _sSt = s, _xSt = x, _emitSt = es }) =
    TState { _sSt = first (subst a) <$> s
           , _xSt = (first (subst a) <$>) <$> x
           , _emitSt = Map.mapKeys (subst a) es
           }
  fVars (TState{ _sSt = s, _xSt = x, _emitSt = es }) =
    concatMap (fVars . fst) s
    ++ fVarMapKeys es
    ++ concatMap (concatMap $ fVars . fst) (Map.elems x)
