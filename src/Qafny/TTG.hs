{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , StandaloneDeriving
  , TypeFamilies
  , TypeOperators
  #-}

module Qafny.TTG where
import           Data.Data
import           Qafny.Syntax.Token
    (SrcLoc (..))
import           Qafny.Utils.Common
--------------------------------------------------------------------------------
-- * Indexed Family for Extensible ADTs

data Source
  deriving (Data, Typeable)

type family XRec idx a where
  XRec ()     a = a
  XRec Source a = Located a

data Located f = L SrcLoc f
  deriving (Show, Functor, Typeable, Data)

unLoc :: Located f -> f
unLoc (L _ f) = f

instance Eq f => Eq (Located f) where
  a == b = unLoc a == unLoc b

instance Ord f => Ord (Located f) where
  a `compare` b = unLoc a `compare` unLoc b


type family T (t :: Type -> Type) a where
  T Identity a = a
  T Maybe    a = Maybe a
  T []       a = [a]
