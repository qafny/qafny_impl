{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , LambdaCase
  , PatternSynonyms
  , StrictData
  , TemplateHaskell
  , TypeApplications
  , TypeOperators
  , ViewPatterns
  #-}
module Qafny.Syntax.IR where

import           Control.Lens
import           Data.List
    (intercalate)
import           Data.List.NonEmpty
    (NonEmpty)
import qualified Data.Map.Strict          as Map
import           Data.Sum
import           Qafny.Syntax.AST
import           Qafny.Syntax.EmitBinding
import           Qafny.TTG

-- * Normalization
newtype Normalized a = Normalized { denorm :: a }
  deriving (Ord, Eq)

instance Show a  => Show (Normalized a) where
  show = show . denorm

instance Injection (Normalized f) (Normalized (f :+: g)) where
  inj = Normalized . inj . denorm

instance Injection Loc (Normalized (f :+: Loc)) where
  inj = Normalized . inj

viewNormalizedRanges
  :: Functor t => Normalized (PartitionT t) -> t (Normalized Range)
viewNormalizedRanges = (Normalized <$>) . ranges . denorm


npart
  :: Functor t => t (Normalized Range) -> Normalized (PartitionT t)
npart = Normalized . Partition . (denorm <$>)

pattern NPartition
  :: (Functor t => t (Normalized Range) -> Normalized (PartitionT t))
pattern NPartition{nranges} <- (viewNormalizedRanges -> nranges) 
{-# COMPLETE NPartition #-}

-- ** LoucsEmitData
-- This is also normalized.

type LocusEmitData = LocusTEmitData []
newtype LocusEmitData' =  LocusEmitData' (LocusTEmitData [])
type LocusTEmitData t = (EmitData, t (Normalized Range, EmitData))

--------------------------------------------------------------------------------
-- High-Order Types
--------------------------------------------------------------------------------
-- TODO: Migrate to Locus representation
data LocusT t =
  Locus { loc     :: Loc                        -- ^ identifier for the locus
        , part    :: Normalized (PartitionT t)  -- ^ partition
        , qty     :: QTy                        -- ^ entanglement type
        , degrees :: [Int]                      -- ^ degrees of phase info
        }

type Locus = LocusT []
deriving instance Show Locus
deriving instance Eq Locus

--------------------------------------------------------------------------------
-- Methods
--------------------------------------------------------------------------------
data MethodElem
  = MTyPure Var Ty
  | MTyQuantum Var Exp'
  deriving (Show, Eq, Ord)

data MethodType = MethodType
  -- Parameters for the source method (Type resolution level)
  { mtSrcParams   :: [MethodElem]
  , mtSrcReturns  :: [MethodElem]
  , mtInstantiate :: Map.Map Var Range -> [(Partition, QTy, Maybe Int)]
  , mtReceiver    :: Map.Map Var Range -> [(Partition, QTy, Maybe Int)]
  -- , mtDebugInit :: [(Partition, QTy)]
  }

instance Show MethodType where
  show MethodType {mtSrcParams=ts, mtSrcReturns=ts'} =
    show ts ++ "\n" ++ show ts'


newtype MTy = MTy { unMTy :: Ty :+: MethodType }

instance Show MTy where
  show (MTy (Inl t)) = show t
  show (MTy (Inr m)) = show (mtSrcParams m) ++ show (mtSrcReturns m)

projTy :: MTy -> Maybe Ty
projTy = projLeft . unMTy

projMethodTy :: MTy -> Maybe MethodType
projMethodTy = projRight . unMTy

instance Injection Ty MTy where
  inj = MTy . inj

instance Injection MethodType MTy where
  inj = MTy . inj

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------
data CtxMode
  = CtxC
  | CtxQ
  deriving Show

type KEnv = Map.Map Var MTy

data TEnv = TEnv
  { _kEnv :: KEnv
  , _ctx  :: CtxMode
  , _qnum :: Exp' -- assume each Q type variable is associated with a qubit num which is C type exp
  }

type RangeOrLoc = Range :+: Loc
type EmitState = Map.Map (Normalized RangeOrLoc) EmitData

data TState = TState
  { _sSt    :: Map.Map Loc (Normalized Partition, (QTy, [Int]))
    -- ^ partition type state
  , _xSt    :: Map.Map Var [(Normalized Range, Loc)]
    -- ^ range reference state
  , _emitSt :: EmitState
  }
$(makeLenses ''TState)
$(makeLenses ''TEnv)

instance Show TState where
  show st = "\n  Partition Reference State:\n    " ++
            (intercalate "\n    " . map show . Map.toList) (st ^. xSt) ++
            "\n  Partition State:\n    " ++
            (intercalate "\n    " .
             map show . ((\(x, (y,z)) -> (x, y, z)) <$>) . Map.toList)
            (st ^. sSt) ++
            "\n  Renaming State:\n    " ++
            (intercalate "\n    " . map show . Map.toList) (st ^. emitSt)

instance Show TEnv where
  show st = "  Kind Environment" ++
            (intercalate "\n    " . map show . Map.toList) (st ^. kEnv)

initTEnv :: TEnv
initTEnv = TEnv { _kEnv = mempty, _ctx = CtxQ, _qnum = ENum 0 }

initTState :: TState
initTState = TState
  { _sSt = mempty
  , _xSt = mempty
  , _emitSt = mempty
  }

data SplitScheme = SplitScheme
  { schEdAffected   :: (Normalized Range, EmitData, EmitData)
    -- ^ Both locus and range `EmitData` for the affected range
  , schEdSplit      :: (Normalized Range, EmitData)
    -- ^ The range `EmitData` for the split range. The affected one shares the
    -- same locus as the split one.
  , schEdRemainders :: NonEmpty (Normalized Range, EmitData, EmitData)
    -- ^ Both locus and range `EmitData` for each (singleton) remainders
  }
  deriving Show

data CastScheme = CastScheme
  { schEdsFrom :: LocusEmitData
  , schEdsTo   :: LocusEmitData
  , schQtFrom  :: QTy
  , schQtTo    :: QTy
  -- , schRsCast  :: [Range] -- | casted ranges
  }
  deriving Show

data MergeScheme
  = MJoin  JoinStrategy  -- ^ Join a 'Range' into an existing 'Range'
  | MMove
  | MEqual EqualStrategy -- ^ Join two copies of data of the same range
  deriving Show

newtype EqualStrategy = EqualStrategy
  { esEdIntoFrom :: [(LocusEmitData, LocusEmitData)]
  }
  deriving Show

data JoinStrategy = JoinStrategy
  { jsQtMain   :: QTy
  , jsQtMerged :: QTy
  , jsLedMain   :: (EmitData, (Normalized Range, EmitData))
  , jsLedMerged :: (EmitData, (Normalized Range, EmitData))
  , jsLedInto   :: (EmitData, (Normalized Range, EmitData))
  }
  deriving Show

--------------------------------------------------------------------------------
-- * Spec Relations
data SRelT t
  = RNor  (T t SpecNor)
  | RHad  (T t SpecHad)
  | REn   (T t SpecEn)
  | REn01 (T t SpecEn01)
  | RWild

type SRel1 = SRelT Identity
type SRel  = SRelT []

deriving instance (Show SRel1)
deriving instance (Show SRel)

