{-# LANGUAGE
    FlexibleInstances
  , PatternSynonyms
  , TypeOperators
  #-}

module Qafny.Syntax.EmitBinding where


import           Control.Applicative
    (Alternative (..))
import           Data.Maybe
    (catMaybes)
import           Data.Sum
import           Qafny.Syntax.AST


-- * EmitBinding related functions

-- | 'EmitData' stores emit variables (a.k.a. data variables) that's supposed to
-- be mapped from either a 'Loc' or a 'Range'
--
data EmitData = EmitData
  { evPhaseRef :: Maybe (PhaseRef, Ty)   -- ^ the ref & type of the phase
  , evBasis    :: Maybe (Var, Ty)        -- ^ the var & type of its kets
  , evAmp      :: Maybe (Var, Ty)        -- ^ the var & type of its amplitude
  }
  deriving (Eq, Ord, Show)

mtEmitData :: EmitData
mtEmitData = EmitData { evPhaseRef   = Nothing
                      , evBasis      = Nothing
                      , evAmp        = Nothing
                      }


-- Extract a list of emitable expressions from an emit data
extractEmitables :: EmitData -> [(Var, Ty)]
extractEmitables EmitData{ evPhaseRef, evBasis, evAmp } =
  maybe []  (uncurry extractRef) evPhaseRef
  ++
  catMaybes  [ evBasis, evAmp ]
  where
    extractRef PhaseRef{prBase, prRepr} ty =
      [(prBase, TNat), (prRepr, ty)]


-- Merge two EmitData pairwise and prefer the 'Just'-fields or the latter one if
-- both are fields 'Just'
instance Semigroup EmitData where
  ed1 <> ed2 = EmitData
    { evPhaseRef   = evPhaseRef ed2   <|> evPhaseRef ed1
    , evBasis      = evBasis ed2      <|> evBasis ed1
    , evAmp        = evAmp ed2        <|> evAmp ed1
    }


-- | EmitBinding : "the query result"
newtype EmitBinding
  = EmitBinding { unEB :: (Range :+: Loc, EmitData) }
  deriving (Eq, Ord)

instance Show EmitBinding where
  show (EmitBinding t) = show t


-- | Emitter : the thing used to perform Gensym
data Emitter
  = EmBaseSeq Range Ty               -- ^ Base  seq per range
  | EmPhaseSeq Loc Int               -- ^ Phase Seq per range/loc with degree
  | EmPhaseBase Loc                  -- ^ Phase Base per range/loc with degree
    -- TODO: I may need to add a Phase Index here
  | EmAmplitude Loc QTy              -- ^ Amplitude?
  | EmAnyBinding Var Ty              -- ^ Anything like a binding
  deriving (Show)

-- pattern PhaseEm :: Var -> Var -> Ty -> EmitData
-- pattern PhaseEm{pvRepr, pvBase, pvPhaseTy} =
--   EmitData { evPhaseRef = Just (PhaseRef{prBase=pvRepr, prRepr=pvBase}, pvPhaseTy)
--            , evBasis = Nothing
--            , evAmp = Nothing
--            }

-- pattern RangeEm :: Range -> Var -> Ty -> (Range, EmitData)
-- pattern RangeEm{prRange, pvKet, pvKetTy} =
--   ( prRange
--   , EmitData { evPhaseRef = Nothing
--              , evBasis = Just (pvKet, pvKetTy)
--              , evAmp = Nothing
--              })
