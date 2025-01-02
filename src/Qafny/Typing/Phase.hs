{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , MultiWayIf
  , NamedFieldPuns
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , TypeFamilies
  #-}


module Qafny.Typing.Phase where

-- | Phase-related Typing

-- Effects
import           Qafny.Effect

-- Qafny
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Typing.Utils
import           Qafny.Utils
import           Qafny.Typing.Locus
    (updateMetaStByLocus)

-- Utils
import           Control.Monad
    (liftM2, when)
import           Data.List
    (nub, singleton)
import           Data.Maybe
    (catMaybes, mapMaybe)
import           Data.Sum
    (Injection (inj))
import           Text.Printf
    (printf)

throwError'
  :: ( Has (Error Builder) sig m )
  => Builder -> m a
throwError' = throwError @Builder . ("[Phase Typing]" <+>)

-------------------------------------------------------------------------------
-- | Promotion Scheme
--------------------------------------------------------------------------------
data PromotionScheme = PromotionScheme
  { psPrefs     :: [PhaseRef]
  , psDgrPrev   :: Int
  , psDgrCurr   :: Int
  , psPromotion :: Promotion
  }

data Promotion
  = Promote'0'1 (Exp', Exp') [Normalized Range] QTy

-- | Promote phases to another level
-- - The degree of the binder should agree with the degree of the tuple
-- - The degree of the spec can be different from the one of the binder: this is
--   where the promotion kicks in
--
promotionScheme
  :: ( GensymEmitterWithStateError sig m
     , Has Trace sig m
     )
  => Locus -> PhaseBinder -> PhaseExp -> m (Maybe PromotionScheme)
promotionScheme st@Locus{loc, part, qty, degrees=dgrsSt} pb pe =
  if (dgrBind < 0 && dgrSpec < 0)
  then return Nothing
  else do
    -- FIXME: for now, the simple dirty way is to restrict `dgrsSt` to have only
    -- one consistent degree. So the promotionScheme can act on the entire
    -- partition, which I think is a good idea for now.  Unless one specified a
    -- weird phase in the precondition, there's no statement available for you to
    -- construct a partition with multiple different degrees.
    -- intro degrees
    dgrSt <- onlyOne (throwError' . ("promote"  <+>)) $ nub dgrsSt
    -- check if binder's and specexp's degrees match
    when (dgrSt /= dgrBind) $ throwError' (errDgrMismatch dgrSt dgrBind)
  
    -- check what promotion can be done here
    case (dgrBind, dgrSpec) of
      (dBind, dSpec) | dBind == dSpec -> return Nothing
      (0, 1)                          -> promote'0'1 pe
      (1, 2)                          -> promote'1'2
      (dBind, dSpec) | dBind > dSpec  -> errDemotion dBind dSpec
      (dBind, dSpec)                  -> errUnimplementedPrompt dBind dSpec
  where
    -- degree 
    dgrBind = analyzePhaseSpecDegree pb
    dgrSpec = analyzePhaseSpecDegree pe

    rs = nranges part

    -- Promote 0 to 1
    promote'0'1
      :: ( Has (Gensym Emitter) sig m'
         , Has (State TState) sig m'
         , Has (Error Builder) sig m'
         , Has Trace sig m'
         )
      => PhaseExp -> m' (Maybe PromotionScheme)
    promote'0'1 (PhaseOmega i n) = do
      let fstSt = modifyPty (1 <$) st
      ptys <- allocAndUpdatePhaseType fstSt
      let prefs = (fst <$>) <$> ptys
      dumpSt "After promotion"
      return . Just $ PromotionScheme
        { psPrefs   = catMaybes prefs -- no way there's a Nothing
        , psDgrPrev = 0
        , psDgrCurr = 1
        , psPromotion = Promote'0'1 (i, n) rs qty
        }
    promote'0'1 _ = internalError


    -- Promote 1 to 2
    errDgrMismatch dSt dbinder = vsep
      [ "Degree " <!> dSt <!> " doesn't match the degree of the binder."
      , incr4 dbinder
      ]

    promote'1'2 = undefined
    errDemotion i j = throwError' $
      "Demote " <!> i <!> " to " <!> j <!> " is not allowed."
    errUnimplementedPrompt i j =  throwError' $
      "Promoting " <!> i <!> " to " <!> j <!> " is undefined."


--------------------------------------------------------------------------------
-- * Phase Typing
--------------------------------------------------------------------------------
-- data SpecExpF f
--   = SESpecNor (SpecNorF f)
--     -- ^ `⊗ id . e`
--   | SESpecHad (SpecHadF f)
--     -- ^ `⊗ id . ω`
--   | SESpecEn (SpecEnF f)
--     -- ^ `Σ id ∈ intv . ω ~ e`
--   | SESpecEn01 (SpecEn01F f)
--     -- ^ `Σ id1 ∈ intv1 . ⊗ id2 . ω ~ e`
--   | SEWildcard
--     -- ^ `_`
--   deriving (Functor, Foldable, Traversable)

-- | Compute phase degree information from a specification
analyzeSpecDegree :: SpecExpF f -> Maybe Int
analyzeSpecDegree (SESpecHad SpecHadF{hadPhase})=
  return (analyzePhaseSpecDegree hadPhase)
analyzeSpecDegree (SESpecEn SpecEnF{enPhaseCoef})=
  return (analyzePhaseSpecDegree enPhaseCoef)
analyzeSpecDegree (SESpecEn01 SpecEn01F{en01PhaseCoef})=
  return (analyzePhaseSpecDegree en01PhaseCoef)
analyzeSpecDegree _ = Nothing

-- | Analyze the degree of a phase expression
analyzePhaseSpecDegree :: PhaseExpF f -> Int
analyzePhaseSpecDegree PhaseWildCard   = -1
analyzePhaseSpecDegree PhaseZ          =  0
analyzePhaseSpecDegree PhaseOmega{}    =  1
analyzePhaseSpecDegree PhaseSumOmega{} =  2


allocAndUpdatePhaseType
  :: ( Has (Gensym Emitter) sig m
     , Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => Locus -> m [Maybe (PhaseRef, Ty)]
allocAndUpdatePhaseType s = do
  updateMetaStByLocus s
  (evPhaseRef <$>) <$> genEmStUpdatePhaseFromLocus s

-- | Query in the emit state the phase types of the given Locus
queryPhase
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => Locus -> m [Maybe (PhaseRef, Ty)]
queryPhase Locus{loc, part, qty, degrees}
  | isEn qty = do
      dgr <- onlyOne (throwError' . ("1" <+>)) degrees
      singleton . evPhaseRef <$> findEm (inj loc)
  | otherwise = do
      haveSameLength "queryPhase" (nranges part) degrees
      sequence [ evPhaseRef <$> findEm (inj r) | r <- nranges part ]

-- | Query in the emit state the phase types of the given Locus
queryPhaseRef
  :: ( Has (State TState) sig m
     , Has (Error Builder) sig m
     )
  => Locus -> m [Maybe (PhaseRef, Ty)]
queryPhaseRef Locus{loc, part=NPartition{nranges}, qty, degrees}
  | isEn qty = do
      dgr <- onlyOne (throwError' . ("2" <+>)) degrees
      singleton . evPhaseRef <$> findEm (inj loc)
  | otherwise = do
      haveSameLength "queryPhaseRef" nranges degrees
      sequence [ evPhaseRef <$> findEm (inj r) | r <- nranges ]

-- * Degree

-- | Produce a "degrees" for an En locus
enDegree :: Int -> [Int]
enDegree = return
