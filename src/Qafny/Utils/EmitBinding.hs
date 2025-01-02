{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , NamedFieldPuns
  , TupleSections
  , TypeApplications
  , TypeOperators
  #-}
module Qafny.Utils.EmitBinding
  ( -- * Gensyms
    gensymBinding
  , genEmStUpdatePhase, genEmStByRange, genEmStByRanges
  , genEmFromLocus, genEmStFromLocus
  , genEmStUpdatePhaseFromLocus
  , genEmStUpdateKets
  , regenEmStByLocus
    -- * Gensyms w/o State
  , genEmByRange
    -- * Query
  , findEm, findEms
  , visitEm, visitEms , visitEmBasis, visitEmsBasis
  , findVisitEm, findVisitEms
  , findEmitBasesByRanges, findEmitBasisByRange
  , findEmsByLocus, findEmInEmitState, findEmsByLocusInEmitState
    -- * Deletion
  , deleteEm, deleteEms, deleteEmPartition
    -- * Update
  , appendEmSt, extendEmSt
    -- * Helper
  , fsts, extractEmitablesFromLocus, extractEmitablesFromEds, eraseRanges
  , eraseMatchedRanges, eraseMatchedRanges'
  , findThenGenLocus, extractMatchedEmitables
    -- * Type
  )
where

import           Control.Applicative (liftA2)
import           Control.Lens
    (at, sans, (?~), (^.))
import           Control.Monad
    (liftM2, zipWithM, zipWithM_, (>=>))
import           Data.Functor
    ((<&>))
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Data.Sum

import           Control.Effect.Lens
import           Data.Foldable
    (Foldable (toList))
import           Data.Maybe
    (catMaybes, listToMaybe)
import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Typing.Utils
    (tyAmp, tyKetByQTy, typingPhaseEmitReprN)
import           Qafny.Utils.Utils
    (errTrace, zipWithExactly, onlyOne)
import GHC.Stack (HasCallStack)

--------------------------------------------------------------------------------
-- * Gensym Utils
--
-- $doc
-- The following functions operate on a 'Range'/'Loc' and a 'QTy', form a
-- `Binding` to be normalized to a variable name, perform modification and query
-- to the emit symbol state w/ the __Gensym Emitter__ effect.
-- $doc
--------------------------------------------------------------------------------

-- ** Basics
gensymBinding :: (Has (Gensym Emitter) sig m) => Var -> Ty -> m (Var, Ty)
gensymBinding v t = (,t) <$> gensym (EmAnyBinding v t)

genKetsByQTy :: Has (Gensym Emitter) sig m => QTy -> [Range] -> m [Maybe (Var, Ty)]
genKetsByQTy qty ranges =
  maybe (return (Nothing <$ ranges)) go tyKets
  where
    go ty = mapM (go' ty) ranges
    go' ty r = Just . (,ty) <$> gensym (EmBaseSeq r ty)
    tyKets = tyKetByQTy qty

genKet :: Has (Gensym Emitter) sig m => QTy -> Range -> m (Maybe (Var, Ty))
genKet qty range =
  mapM go tyKets
  where
    go ty = (,ty) <$> gensym (EmBaseSeq range ty)
    tyKets = tyKetByQTy qty

-- | Generate a phase representation
-- TODO: a lot of redundency from changes, refactor this
genPhase
  :: (GensymEmit sig m, MayFail sig m )
  => QTy -> Maybe Int -> Loc -> m (Maybe (PhaseRef, Ty))
genPhase qt dgr = go qt dgr
  where
    err = throwError $ qt <+> "is incompatible with degree" <+> dgr <+> "."
    go _     (Just n) _ | n < 0 = return Nothing
    go TNor  Nothing  _ = return Nothing
    go TNor  Just{}   _ = err
    go _     Nothing  _ = err
    go _     (Just n) r = do
      pr <- liftM2 PhaseRef (gensym (EmPhaseBase r)) (gensym (EmPhaseSeq r n))
      return $ Just (pr, typingPhaseEmitReprN n)


genAmp :: Has (Gensym Emitter) sig m
       => QTy -> Loc -> m (Maybe (Var, Ty))
genAmp qty l =
  mapM go $ tyAmp qty
  where
    go ty = gensym (EmAmplitude l qty) <&> (, ty)

-- ** Basics but Stateful

-- | Generate a /complete/ 'EmitData' of a Range and manage it within the 'emitSt'
genEmStByRange
  :: GensymEmitterWithState sig m => QTy -> Normalized Range -> m EmitData
genEmStByRange qt r = do
  ed <- genEmByRange qt (denorm r)
  emitSt %= (at (inj r) ?~ ed)
  return ed

genEmByRange :: (Has (Gensym Emitter) sig m) => QTy -> Range -> m EmitData
genEmByRange qt r = do
  b <- genKet qt r
  let ed =  EmitData { evPhaseRef   = Nothing
                     , evBasis      = b
                     , evAmp        = Nothing -- amplitude is not managed by range
                     }
  return ed



-- | Generate EmitData for a list of ranges
genEmByRanges
  :: ( GensymEmit sig m, MayFail sig m, Traversable t)
  => QTy -> t (Normalized Range) -> m (t (Normalized Range, EmitData))
genEmByRanges qt = mapM go
  where
    go r = (r, ) <$> genEmByRange qt (denorm r)

-- | Generate EmitData for a list of ranges and manage it in state.
genEmStByRanges
  :: ( GensymEmitterWithState sig m
     , Traversable t
     )
  => QTy -> t (Normalized Range) -> m (t (Normalized Range, EmitData))
genEmStByRanges qt = mapM go
  where
    go r = (r,) <$> genEmStByRange qt r


-- | Remove the previous EmitData, and generate an `EmitData` from a Locus
-- including both amplitude, range and phase.
regenEmStByLocus
  :: ( GensymEmitterWithStateError sig m, Traversable t)
  => LocusT t -> LocusT t -> m (EmitData, t (Normalized Range, EmitData))
regenEmStByLocus prevLocus newLocus =
  deleteEmByLocus prevLocus >> genEmStFromLocus newLocus

-- | Generate an `EmitData` from a Locus including both amplitude, range and
-- phase. The newly generated entries simply overwrites the previous ones.
genEmStFromLocus
  :: ( GensymEmitterWithStateError sig m
     , Traversable t
     )
  => LocusT t -> m (EmitData, t (Normalized Range, EmitData))
genEmStFromLocus Locus{loc, part=NPartition{nranges}, qty, degrees} = do
  rEms <- genEmStByRanges qty nranges
  evPhaseRef <- genPhase qty (listToMaybe degrees) loc
  evAmp <- genAmp qty loc
  let edL = mtEmitData { evPhaseRef, evAmp }
  emitSt %= (at (inj loc) ?~ edL)
  return ( edL , rEms )

genEmFromLocus
  :: ( Has (Gensym Emitter) sig m, MayFail sig m , Traversable t)
  => LocusT t -> m (EmitData, t (Normalized Range, EmitData))
genEmFromLocus Locus{loc, part=NPartition{nranges}, qty, degrees} = do
  rEms <- genEmByRanges qty nranges
  evPhaseRef <- genPhase qty (listToMaybe degrees) loc
  evAmp <- genAmp qty loc
  let edL = mtEmitData { evPhaseRef, evAmp }
  return ( edL , rEms )

{-# DEPRECATED genEmStUpdatePhaseFromLocus
    "What's the differnce between update and overwrite?"
  #-}
-- | Update existing `EmitData` based on degree information from a Locus.
genEmStUpdatePhaseFromLocus
  :: ( GensymEmitterWithState sig m
     , Has (Error Builder) sig m
     )
  => Locus -> m [EmitData]
genEmStUpdatePhaseFromLocus Locus{loc, qty, degrees} =
  zipWithM (genEmStUpdatePhase qty) degrees [loc]

-- | Append the given `EmitData` to the given entry.
appendEmSt
  :: (StateMayFail sig m, HasCallStack)
  => Normalized RangeOrLoc -> EmitData -> m EmitData
appendEmSt rl ed = do
  emitSt %= Map.adjust (<> ed) rl
  findEm rl

extendEmSt
  :: (StateMayFail sig m, HasCallStack)
  => Normalized RangeOrLoc -> EmitData -> m EmitData
extendEmSt rl ed = do
  emitSt %= Map.insertWith (flip (<>)) rl ed
  findEm rl


{-# DEPRECATED genEmStUpdatePhase
    "What's the differnce between update and overwrite?"
  #-}
-- | Update an existing `EmitData` by generating a phase from a given degree.
genEmStUpdatePhase
  :: GensymEmitterWithStateError sig m
  => QTy -> Int -> Loc -> m EmitData
genEmStUpdatePhase qt i l  = errTrace (pp "`genEmStUpdatePhase`") $ do
  evPhaseRef  <- genPhase qt (Just i) l
  appendEmSt (inj l) (mtEmitData {evPhaseRef})

-- {-# DEPRECATED genEmStUpdateKets
--     "What's the differnce between update and overwrite?"
--   #-}
genEmStUpdateKets
  :: GensymEmitterWithStateError sig m
  => QTy -> [Normalized Range] -> m [Var]
genEmStUpdateKets qty nranges = do
  vtys <- genKetsByQTy qty (denorm <$> nranges)
  zipWithM_ (\r evBasis -> appendEmSt (inj r) (mtEmitData{evBasis}))
    nranges vtys
  return (fsts (catMaybes vtys))


-- ** Getters
findEmInEmitState
  :: MayFail sig m => EmitState -> Normalized RangeOrLoc -> m EmitData
findEmInEmitState es rl = do
  maybe (complain es) return (es ^. at rl)
  where
    complain st = throwError $
      rl <+> "cannot be found in emitSt" <!> line <+> incr4 st

findEm :: (StateMayFail sig m, HasCallStack) => Normalized RangeOrLoc -> m EmitData
findEm rl = use emitSt >>= (`findEmInEmitState` rl)

findEms
  :: (Traversable t, StateMayFail sig m)
  => t (Normalized RangeOrLoc) -> m (t EmitData)
findEms = mapM findEm

findEmsByLocusInEmitState
  :: ( MayFail sig m , Traversable t)
  => EmitState -> LocusT t -> m (EmitData, t (Normalized Range, EmitData))
findEmsByLocusInEmitState es Locus{loc, part=NPartition{nranges}, qty, degrees} =
  liftM2 (,) (findEm' (inj loc)) (mapM perRange nranges)
  where
    findEm' = findEmInEmitState es
    perRange r = (r,) <$> findEm' (inj r)

findEmsByLocus :: ( StateMayFail sig m , Traversable t)
               => LocusT t -> m (EmitData, t (Normalized Range, EmitData))
findEmsByLocus Locus{loc, part=NPartition{nranges}, qty, degrees} = do
  liftM2 (,) (findEm (inj loc)) (mapM perRange nranges)
  where
    perRange r = (r,) <$> findEm (inj r)

-- | Find the EmitData and visit it with an accessor
visitEm :: Has (Error Builder) sig m => (EmitData -> Maybe a) -> EmitData -> m a
visitEm evF ed = do
  maybe (complain ed) return (evF ed)
  where
    complain ed' = throwError $
      "Attempting to access non-Just field in" <+> ed

visitEms
  :: Has (Error Builder) sig m
  => (EmitData -> Maybe a) -> [EmitData] -> m [a]
visitEms f = mapM (visitEm f)

findVisitEm
  :: StateMayFail sig m
  => (EmitData -> Maybe c) -> Normalized RangeOrLoc -> m c
findVisitEm evF = findEm >=> visitEm evF

findVisitEms
  :: (Traversable t, StateMayFail sig m)
  => (EmitData -> Maybe c) -> t (Normalized RangeOrLoc) -> m (t c)
findVisitEms f = errTrace (pp "findVisitEms") .
  mapM (findVisitEm f)


-- *** Shorthands
visitEmBasis :: Has (Error Builder) sig m => EmitData -> m (Var, Ty)
visitEmBasis = visitEm evBasis

visitEmsBasis :: Has (Error Builder) sig m => [EmitData] -> m [(Var, Ty)]
visitEmsBasis = mapM visitEmBasis

-- findVisitEmsBasis
--   :: (Traversable t, StateMayFail sig m)
--   => t (Normalized RangeOrLoc)[RangeOrLoc] -> m [(Var, Ty)]
-- findVisitEmsBasis = findVisitEms evBasis

findEmitBasisByRange
  :: StateMayFail sig m
  => Normalized Range -> m (Var, Ty)
findEmitBasisByRange = findVisitEm evBasis . inj

findEmitBasesByRanges
  :: StateMayFail sig m
  => [Normalized Range] -> m [(Var, Ty)]
findEmitBasesByRanges = findVisitEms evBasis . (inj <$>)

-- ** Destructor
deleteEm :: (Has (State TState) sig m) => RangeOrLoc -> m ()
deleteEm rl = emitSt %= sans (normalize rl)

deleteEmByLocus
  :: (Has (State TState) sig m, Traversable t)
  => LocusT t -> m ()
deleteEmByLocus Locus{loc, part=NPartition{nranges}} =
  deleteEmPartition loc nranges

deleteEms
  :: (Has (State TState) sig m, Traversable t)
  => t (Normalized RangeOrLoc) -> m ()
deleteEms s =
  emitSt %= (`Map.withoutKeys` Set.fromList (toList s))

deleteEmPartition
  :: (Has (State TState) sig m, Traversable t)
  => Loc -> t (Normalized Range) -> m ()
deleteEmPartition l rs =
  deleteEm (inj l) >> deleteEms (inj <$> rs)


-- ** Together
findThenGenLocus
  :: GensymEmitterWithStateError sig m
  => Locus -> m (LocusEmitData, LocusEmitData)
findThenGenLocus locus =
  liftA2 (,) (findEmsByLocus locus) (genEmStFromLocus locus)

-- ** Helpers
fsts :: [(a, b)] -> [a]
fsts = (fst <$>)

extractEmitablesFromLocus :: StateMayFail sig m => Locus -> m [(Var, Ty)]
extractEmitablesFromLocus Locus{loc, part} = do
  emLoc <- findEm (inj loc)
  emRanges <- (findEm . inj) `mapM` nranges part
  return $ concatMap extractEmitables (emLoc : emRanges)

extractEmitablesFromEds :: EmitData -> [(r, EmitData)] -> [(Var, Ty)]
extractEmitablesFromEds eds rEds =
  concatMap extractEmitables (eds : (snd <$> rEds))

eraseRanges :: (EmitData, [(r, EmitData)]) -> [EmitData]
eraseRanges (ed, eds) = ed : (snd <$> eds)

eraseMatchedRanges
  :: (MayFail sig m)
  => [(LocusEmitData, LocusEmitData)] -> m [(EmitData, EmitData)]
eraseMatchedRanges = (concat <$>) . eraseMatchedRanges'

eraseMatchedRanges'
  :: (MayFail sig m, Traversable t)
  => t (LocusEmitData, LocusEmitData) -> m (t [(EmitData, EmitData)])
eraseMatchedRanges' = mapM (uncurry go)
  where
    -- go :: LocusEmitData -> LocusEmitData -> m [(EmitData, EmitData)]
    go led1 led2 = zipWithExactly' (,) (eraseRanges led1) (eraseRanges led2)
    zipWithExactly' = zipWithExactly pp pp


-- | Extract matched bindings between two EmitData. Unless the leftJoin
-- semantics is specified, any "unbalanced" absence of value raises an error.
-- When leftJoin is `True`, the absence of value on the left is permissible.
extractMatchedEmitables
  :: MayFail sig m => Bool -> EmitData -> EmitData -> m [((Var, Ty), (Var, Ty))]
extractMatchedEmitables leftJoin ed1 ed2 = do
  b <- one2one (evBasis ed1)  (evBasis ed2)
  a <- one2one (evAmp ed1)    (evAmp ed2)
  p <- one2one (evP ed1)      (evP ed2)
  return $ b ++ a ++ (uncurry zip `concatMap` p)
  where
    one2one (Just a) (Just b) = pure [(a, b)]
    one2one Nothing  Nothing  = pure []
    one2one Nothing _ | leftJoin = pure []
    one2one a1 a2 = throwError $ vsep
      [ pp "Non 1-1 conrrespondence between the following two EmitData's."
      , incr4 ed1
      , incr4 ed2]

    evP = (uncurry extractRef <$>) .  evPhaseRef
    extractRef PhaseRef{prBase, prRepr} ty =
      [(prBase, TNat), (prRepr, ty)]
