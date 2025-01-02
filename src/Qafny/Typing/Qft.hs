module Qafny.Typing.Qft where

import           Control.Monad
    (guard)
import           Data.List
    (delete)
import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit
import           Qafny.Syntax.IR
import           Qafny.Typing.Locus
    (updateMetaStByLocus)
import           Qafny.Typing.Phase
    (enDegree)

throwError'
  :: ( Has (Error Builder) sig m )
  => Builder -> m a
throwError' = throwError . ("[Qft Typing] " <!>)

typingQft
  :: GensymEmitterWithStateError sig m
  => Range -> Locus -> m Locus
typingQft rApplied locus = do
  let newLocusMaybe = locusAfterQftPure rApplied locus
  newLocus <- maybe (throwError' (pp "Internal error!")) return newLocusMaybe
  updateMetaStByLocus newLocus
  return newLocus

-- | Calculate the locus after applying Qft to it.
--
-- In particular, ranges in the original locus is reordered such that the range to
-- which Qft applied is move to the front of the range list. This property will be
-- used in calculating emit variables.
--
locusAfterQftPure :: Range -> Locus -> Maybe Locus
locusAfterQftPure rApplied l = guard requires >> return newLocus
  where
    nrApplied = normalize rApplied
    Locus {loc, part=NPartition{nranges}, qty, degrees} = l
    requires = (nrApplied `elem` nranges) && qty == TEn && degrees == [1]
    newLocus =
      let rRemainder = delete nrApplied nranges
          rsReorder  = nrApplied : rRemainder
          part       = npart rsReorder
          l'         = l { loc, part }
      in case rRemainder of
           -- Qft over singleton partition simply promotes the phase degree
           [] -> l'{ degrees=enDegree 2 }
           -- Qft over compound partition promotes the entire type to Qft
           xs -> l'{ qty=TQft, degrees=enDegree 2 }


{-

Note [Qft Type Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~

The new `Locus` evolved by a Qft operator may only differ from the original one
in the `part`, `qty`, and `degrees` part. In particular,

  - `part` only differs in the order: the range to which Qft applies is moved to
    the front
  - `qty` either stays the same or be promoted to `TQft`
  - `degrees` shall be promoted by one always.
-}
