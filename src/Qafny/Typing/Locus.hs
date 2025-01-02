module Qafny.Typing.Locus where

import           Control.Lens
    (at, (?~))
import           Qafny.Effect
import           Qafny.Syntax.IR


updateMetaStByLocus
  :: ( Has (State TState) sig m )
  => Locus -> m ()
updateMetaStByLocus s@Locus{loc, part, qty, degrees} =
  sSt %= (at loc ?~ (part, (qty, degrees)))
