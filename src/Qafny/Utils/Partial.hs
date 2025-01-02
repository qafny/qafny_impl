module Qafny.Utils.Partial where

import qualified Data.Map.Strict         as Map

import           Data.Foldable
    (Foldable (foldl'))
import           Qafny.Analysis.Interval
import           Qafny.Syntax.AST
import           Qafny.Syntax.IR

-- | Compute the dynamic bound applicable to phase, amplitude, and ket
-- representations from the specs.
--
-- For example, if `κ1` has two specs with interval `[0..10]` and `[10..20]`,
-- we take its lub `[0..20]`
--
-- Static bounds which can be extracted directly from the kinding information is
-- not gathered here.
gatherDynBoundsFromSpec :: SRel -> [Intv]
gatherDynBoundsFromSpec = go
  where
    go RNor{} = []
    go RHad{} = []
    go RWild  = []
    go (REn specs)   = enIntvSup   <$> specs
    go (REn01 specs) = en01IntvSup <$> specs
