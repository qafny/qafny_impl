--------------------------------------------------------------------------------
-- |
-- Code generation of amplitude to its real representation.
--
-- Unless stated explicitly, all amplitudes here refer to the probability
-- instead.
--------------------------------------------------------------------------------
module Qafny.Codegen.Amplitude where

import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory

-- | Given a sequence standing for a representation of equal probabilities,
-- calculate the amplitude for each term.
ampFromRepr :: AstInjection a Exp' => a -> Exp'
ampFromRepr vRepr =
  seqLike TReal vRepr (constLambda eAmp)
  where
    eAmp  = (1 :: Exp') >// denom
    denom = mkPow2 (mkCard vRepr)
