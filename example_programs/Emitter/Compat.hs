{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , NamedFieldPuns
  , TupleSections
  , TypeApplications
  , TypeOperators
  #-}


module Qafny.Utils.Emitter.Compat
  (findEmitRanges)
where

-- Qafny
import           Data.Sum
    (Injection (inj))

import           Qafny.Syntax.AST
    (Range, Var)
import           Qafny.Syntax.EmitBinding
import           Qafny.Utils.EmitBinding

{-# DEPRECATED findEmitRanges  "Use 'findVisitED' instead" #-}
findEmitRanges
  :: StateMayFail sig m
  => [Range] -> m [Var]
findEmitRanges = findVisitEDs evBasis . (inj <$>)
