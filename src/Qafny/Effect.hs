module Qafny.Effect
  ( module Control.Effect.Reader
  , module Control.Effect.Error
  , module Control.Effect.Catch
  , module Control.Effect.State
  , module Control.Effect.Trace
  , module Control.Effect.Lens
  , module Control.Algebra
  , module Effect.Gensym
  , GensymEmitterWithState, GensymEmitterWithStateError, StateMayFail
  , GensymMeta
  , HasResolution, GenConditionally, MayFail, HasPContext, GensymEmit
  ) where

-- | Re-export useful effects to avoid cluttered imports in other modules

import           Control.Algebra
    (Has)
import           Control.Effect.Catch
import           Control.Effect.Error
import           Control.Effect.Lens
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Trace

import           Effect.Gensym
    (Gensym, gensym)
import           Qafny.Syntax.AST
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Syntax.Emit

-- TODO: Separate EmitStates from MetaStates on the type level.

-- | May mangae and generate emitted symbols
type GensymEmitterWithState sig m =
  (Has (Gensym Emitter) sig m , Has (State TState) sig m)

-- | May mangae and generate emitted symbols, may fail.
type GensymEmitterWithStateError sig m =
  (GensymEmitterWithState sig m, Has (Error Builder) sig m)

type GensymMeta sig m =
  Has (Gensym Var) sig m

type GensymEmit sig m =
  Has (Gensym Emitter) sig m

type StateMayFail sig m =
  (Has (Error Builder) sig m , Has (State TState) sig m)

-- | May perform type resolution.
type HasResolution sig m =
  (StateMayFail sig m, Has (Reader IEnv) sig m, Has Trace sig m)

-- | May generate differently depending on what conditional branch the current
-- context corresponds to.
type GenConditionally sig m =
  Has (Reader Bool) sig m

type MayFail sig m =
  Has (Error Builder) sig m

-- | May run partial evaluation in context
type HasPContext sig m =
  Has (Reader IEnv) sig m
