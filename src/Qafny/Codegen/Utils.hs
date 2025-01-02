--------------------------------------------------------------------------------
-- |
-- Utilities for code generation.
--------------------------------------------------------------------------------

module Qafny.Codegen.Utils where

import           Data.Bool
    (bool)
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit

--------------------------------------------------------------------------------
-- * Splits
--
-- In a conditional, the portion of the states satisfying the condition should
-- not be changed. Therefore, an easy way is to produce two for each loop such
-- that one block has state-changing statements emitted, another has those
-- statements as no-ops.
--
-- putPure and putOpt are used to flag which ones are state-changing, and which
-- are not.
--------------------------------------------------------------------------------
-- | Put Stmt regardless of which branch is taken.
putPure :: Has (Reader Bool) sig m
        => [Stmt'] -> m [Stmt']
putPure = pure

-- | Suppress the Stmt if the current context tag is `False`.
putOpt :: Has (Reader Bool) sig m => m [a] -> m [a]
putOpt s = do
  bool (pure []) s =<< ask


-- * Debug and Report

-- | Attach control flow information to error handling.
runWithCallStack
  :: ( Has (Error Builder) sig m
     , DafnyPrinter s
     )
  => s
  -> m b
  -> m b
runWithCallStack s =
  flip (catchError @Builder) fmt
  where
    fmt err = throwError $
      err <+> line <+> "at" <+> line <+> indent 4 s
