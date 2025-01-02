{-# LANGUAGE
    NamedFieldPuns
  , TypeApplications
  #-}

module Qafny.Typing.Error where

import qualified Control.Carrier.Error.Either as ErrE
import           Control.Effect.Error
import           Qafny.Syntax.AST
    (Range)
import           Qafny.Syntax.Emit
import           Qafny.Syntax.IR

data SCError
  = SplitENError    Locus Range Range [Range]
  | SplitOtherError Builder


instance DafnyPrinter SCError where
  pp (SplitENError s@Locus{part} r0 rAff rs) = vsep
    [ "The partition" <+> r0 <+> "cannot be obtained from an 'En' partition"
      <+> part
    , "Reason: it requires tearing the range" <+> rAff <+> "apart into"
      <+> byComma rs
    , pp "Advice: Use `EN01` isntead."
    , "Info:" <+> s
    ]
  pp (SplitOtherError s) = s

failureAsSCError
  :: ( Has (Error SCError) sig m )
    => ErrE.ErrorC Builder m b -> m b
failureAsSCError m = do
  e <- ErrE.runError @Builder m
  case e of
    Left err -> throwError $ SplitOtherError err
    Right v  -> return v

hdlSCError
  :: ( Has (Error Builder) sig m )
    => ErrE.ErrorC SCError m b -> m b
hdlSCError m = do
  e <- ErrE.runError @SCError m
  case e of
    Left err -> throwError $ pp err
    Right v  -> return v

