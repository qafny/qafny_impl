{-# LANGUAGE
    ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , TypeOperators
  #-}
module Qafny.Utils.Utils where

--
import           Control.Applicative (liftA2)
import           Control.Effect.Error
    (Error, catchError, throwError)
import           Control.Effect.Lens
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Trace
import           Control.Lens
    (at, (^.))
import           Control.Monad
    (join, unless)

import           Data.Bifunctor
import           Data.Sum
--
import           Effect.Gensym
    (Gensym, gensym)

-- Qafny
import           Qafny.Effect
    (MayFail)
import           Qafny.Error
    (QError (UnknownVariableError))
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit
import           Qafny.Syntax.IR
import           Qafny.Variable
    (Variable (variable))

--------------------------------------------------------------------------------
-- * 3-Tuples

first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (a, b, c) = (f a, b, c)


uncurry3 ::  (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

fst2 :: (a, b, c) -> (a, b)
fst2 (a, b, c) = (a, b)

internalError :: a
internalError = error "Internal Error!"

trd :: (a, b, c) -> c
trd (_, _, c) = c

third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f (a, b, c) = (a, b, f c)
--------------------------------------------------------------------------------

onlyOne
  :: ( Has (Error Builder) sig m
     , DafnyPrinter a
     )
  => (Builder -> m a) -> [a] -> m a
onlyOne throw v =
  case v of
    [v'] -> return v'
    _    -> throw $
      "Expecting only one element, but given:" <+> list v <+> "!"

-- | Catch the error in the Maybe and rethrow it as an Error
rethrowMaybe
  :: ( Has (Error Builder) sig m, DafnyPrinter s )
  => m (Maybe a) -> s -> m a
rethrowMaybe mayFail err =
  mayFail >>= fromMaybeM (throwError (pp err))

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM b = maybe b pure

gensymLoc
  :: ( Has (Gensym String) sig m )
  => String -> m Loc
gensymLoc = (Loc <$>) . gensym . variable . Loc

--------------------------------------------------------------------------------
-- * Gensym Utils
--
-- $doc
-- The following functions operate on a 'Range' and a 'QTy', form a `Binding` to
-- be normalized to a variable name, perform modification and query to the emit
-- symbol state and the __Gensym EmitBinding__ effect.
-- $doc
--------------------------------------------------------------------------------

-- gensymBase
--   :: ( Has (Gensym EmitBinding) sig m )
--   => Range -> Int -> m Var
-- gensymBase r i = gensym $ rbindingOfRange r (inj i)

-- rbindingOfRangeQTy :: Range -> QTy -> EmitBinding
-- rbindingOfRangeQTy r qty = RBinding (reduce r, inj qty)

-- rbindingOfRangePTyRepr :: Range -> PhaseTy -> Maybe EmitBinding
-- rbindingOfRangePTyRepr r pty =
--   pty <&> RBinding . (reduce r, ) . snd



--------------------------------------------------------------------------------
exp2AExp
  :: (Has (Error Builder) sig m)
  => Exp' -> m AExp
exp2AExp (EVar v) = return $ AVar v
exp2AExp (ENum n) = return $ ANat n
exp2AExp e = throwError $
  e <+> "cannot be projected to an AExp."

dumpSt
  :: ( Has (State TState) sig m
     , Has Trace sig m
     )
  => String -> m ()
dumpSt str = do
  s <- get @TState
  tracep $ vsep
    [ pp "[info] The state after ("<!>str<!>") is:"
    , incr4 s ]

--------------------------------------------------------------------------------
-- * Method Types

-- | Retrive the type of the given formal variable from the environment
getMethodType
  :: ( Has (Error Builder) sig m
     , Has (Reader TEnv) sig m
     )
  => Var -> m MethodType
getMethodType v = do
  tyM :: Maybe MTy  <- asks (^. kEnv . at v)
  case tyM of
    Just mty ->
      case unMTy mty of
        Inl ty  -> throwError $ v <+> "is not a method but a" <+> ty
        Inr mty -> pure mty
    _             -> asks (^. kEnv) >>= throwError . pp . UnknownVariableError v

haveSameLength
  :: ( Has (Error Builder) sig m
     , DafnyPrinter a
     , DafnyPrinter b
     , DafnyPrinter c
     )
  => c -> [a] -> [b] -> m ()
haveSameLength blame vsEmit eValues =
  unless (length vsEmit == length eValues) $
    throwError @Builder $ vsep
      [ ">>"<+>parens blame <+> "the number of elements doesn't agree with each other:"
      , incr4 (list vsEmit)
      , incr4 (list eValues) ]

--------------------------------------------------------------------------------
-- | Catch error and add information to it
errTrace :: (Has (Error Builder) sig m, DafnyPrinter s) => s -> m a ->  m a
errTrace info m =
  catchError m (\e -> throwError (e <> line <> "↑ " <+> info))



both :: Bifunctor f => (a -> b) -> f a a -> f b b
both = join bimap

bothM :: Applicative m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (a, b) = liftA2 (,) (f a) (f b)

hasNoDup :: Eq a => [a] -> Bool
hasNoDup [] = True
hasNoDup (x:xs) = foldr go True xs
  where
    go x' ans = x == x' && ans

tracep :: (Has Trace sig m, DafnyPrinter s) => s -> m ()
tracep = trace . showEmit0


zipWithExactly
  :: MayFail sig m
  => (a -> Builder) -> (b -> Builder) -> (a -> b -> c) -> [a] -> [b] -> m [c]
zipWithExactly ppa ppb f l1 l2 = go l1 l2
  where
    go []     []     = pure []
    go (x:xs) (y:ys) = (f x y :) <$> go xs ys
    go _      _      = throwError $ vsep
      [ pp "The follwoing two lists are of difference lengths."
      , incr4 $ align (vsep [ list (ppa <$> l1)
                            , list (ppb <$> l2)])
      ]
