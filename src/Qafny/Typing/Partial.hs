{-# LANGUAGE
    MultiWayIf
  #-}

-- | Instantiate partial evaluation engine in the type domain.
module Qafny.Typing.Partial where

import           Qafny.Analysis.Interval
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.Subst

import           Data.Foldable
    (Foldable (foldl'))
import           Data.List.NonEmpty
    (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE

-- * Lattice and ordering operators lifted to IEnv

-- | Given a generic bi-operator `<?>`, compute all outcomes under all
-- permutations of the local `IEnv`.
liftIEnv2
  :: ( Has (Reader IEnv) sig m, Substitutable b )
  => (b -> b -> a) -> m (b -> b -> NonEmpty (a, AEnv))
liftIEnv2 (<?>) = do
  ienv <- ask @IEnv
  return $ \r1 r2 ->
    let fvs = fVars r1 ++ fVars r2 ++ concatMap (concatMap fVars . NE.toList . snd) ienv
        aenvs = nondetIEnv $ filterIEnv fvs ienv
        subst' r'' = (\aenv -> fixN (subst aenv) (length aenv) r'') <$> aenvs
    in NE.zipWith (\(r1', r2') -> (r1' <?> r2',)) (NE.zip (subst' r1) (subst' r2)) aenvs

-- | Iterate `n` times
fixN ::  (a -> a) -> Int -> (a -> a)
fixN f = go
  where
    go 0 = id
    go n = go (n - 1) . f

-- | lift interval inclusion to the local environment.
(⊑/)
  :: ( Has (Reader IEnv) sig m )
  => m (Range -> Range -> NonEmpty (Maybe Bool, AEnv))
(⊑/) = liftIEnv2 (⊑)

-- | Check if all results are _known_ to be `True`
allI :: NonEmpty (Maybe Bool, a) -> Bool
allI = all ((== Just True) . fst)

-- | every pair of intervals in the permutaton are known to be "included"
(∀⊑/)
  :: ( Has (Reader IEnv) sig m )
  => m (Range -> Range -> Bool)
(∀⊑/) = (allI .:) <$> (⊑/)

isBotI
  :: ( Has (Reader IEnv) sig m )
  => m (Range -> NonEmpty (Maybe Bool))
isBotI = do
  ienv <- ask
  return $ \r -> isBot .
    (\env -> fixN (substR env) (length env) r)
    <$> nondetIEnv ienv


--------------------------------------------------------------------------------
-- * Instantiate lub in `Intv` domain (defined in AST)
--------------------------------------------------------------------------------
-- | Assumption: both intervals are well-formed, so it suffices to compare
-- between lefts and between rights only.
--
instLubIntv :: (Has (Reader IEnv) sig m) => m ([Intv] -> Maybe Intv)
instLubIntv = do
  lubSome <$> liftIEnv2 (⊑)
  where
    lubSome (≤) [ ]  = Nothing
    lubSome (≤) (x:xs) =
      foldl' go' (Just x) xs
      where
        go' acc y = acc >>= lub' y
        lub' (Intv al ar) (Intv bl br) = do
          l <- if | allI (al ≤ bl) -> Just al
                  | allI (bl ≤ al) -> Just bl
                  | otherwise      -> Nothing
          r <- if | allI (ar ≤ br) -> Just br
                  | allI (br ≤ ar) -> Just ar
                  | otherwise      -> Nothing
          pure (Intv l r)
