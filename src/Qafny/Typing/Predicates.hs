-- | Specification and predicate typechecking
module Qafny.Typing.Predicates(
  wfSignatureFromPredicate, wfSignatureFromPredicate',
  wfSignatureFromPredicates, dropSignatureSpecs,
  wfQTySpecs
  ) where

-- Effects
import           Data.Foldable
    (Foldable (toList))
import           Data.Maybe
    (catMaybes, listToMaybe)
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit
import           Qafny.Syntax.IR
import           Qafny.Typing.Phase hiding
    (throwError')
import           Qafny.Typing.Utils
    (assertPartQTyArity)
import           Qafny.Utils
import           Qafny.Utils.Common

throwError'
  :: ( Has (Error Builder) sig m, DafnyPrinter s )
  => s -> m a
throwError' = throwError . ("[Typing/Predicates] " <!>)

--------------------------------------------------------------------------------
-- | Check if a specification expression has correct corresponding types.
assertSpecWf :: QTy -> SpecExp -> Bool
assertSpecWf TNor  SESpecNor{}  = True
assertSpecWf THad  SESpecHad{}  = True
assertSpecWf TEn   SESpecEn{}   = True
assertSpecWf TEn01 SESpecEn01{} = True
assertSpecWf _     SEWildcard   = True
assertSpecWf _     _            = False

wfQTySpec :: (Has (Error Builder) sig m) => QTy -> SpecExp -> m SRel1
wfQTySpec TNor  (SESpecNor  s) = pure (RNor  s)
wfQTySpec THad  (SESpecHad  s) = pure (RHad  s)
wfQTySpec TEn   (SESpecEn   s) = pure (REn   s)
wfQTySpec TEn01 (SESpecEn01 s) = pure (REn01 s)
wfQTySpec _     SEWildcard     = pure RWild
wfQTySpec ty    spec           = throwError' $
  "Specification"<+>spec
  <+>"is not well-formed w.r.t. the given entanglement type"
  <+>ty

wfQTySpecs :: (Has (Error Builder) sig m) => QTy -> [SpecExp] -> m SRel
wfQTySpecs qty specs' =
  case qty of
    TNor  -> RNor  <$> mapM goNor         specs
    THad  -> RHad  <$> mapM goHad         specs
    TEn   -> REn   <$> mapM goEn          specs
    TEn01 -> REn01 <$> mapM goEn01        specs
    qt    -> RWild <$  mapM (fmtError qt) specs
  where
    specs = filter (SEWildcard /=) specs'
    goNor (SESpecNor s) = pure s
    goNor s             = fmtError TNor s
    goHad (SESpecHad s) = pure s
    goHad s             = fmtError THad s
    goEn (SESpecEn s) = pure s
    goEn s            = fmtError TEn s
    goEn01 (SESpecEn01 s) = pure s
    goEn01 s              = fmtError TEn01 s
    fmtError ty spec = throwError' $
      "Specification"
      <+>spec
      <+>"is not well-formed w.r.t. the given entanglement type"
      <+>ty

-- | Collect entanglement type signature from predicate-ish clauses
-- without checking the well-formedness of the predicate.
-- Return Nothing if the expression doesn't contain any predicates.
wfSignatureFromPredicate'
  :: Has (Error Builder) sig m
  => Exp' -> m (Maybe (Partition, QTy, Maybe Int, [SpecExp]))
wfSignatureFromPredicate' e@(ESpec part qty es) = do
  unless isWf $
    throwError' ("Specification is not well formed.\n" <!> e)
  return $ Just (part, qty, degree, es)
  where
    degrees = analyzeSpecDegree <$> es
    isWf =
      assertPartQTyArity part qty  -- repr arity matches with qtype
      && all (assertSpecWf qty) es -- all specs are wf w.r.t. qtype
      && hasNoDup degrees          -- all phases have the same degree
    degree = listToMaybe $ catMaybes degrees
wfSignatureFromPredicate' _ = return Nothing

-- | Collect entanglement type signature from predicate-ish clauses
-- without checking the well-formedness of the predicate.
-- Return Nothing if the expression doesn't contain any predicates.
wfSignatureFromPredicate
  :: Has (Error Builder) sig m
  => Exp' -> m (Maybe (Partition, QTy, Maybe Int))
wfSignatureFromPredicate =
  ((strip4 <$>) <$>) . wfSignatureFromPredicate'
  where
    strip4 (a, b, c, d) = (a, b, c)

wfSignatureFromPredicates
  :: Has (Error Builder) sig m
  => [Exp'] -> m [(Partition, QTy, Maybe Int, [SpecExp])]
wfSignatureFromPredicates = (catMaybes <$>) .  mapM wfSignatureFromPredicate'

dropSignatureSpecs
  :: [(Partition, QTy, Maybe Int, [SpecExp])] -> [(Partition, QTy, [Int])]
dropSignatureSpecs = (go <$>)
  where go (a, b, c, _) = (a, b, toList c)
