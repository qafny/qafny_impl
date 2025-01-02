module Qafny.Typing.Lambda where

import           Control.Monad
    (guard)
import           Control.Monad.Fail
import           Data.List
    (uncons)
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.Emit
import           Qafny.Syntax.IR
import           Qafny.Typing.Range
    (areRangesEquiv)
import           Qafny.Utils
    (fst2)
import           Qafny.Utils.Common
import           Text.Printf
    (printf)

data LambdaType
  = LamUnary (Range, Range, Locus)
  | LamBinary (Range, Range, Locus) (Range, Range, Locus)
  | LamPhaseKickback (((Range, Range, Locus), Var, Exp')
                     ,((Range, Range, Locus), Var, Exp'))

analyzeLambdaType
  :: MayFail sig m => [(Range, Range, Locus)] -> Lambda -> m LambdaType
analyzeLambdaType rsAndLoci LambdaF{bBases, eBases} =
  case rsAndLoci of
    []  -> throwError (pp "An oracle cannot be applied to an empty partition.")
    [r] -> pure (LamUnary r)
    [rl1, rl2] -> case checkKickback rsAndLoci bBases eBases of
      Nothing -> do
        let rsMap =  (fst2 <$> rsAndLoci)
        unless (areRangesEquiv rsMap) $
          throwError (pp (errRangesAreProper rsMap))
        pure (LamBinary rl1 rl2)
      Just s -> pure $ LamPhaseKickback s
    -- _ ->
    --   throwError' "An oracle function can take no more than 2 arguments."
  where
    errRangesAreProper :: [(Range, Range)] -> String
    errRangesAreProper rMap = printf
      "Ranges given on the LHS of the application contains some incomplete range(s).\n%s"
      (showEmit0 $ vsep rMap)


checkKickback
  :: [(Range, Range, Locus)] -> [Var] -> [Exp']
  -> Maybe (((Range, Range, Locus), Var, Exp')
           ,((Range, Range, Locus), Var, Exp'))
checkKickback rsl vs es = do
  (hadEntry, []) <- uncons had
  (enEntry, []) <- uncons notHad
  (_, vEn, EVar vExpEn) <- pure enEntry
  (_, vHad, EOp2 OAdd e1 e2) <- pure hadEntry
  guard (vEn == vExpEn)
  case (e1, e2) of
    (EVar vSelf, EApp _ [EVar vOther]) -> do
      guard (vSelf == vHad && vEn == vOther)
      pure (hadEntry, enEntry)
    (EApp _ [EVar vOther], EVar vSelf) -> do
      guard (vSelf == vHad && vEn == vOther)
      pure (hadEntry, enEntry)
    _ -> Nothing
  where
    zipped = zip3 rsl vs es
    hadish ((_, _, Locus{qty}), _, _) = qty == THad
    spanned@(had, notHad) = span hadish zipped
