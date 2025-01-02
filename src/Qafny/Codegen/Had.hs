module Qafny.Codegen.Had where

import           Control.Arrow
    (Arrow (second))
import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR

throwError'
  :: ( Has (Error Builder) sig m )
  => Builder -> m a
throwError' = throwError @Builder . ("[Codegen/Had] " <+>)

-- | Generate statements to promote the type of a locus.
codegenNorToHad
  :: ( Has (Error Builder) sig m)
  => CastScheme -> m [Stmt']
codegenNorToHad
  CastScheme{ schEdsFrom=edsFrom@(lEdFrom, rsEdFrom), schEdsTo=edsTo@(lEdTo, rsEdTo)
            , schQtFrom , schQtTo
            } =
  case rules schQtFrom schQtTo of
    Nothing -> throwError' $
      schQtFrom <+> "cannot be casted into" <+> schQtTo <+> line <+>
      incr2 (vsep (second byComma <$> [edsFrom, edsTo]))
    Just s  -> return s
  where
    rules :: QTy -> QTy -> Maybe [Stmt']
    -- "nor < *"
    rules TNor THad = do
      -- | Cast Nor to first degree Had
      -- No amplitude is involved
      (PhaseRef vBase vRepr, TSeqNat) <- evPhaseRef lEdTo
      [(vKet, TSeqNat)]               <- (evBasis . snd) `mapM` rsEdFrom
      return $ SEmit <$>
        [ [vRepr] :*:=: ["CastNorHad_Phase_1st" >$ vKet]
        , [vBase] :*:=: [2] ]
    rules _ _ = Nothing
