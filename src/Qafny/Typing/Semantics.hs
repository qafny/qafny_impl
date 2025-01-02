module Qafny.Typing.Semantics where

import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
    (tySn, tySsn)

-- * "Semantics of Entanglement Types"

data TyComp
  = TL [Ty]
  | T0 Ty

class TyInjection a where
  tyInj :: a -> Maybe TyComp

instance TyInjection Ty where
  tyInj = pure . T0

instance TyInjection [Ty] where
  tyInj = pure . TL


data TySem
  = TySem { tsKets      :: Maybe TyComp
          , tsPhase     :: Maybe TyComp
          , tsInner     :: Maybe TySem
          , tsAmplitude :: Maybe Ty
          }

interp :: QTy -> TySem
interp qty = TySem {tsKets, tsPhase, tsInner, tsAmplitude}
  where
    (tsKets, tsPhase, tsInner, tsAmplitude) = case qty of
      TNor -> ( tyInj tySn
              , Nothing
              , Nothing
              , Nothing
              )
      THad -> ( Nothing
              , tyInj tySn
              , Nothing
              , Nothing
              )
      TEn  -> ( tyInj [tySn]
              , tyInj tySn
              , Nothing
              , Just tySn
              )
      TEn01-> ( tyInj [tySsn]
              , tyInj tySn
              , Nothing
              , Just tySn
              )
      TQft -> ( tyInj [tySn]
              , Nothing
              , pure $ interp TEn -- FIXME: seq<...> <$>
              , Just tySn
              )

