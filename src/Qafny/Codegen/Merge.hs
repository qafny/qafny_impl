{-# LANGUAGE
    TypeFamilies
  #-}

module Qafny.Codegen.Merge(codegenMergeScheme) where

-- Effects
import           Qafny.Effect

-- Handlers

-- Utils

import           Text.Printf
    (printf)

-- Qafny

import           Qafny.Analysis.Partial
    (Reducible (reduce))
import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR

import           Control.Monad
    (liftM3, when)
import           Data.Sum
    (Injection (inj))
import           Qafny.Codegen.Common
    (codegenAssignEmitData, codegenAssignEmitData')
import           Qafny.Syntax.Emit
import           Qafny.Utils.Common
import           Qafny.Utils.EmitBinding
import           Qafny.Utils.Utils
    (dumpSt, tracep)
import Data.Maybe (fromJust)


throwError'
  :: ( DafnyPrinter b, Has (Error Builder) sig m )
  => b -> m a
throwError' = throwError . ("[Codegen/Merge] " <+>)



--------------------------------------------------------------------------------
-- * Merge Semantics
--------------------------------------------------------------------------------
-- | Merge semantics of a Had qubit into one EN emitted state
-- uses the name of the emitted seq as well as the index name
addENHad1 :: Var -> Exp' -> Stmt'
addENHad1 vEmit idx =
  (::=:) vEmit $
    EOp2 OAdd (EVar vEmit) (EEmit $ ECall "Map" [eLamPlusPow2, EVar vEmit])
  where
    vfresh = "x__lambda"
    eLamPlusPow2 =
      simpleLambda vfresh $
        EOp2 OAdd (EVar vfresh) (EEmit (ECall "Pow2" [idx]))


-- | Multiply the Had coutner by 2
doubleHadCounter :: Var -> Stmt'
doubleHadCounter vCounter =
  (::=:) vCounter $ EOp2 OMul (ENum 2) (EVar vCounter)


-- | Generate from the merge scheme statements to perform the merge and the
-- final result variable.
codegenMergeScheme
  :: ( Has (Gensym Emitter) sig m
     , Has (Gensym String) sig m
     , Has (State TState) sig m
     , Has (Error Builder) sig m
     , Has Trace sig m
     )
  => [MergeScheme] -> m [(Stmt', Var)]
codegenMergeScheme = (concat <$>) . mapM go
  where
    go MMove = throwError' (pp "I have no planning in solving it here now.")
    go (MJoin JoinStrategy { jsQtMain=qtMain, jsQtMerged=qtMerged
                           , jsLedMain, jsLedMerged, jsLedInto
                           }) = do
      let (edlMain, ( nrMain, edMain)) = jsLedMain
          (edlMerg, ( nrMerg, edMerg)) = jsLedMerged
          (edlInto, (_nrInto, edInto)) = jsLedInto
      case (qtMain, qtMerged) of
        (TEn01, TNor)     -> do
          (vEmitMain, _) <- visitEmBasis edMain
          (vEmitMerg, _) <- visitEmBasis edMerg
          (vEmitInto, _) <- visitEmBasis edInto
          vBind <- gensym "lambda_x"
          let stmt = vEmitInto ::=: callMap ef vEmitMain
              ef   = simpleLambda vBind (EVar vBind + EVar vEmitMerg)
          return [(stmt, vEmitInto)]
        (TEn, THad)       -> do
          let (Normalized (Range _ lBoundH rBoundH)) = nrMerg
          unless (reduce (rBoundH - lBoundH) == 1) $
            throwError' . vsep $
            [ pp "Merging TEn with THad of cardinality != 1 is disallowed "
            , incr4 nrMerg ]
          (vKetMain, _) <- visitEmBasis edMain
          let (Normalized (Range _ lBound rBound)) = nrMain
          let stmtAdd = addENHad1 vKetMain (reduce (rBound - lBound))
          tracep "FIXME: (Merge) a phase promotion maybe needed!"
          let pvMain = evPhaseRef edlMain
              pvMerg = evPhaseRef edlMerg
              pvInto = evPhaseRef edlInto
          vBind <- gensym "lambda_x"
          let svPhase = mergePhase vBind pvMain pvMerg pvInto
          return $ (stmtAdd, vKetMain):svPhase
        _unsupportedMerge -> throwError' $
          "No idea about " <!> qtMain <!> " to " <!> qtMerged <!> " conversion."
    go (MEqual EqualStrategy{esEdIntoFrom}) = codegenAssignEmitData' False
      =<< eraseMatchedRanges esEdIntoFrom

    mergePhase _ Nothing Nothing Nothing = []
    mergePhase v pvMain pvMerg pvInto =
      fromJust $ liftM3 (goMergePhase v) pvMain pvMerg pvInto

    goMergePhase v (pvMain, _) (pvMerg, _) (pvInto, _) =
      let PhaseRef{prRepr=reprMain} = pvMain
          PhaseRef{prRepr=reprMerg} = pvMerg
      in [ (prBase pvInto >::=: prBase pvMain, prBase pvInto)
         , ( prRepr pvInto >::=:
             (reprMain >+ 
              callMap (simpleLambda v (reprMain >+ (reprMerg >:@: (0 :: Exp')))) reprMain)
           , prRepr pvInto ) ]
