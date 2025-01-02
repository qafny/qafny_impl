{-# LANGUAGE
    TypeFamilies
  #-}

module Qafny.Codegen.Common where

import           Qafny.Effect
import           Qafny.Syntax.AST
import           Qafny.Syntax.EmitBinding
import           Qafny.Utils.EmitBinding
    (extractMatchedEmitables)

--------------------------------------------------------------------------------
-- * EmitData Utils
--------------------------------------------------------------------------------
codegenAssignEmitData'
  :: MayFail sig m => Bool -> [(EmitData, EmitData)] -> m [(Stmt', Var)]
codegenAssignEmitData' leftJoin eds = concat <$> mapM go eds
  where
    go = ((uncurry perVar <$>) <$>) . uncurry (extractMatchedEmitables leftJoin)
    perVar (v1, _) (v2, _) = (v1 ::=: EVar v2, v1)

codegenAssignEmitData
  :: MayFail sig m => Bool -> [(EmitData, EmitData)] -> m [Stmt']
codegenAssignEmitData leftJoin eds = concat <$> mapM go eds
  where
    go = ((uncurry perVar <$>) <$>) . uncurry (extractMatchedEmitables leftJoin)
    perVar (v1, _) (v2, _) = v1 ::=: EVar v2


codegenAssignEmitDataF
  :: MayFail sig m
  => Bool -> ((Var, Ty) -> (Var, Ty) -> a) -> [(EmitData, EmitData)] -> m [a]
codegenAssignEmitDataF leftJoin perVar = (concat <$>) . mapM go
  where
    go = ((uncurry perVar <$>) <$>) . uncurry (extractMatchedEmitables leftJoin)


