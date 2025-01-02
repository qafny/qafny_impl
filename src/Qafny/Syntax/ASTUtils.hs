{-# LANGUAGE
    TupleSections
  #-}

module Qafny.Syntax.ASTUtils where

import           Qafny.Syntax.AST
import           Text.Printf
    (printf)


-- getPhaseRef :: PhaseTy -> PhaseRef
-- getPhaseRef (PTN _ r) = r
-- getPhaseRef _         = undefined

-- getPhaseRefMaybe :: PhaseTy -> Maybe PhaseRef
-- getPhaseRefMaybe (PTN _ r) = Just r
-- getPhaseRefMaybe _         = Nothing


-- getPhaseRefN :: [PhaseTy] -> [(Int, PhaseRef)]
-- getPhaseRefN ptys = do
--   pty <- ptys
--   case pty of
--     PT0      -> []
--     PTN n pr -> return (n, pr)

-- phaseRefToTy :: Int -> Maybe PhaseRef -> PhaseTy
-- phaseRefToTy 0 Nothing  = PT0
-- phaseRefToTy n (Just p) = PTN n p
-- phaseRefToTy n m        = error $ printf "Degree mismatch %d =/= %s" n (show m)
