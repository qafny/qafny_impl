module Qafny.Runner where
import           Carrier.Gensym.Emit
    (GensymC, runGensymEmit)
import           Control.Carrier.Reader
    (runReader)
import           Control.Carrier.State.Lazy
    (StateC, runState)
import           Control.Carrier.Trace.Returning
    (runTrace)
import           Data.Functor.Identity
    (Identity)
import           Data.Maybe
    (catMaybes)
import           Qafny.Codegen
    (codegenAST)
import           Qafny.Config
    (Configs)
import           Qafny.Effect
import           Qafny.Syntax.AST
    (AST, Toplevel', Var)
import           Qafny.Syntax.Emit
    (Builder, prettyIO)
import           Qafny.Syntax.EmitBinding
    (Emitter)
import           Qafny.Syntax.IR
import           Qafny.Syntax.Parser
    (scanAndParse)

--------------------------------------------------------------------------------
-- Wrapper
--------------------------------------------------------------------------------
data Production a = Production
  { pResult ::  Either Builder a
  , pDetail :: [Either Builder Toplevel']
  , pState  :: [(Var, TState)]
  , pTrace  :: String
  }

collectErrors :: Production a -> [(Var, Builder)]
collectErrors Production{ pDetail=detail, pState=st } = catMaybes $ do
  (errHuh, (v, st')) <- zip detail st
  case errHuh of
    Left s -> return (Just (v, s))
    _      -> return Nothing

--------------------------------------------------------------------------------
-- | Runner
--------------------------------------------------------------------------------
runCodegen
  :: Configs -> AST
  -> ([String], ([((Var, TState), Either Builder Toplevel')], Either Builder AST))
runCodegen conf ast = do
  run . run' $ codegenAST ast
  where
    run' =
      runTrace .
      runReader conf .
      runReader initTEnv

produceCodegen :: Configs -> AST -> Production AST
produceCodegen conf ast =
  let (trace, (stm, res)) = runCodegen conf ast
      (st, ms) = unzip stm
  in Production { pResult = res
                , pState = st
                , pDetail = ms
                , pTrace = sep ++ "Trace:\n\n" ++ unlines trace ++ sep
                }
  where
    sep = replicate 80 '=' ++ "\n"

-- Load a source file (specified by its name w/o the extension) and parse it
-- into an AST
loadFileIO :: String -> IO AST
loadFileIO prog = do
  file <- readFile $ "./test/Resource/" ++ prog ++ ".qfy"
  either fail return $ scanAndParse file

formatProg :: String -> IO ()
formatProg s = case scanAndParse s of
  Left parseErr -> putStrLn parseErr
  Right ast     -> prettyIO ast

--------------------------------------------------------------------------------
debugGensymEmitterWithState
  :: StateC TState (GensymC Emitter Identity) a
  -> (Int, [(Emitter, String)], (TState, a))
debugGensymEmitterWithState =
  run . runGensymEmit . runState initTState
--------------------------------------------------------------------------------

