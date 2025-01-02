module Qafny.Syntax.ParserUtils where
import           Qafny.Syntax.AST
import qualified Qafny.Syntax.Lexer as L
import           Text.Printf (printf)

type Parser a = Either String a

withParse :: String -> String
withParse = ("Parser Error: " ++)

-- separatesOnly :: Conds -> Parser Partition
-- separatesOnly (Separates s) = return s
-- separatesOnly c             =
--  Left $ withParse $ show c ++ "is not a `separates` specification"

parseError :: ([L.SToken], [String]) -> Parser a
parseError (cont, expected) = go cont
  where
    go [] = Left $ withParse "Expect more tokens"
    go ((L.SrcLoc {L.sLine=sLine, L.sColumn=sColumn}, tok) : xs) = 
      Left . withParse $
      printf "at line %s, col %s, token %s\nRest tokens: %s"
      (show sLine) (show sColumn) (show tok) (show (snd <$> xs))
      <> "\n" <>
      printf "Candidate tokens: %s"
      (show expected)
      
ifcase :: BoolExp -> [Stmt] -> Parser Stmt
ifcase bex v = case bex of BE d -> return $ SIf d v
                           QE d -> return $ QIf d v

solvePattern :: AExp -> ([AExp], [AExp])
solvePattern a = case a of AOp2 OMul x y -> let r = solvePattern y in 
                               case x of AOp2 Omega _ _ -> (fst r,x: snd r)
                                         _ -> (x: fst r, snd r)
                           AOp2 Omega _ _ -> ([], [a])
                           _ -> ([a],[])

omegaCase a b c = let (x,y) = solvePattern b in return $ SESpecEn a x y c
                     
                     
getFirst a =
  case a of DEq x y -> x
            DLt x y -> x
            DLe x y -> x
            DAnd x y -> getFirst x
    
{-
requireEnsures :: [Conds] -> Parser ([Exp'], [Exp'])
requireEnsures =
  foldr inner (return ([], [])) 
  where
    inner cond mrqens = do
      (rqs, ens) <- mrqens
      case cond of
        Ensures e  -> return (rqs, e : ens)
        Requires r -> return (r : rqs, ens)
        _          -> Left . err $ show cond
    err = printf "%s is not one of `requires` or `ensures`"


invariantSeperates :: [Conds] -> Parser ([Exp'], Maybe Partition)
invariantSeperates conds = do
  (invs, seps) <- foldr inner (return ([], [])) conds
  case seps of
    []    -> return (invs, Nothing)
    [sep] -> return (invs, Just sep)
    _     -> Left . errSep $ show seps
  where
    inner cond mrqens = do
      (ins, ses) <- mrqens
      case cond of
        Invariants i  -> return (i : ins, ses)
        Separates  s  -> return (ins, s : ses)
        _             -> Left $ err (show cond)
    err = printf "%s is not one of `invariant` or `separates`"
    errSep = printf "There should be exactly one `separates` condition, given %s."


unchainExps :: Exp' -> [(Op2, Exp')] -> Exp'
unchainExps eLeft [] = eLeft
unchainExps eLeft [(op2, eLast)] = EOp2 op2 eLeft eLast
unchainExps eLeft ((op2, eNow) : oes) = 
  EOp2 OAnd (EOp2 op2 eLeft eNow) (unchainExps eNow oes)
-}
