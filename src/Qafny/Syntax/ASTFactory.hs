module Qafny.Syntax.ASTFactory where

import           Qafny.Analysis.Partial
    (reduce)
import           Qafny.Syntax.AST


-- | AstInjection makes ast construction easier.
class AstInjection a b where
  injAst :: a -> b

instance AstInjection Exp' Exp' where
  injAst = id

instance AstInjection Var Exp' where
  injAst = EVar

instance AstInjection EmitExp Exp' where
  injAst = EEmit

instance AstInjection Stmt' Stmt' where
  injAst = id

instance AstInjection EmitStmt Stmt' where
  injAst = SEmit


infixl 6 >+
(>+) :: (AstInjection a Exp', AstInjection b Exp') => a -> b -> Exp'
e1 >+ e2 = injAst e1 + injAst e2

infixl 7 >*
(>*) :: (AstInjection a Exp', AstInjection b Exp') => a -> b -> Exp'
e1 >* e2 = injAst e1 * injAst e2

--------------------------------------------------------------------------------
-- | AST Constants
-------------------------------------------------------------------------------
wild :: Var
wild =  "_"

-- | An Oracle term that always returns a constant.
constLambda :: (AstInjection a Exp') => a -> Exp'
constLambda = simpleLambda wild . injAst

qComment :: String -> Stmt'
qComment = SDafny . ("// " ++)

-- | An Oracle term w/o phase specification.
simpleLambda :: Var -> Exp' -> Exp'
simpleLambda v e = ELambda $
  LambdaF PhaseWildCard [v] PhaseWildCard [e]

multiLambda :: [Var] -> Exp' -> Exp'
multiLambda v = EEmit . EMultiLambda v


--------------------------------------------------------------------------------
-- * Special Constructors
--------------------------------------------------------------------------------
natB :: Var -> Binding'
natB = (`Binding` TNat)

-- | Construct an interval guard over a variable
-- @
--   l <= x < r
-- @
eIntv :: Var -> Exp' -> Exp' -> Exp'
eIntv x l r = EEmit (EOpChained l [(OLe, EVar x), (OLt, r)])

predFromIntv :: Intv -> Var -> Exp'
predFromIntv (Intv l r) v = eIntv v (reduce l) (reduce r) 

ands :: [Exp'] -> Exp'
ands []       = EBool True
ands (x : xs) = EEmit (EOpChained x [ (OAnd, x') | x' <- xs ])

-- | Make a chained, left-associated expression
--
-- This is done in different fashion from 'ands' because the constant fold
-- doesn't work with the 'EmitExp's.
--
joinArith :: Op2 -> Exp' -> [Exp'] -> Exp'
joinArith _ e [] = e
joinArith op _ (x : xs) =
  foldr inner id xs x
  where
    inner y f = f . (EOp2 op `flip` y)

infixl >:@:
-- | Construct "e1 [ e2 ]"
(>:@:) :: (AstInjection a Exp', AstInjection b Exp')
       => a -> b -> Exp'
(>:@:) e1 e2 = injAst (reduce (injAst e1) :@: reduce (injAst e2))

infix 4 `eEq`
eEq :: (AstInjection a Exp', AstInjection b Exp')
    => a -> b -> Exp'
eEq a b = EOp2 OEq (injAst a) (injAst b)

infixl 6 >:@@:
-- | Construct "e1 [ e2 .. e3 ]"
(>:@@:) :: (AstInjection a Exp', AstInjection b Exp', AstInjection c Exp')
        => a -> (b, c) -> Exp'
(>:@@:) e1 (e2, e3) = injAst $
  injAst e1 :@@: (reduce (injAst e2),  reduce (injAst e3))


infixr 1 >$
(>$) :: (AstInjection b Exp') => Var -> b -> Exp'
f >$ e = injAst $ f `ECall` [injAst e]

infixr 1 >$*
(>$*) :: (AstInjection b Exp') => Var -> [b] -> Exp'
f >$* es = injAst $ f `ECall` (injAst <$> es)


callMap :: (AstInjection a Exp', AstInjection b Exp')
        => a -> b -> Exp'
callMap f e = injAst $ ECall "Map" [injAst f, injAst e]

callMaps :: Exp' -> [Exp'] -> Exp'
callMaps f es = EEmit (ECall "Map" (f : es))


mkCard :: AstInjection a Exp' => a -> Exp'
mkCard = injAst . ECard . injAst

rangeSize :: Range -> Exp'
rangeSize (Range _ l r) = reduce (r - l)

(>::=:) ::  (AstInjection a Exp') => Var -> a -> Stmt'
v1 >::=: v2 = v1 ::=: injAst v2

mkAssignment ::  Var -> Var -> Stmt'
mkAssignment v1 v2 = v1 ::=: EVar v2

mkDeclAssign :: Var -> Ty -> Var -> Stmt'
mkDeclAssign v1 t v2 = SVar (Binding v1 t) (Just (EVar v2))


-- Erase phase arguments in a lambda term
lambdaUnphase :: Lambda -> Exp'
lambdaUnphase l = ELambda
  l{ bPhase = PhaseWildCard, ePhase = PhaseWildCard }

tySr :: Ty
tySr = TSeq TReal

tySn :: Ty
tySn = TSeq TNat

tySsn :: Ty
tySsn = TSeq (TSeq TNat)

natSeqLike :: (AstInjection a Exp') => a -> Exp' -> Exp'
natSeqLike = seqLike TNat

seqLike :: (AstInjection a Exp') => Ty -> a -> Exp' -> Exp'
seqLike ty liked = EEmit . EMakeSeq ty (EEmit (ECard (injAst liked)))

-- | Construct a sequence
mkSeqConst :: (AstInjection a Exp', AstInjection b Exp') => Ty -> a -> b -> Exp'
mkSeqConst ty cnt content =
  injAst $ EMakeSeq ty (injAst cnt) (constLambda content)


mkPow2 :: (AstInjection a Exp') => a -> Exp'
mkPow2 e = injAst $ ECall "Pow2" [injAst e]

-- | "forall x : nat | P(x) :: v[x] == e"
mkForallEq :: Var -> (Var -> Exp') -> Var -> Exp' -> Exp'
mkForallEq x p v e = EForall (Binding x TNat) (Just (p x)) ((v >:@: x) `eEq` e)

-- | "forall x : nat | P(x) :: |v[x]| == e"
mkForallCardEq :: Var -> (Var -> Exp') -> Var -> Exp' -> Exp'
mkForallCardEq x p v e =
  EForall (Binding x TNat) (Just (p x)) (mkCard (v >:@: x) `eEq` e)

-- | "forall x : t1 | P(x) :: forall y : t2 | Q(y) :: v[x][y] == e"
mkForallEq2
  :: Var -> (Var -> Exp')
  -> Var -> (Var -> Exp')
  -> Var -> Exp' -> Exp'
mkForallEq2 x p y q v e =
  EForall (Binding x TNat) (Just (p x)) $
  EForall (Binding y TNat) (Just (q y))
  ((v >:@: x >:@: y) `eEq` e)



(>//) :: (AstInjection a Exp', AstInjection b Exp') => a -> b -> Exp'
e1 >// e2 =
  EOp2 ODiv (injAst (EAsReal (injAst e1))) (injAst (EAsReal (injAst e2)))
