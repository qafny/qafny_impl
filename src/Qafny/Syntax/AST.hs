{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , NamedFieldPuns
  , PatternSynonyms
  , StandaloneDeriving
  , TemplateHaskell
  , TupleSections
  , TypeFamilies
  , TypeOperators
  , StrictData
  , UndecidableInstances
  #-}

module Qafny.Syntax.AST where

-- import           Qafny.TTG

--------------------------------------------------------------------------------

-- import           Data.Data
import           Control.Carrier.Reader
    (runReader)
import           Control.Carrier.State.Strict
    (runState)
import           Control.Effect.Lens
-- import           Qafny.Effect
import           Data.Functor.Foldable
    (Base, Corecursive, Recursive)
--import           Data.Functor.Identity
--import           Data.List.NonEmpty
--    (NonEmpty (..))
import           Data.Sum
import           Data.Map

--import Qafny.Syntax.Codegen
--import           GHC.Generics          hiding
--    ((:+:))
--import           Text.Printf
--    (printf)
--------------------------------------------------------------------------------

{-
data AExp
--  = ANat Int
  | AVar Var
  deriving (Show, Eq, Ord)

aexpToExp :: AExp -> Exp ()
aexpToExp (ANat i) = ENum i
aexpToExp (AVar v) = EVar v
-}
-- | The good thing about this design is that I don't need to modify the
-- emitState when creating a new phaseRef.
--
-- The only bad thing is that everytime
-- I want to get the pointer to the phase, I need to resolve through `sSt`,
-- which is not too bad because phase are designed to live with partitiions
-- instead of with ranges :)
--
-- TODO: move this into the proposal
--

data Op2
  = OAdd
  | ODiv
  | OSub
  | OMul
  | OMod
  | OPow
  | Omega
  | OInd
  | OXor
--  | OLe
--  | OGt
--  | OGe
--  | OEq
  deriving (Show, Eq, Ord-- , Data, Typeable
           )

data Op1
  = OSqrt
  | OSin
  | OCos
  | OLen
  deriving (Show, Eq, Ord-- , Data, Typeable
           )
           
data AExp
  = ANat Int
  | AVar Var
  | AHad Bool
  | AFun Var [AExp]
  | AOp2 Op2 AExp AExp
  | AOp1 Op1 AExp
  deriving (Show, Eq, Ord)
  
data Ty
  = TNat
  | TReal
  | TInt
  | TBool
  | TSeq Ty
  | TArrow [Ty] Ty
-- | TMeasured
  | TQReg AExp
  -- | TMethod [Ty] [Ty] -- parameter and return types
 -- | TEmit EmitTy
  deriving (Show, Eq, Ord)
  

data QTy
  = TNor
  | THad
  | TEn AExp
--  | TEn01
  | TAA
--  | TQft
  deriving (Show, Eq, Ord)

type Var = String

data Range = Range Var (AExp) (AExp)
  deriving (Show, Eq, Ord-- , Data, Typeable
           )

getRangeName :: Range -> Var
getRangeName (Range n _ _) = n

{-
showExp :: Exp -> String
showExp (ENum n) = show n
showExp (EVar v) = v
showExp e = show e

instance Show Range where
  show (Range x y z) = print "%s[%s .. %s]" x (showExp y) (showExp z)
  
-}

{-
--newtype PartitionT t = Partition { ranges :: t Range }
--type Partition = PartitionT []

--deriving instance Eq Partition
--deriving instance Ord Partition


--unpackPart :: Partition -> [Range]
--unpackPart = ranges

infixl 5 ::=:
infixl 5 :*=:
instance Show Partition where
  show = showPP . unpackPart
    where
      showPP []       = "∅"
      showPP (r : rs) = foldr (\r' s -> show r' ++ " ⊎ " ++ s) (show r) rs
-}
    
data DGuardExp
  = DEq AExp AExp -- guard partition with a split at
  | DLt AExp AExp
  | DLe AExp AExp
  | DNeg DGuardExp
  | DAnd DGuardExp DGuardExp
  | DOr DGuardExp DGuardExp
  | DImply DGuardExp DGuardExp
  | DFun Var [AExp]
  deriving (Show, Eq, Ord)
  
data GuardExp
  = GEPar Var AExp
  | GEq AExp AExp Var AExp
  --- GEPartition Partition (Maybe (Exp ())) -- guard partition with a split at
  | GLt AExp AExp Var AExp
  | GNeg GuardExp
 -- | Guard DGuardExp
 -- | EmptyGE
  deriving (Show, Eq, Ord)

data BoolExp
  = BE DGuardExp
  | QE GuardExp

--data SpecKet =
--  SpecKet { norVar :: Var -- ^ enumerator for each qubit
 --          , norKet :: AExp   -- ^ basis ket for each qubit
 --          }
 -- deriving (Functor, Foldable, Traversable)
data Intv = Intv AExp AExp
  deriving (Eq, Show, Ord-- , Data, Typeable
           )
           
data SpecExp
  = SESpecTen Var (Maybe Intv) [AExp]
    -- ^ `⊗ id . e`
 -- | SESpecHad Var AExp
    -- ^ `⊗ id . ω`
  | SESpecEn [(Var,Intv)] [AExp] [AExp] [AExp]
    -- ^ `Σ id ∈ intv . amp . ω ~ e`
  | SESpecAA GuardExp AExp AExp
    -- ^ `Σ id1 ∈ intv1 . ⊗ id2 . ω ~ e`
--  | SEWildcard
    -- ^ `_`
  deriving (
  --Functor, Foldable, Traversable, 
  Show, Eq, Ord)

--deriving instance Generic (SpecExpF f)
--deriving instance (Show f) => Show (SpecExpF f)
--deriving instance (Eq f) => Eq (SpecExpF f)
--deriving instance (Ord f) => Ord (SpecExpF f)
--type SpecExp = SpecExpF Exp'

data Binding = Binding Var Ty
  deriving (Show, Eq, Ord)

data PredExp
  = DPred DGuardExp
  | EForall Binding (Maybe DGuardExp) DGuardExp
  | ESpec [Range] QTy SpecExp
  deriving (Show, Eq, Ord)
  
-- the exp is not reversible
data Exp
  = EHad
  | EQft Bool
  | ELambda [Var] (Maybe AExp) [AExp]
  --ENum Int
  -- | EVar Var
 -- | ELen Var
--  | EInd Var Var
--  | EWildcard
--  | EHad
--  | EQft Bool
--  | EMeasure Partition
--  | EBool Bool
--  | EApp Var [XRec x (Exp x)]
-- | EOp1 Op1 (XRec x (Exp x))
 -- | EOp2 Op2 (XRec x (Exp x)) (XRec x (Exp x))
-- | EForall (Binding x) (Maybe (XRec x (Exp x))) (XRec x (Exp x))
--  | ECPec (Maybe (XRec x (Exp x))) (XRec x (Exp x))
  | EDafny String
--  | EEmit EmitExp
--  | ERange Range
--  | ESpec Partition QTy [SpecExpF (XRec x (Exp x))]
--  | ERepr Range
--  | ELambda (LambdaF (XRec x (Exp x)))
    deriving (Show, Eq, Ord)
    
data Stmt where
  SAssert :: PredExp -> Stmt
  SCall :: Var -> [AExp] -> Stmt
  SVar :: Binding -> Maybe AExp -> Stmt
  SCast :: [Range] -> QTy -> Stmt
  (::=:) :: Var -> AExp -> Stmt
  (:*=:) :: [Range] -> Exp -> Stmt
  SMea :: Var -> [Var] -> [Range] -> Stmt
  SDafny :: String -> Stmt
  SIf :: DGuardExp -> [Stmt] -> Stmt
  QIf :: GuardExp -> [Stmt] -> Stmt
  -- TODO: Refactor 'For' with a record
  --     id      left    right     guard       invarants             separates Body
  SFor :: Var -> AExp -> AExp -> [PredExp] -> [Stmt] -> Stmt
  --SEmit :: EmitStmt -> Stmt
  deriving (Show, Eq, Ord)
  
--deriving instance Show (Stmt ())
--deriving instance Show (Stmt Source)
--deriving instance Eq (Stmt ())
--deriving instance Eq (Stmt Source)

data QMethod
  = QMethod { qmName     :: Var
            , qmInputs   :: [Binding]
            , qmOutputs  :: [Binding]
            , qmRequires :: [PredExp]
            , qmEnsures  :: [PredExp]
            , qmBody     :: [Stmt]
            }
    deriving (Show, Eq, Ord)

type AST = [QMethod]
  
data DStmt where
  DAssert :: DGuardExp -> DStmt
  DCall :: Var -> [AExp] -> DStmt
  --SVar :: (XRec x (Binding x)) -> (Maybe (XRec x (Exp x))) -> Stmt x
  DAssign :: Var -> AExp -> DStmt
 -- (:*=:) :: Partition -> (XRec x (Exp x)) -> Stmt x
 -- SMea :: Var -> Var -> (XRec x (Exp x)) -> Stmt x
  DDafny :: String -> DStmt
  DIf :: DGuardExp -> [DStmt] -> DStmt
  -- TODO: Refactor 'For' with a record
  --     id      left                right               guard       invarants             separates Body
 -- SFor :: Var -> (XRec x (Exp x)) -> (XRec x (Exp x)) -> GuardExp -> [(XRec x (Exp x))] -> Maybe Partition -> (Block x) -> Stmt x
 -- SEmit :: EmitStmt -> Stmt x
  deriving  (Show, Eq)


--type Stmt' = Stmt ()
--type Exp' = Exp ()
-- type Binding' = Binding ()
type TEnv = [([Range],QTy)]
type TState = [GuardExp]  --genereating list of predicates for the current state.
type KEnv = Map Var Ty

maxEqual x a y b = x == a && y == b
oneEqual x y = y == AOp2 OAdd x (ANat 1)



-- DONE: write a function to simplify AExp, combining constant terms, such as AOp2 OAdd (ANat n) (ANat m) = ANat (n+m)
simpAExp (AOp2 OAdd (ANat n) (ANat m)) = ANat (n + m)
simpAExp (AOp2 OSub (ANat n) (ANat m)) = ANat (n - m)
simpAExp (AOp2 OMul (ANat n) (ANat m)) = ANat (n * m)
simpAExp (AOp2 ODiv (ANat n) (ANat m)) = ANat (n `div` m)
simpAExp (AOp2 OMod (ANat n) (ANat m)) = ANat (n `mod` m)
simpAExp (AOp2 OPow (ANat n) (ANat m)) = ANat (n ^ m)
--simpAExp (AOp2 OXor (ANat n) (ANat m)) = ANat (xor n m)
simpAExp x = x -- in the end, if an expression cannot be simplified further, it stays the same.


-- Finished
findRange [] x a = False
findRnage ((Range y c d):l) x a = if x == y && (simpAExp c) == a then True else findRange l x a

findLocus [] x a = Nothing
findLocus ((xl,t):l) x a = if findRange xl x a then Just (xl,t) else findLocus l x a

-- TODO: find state, input a list of pair [(l,s)], and then search for l, and ret s.
-- ... existing code ...

-- DONE?: find state, input a list of pair [(l,s)], and then search for l, and ret s.
findState :: Eq l => [(l, s)] -> l -> Maybe s
findState [] _ = Nothing
findState ((l', s):xs) l = if l == l' then Just s else findState xs l

-- ... existing code ...


{-
-- TODO: addSum will take in a state v with the form sum_j sum_k ... sum_d B_{j,k,d,...}, and then do sum_j sum_k ... sum_d sum_{da} omega(old(k) * B_{j,k,...,d,da}
-- we first need to generate a new fresh variable da, and then for each basis-ket B_{j,k,...,d,da}, you will multiple omega(old(k) * da, 2^ (b - a)), 
-- and then in the x[a,b) place in the basis-state B_{j,k,...,d}, we replace it with da.
-- addSum v x a b = 



-- TODO: 
collectVars [] = []
collectVars (Range x a b:xl) = x:(collectVars xl)

subList [] l = True
subList (x:xl) l = elem x l && subList xl l

collectLocus [] l = []
collectLocus ((xl,v):s) l = if subList l (collectVars xl) then (xl,v):collectLocus s l else collectLocus s l

-- DONE: check if the locus xl is a sub-piece in l. like x[i,j) u y[a,b) u z[t,c) and xl = y[i] u z[t,c),
-- then we will say y[i] maps to y[a,b) provided that a <= i < b, and z[t,c) is mapped to the last piece.
checkLocus :: [Range] -> [Range] -> Bool
checkLocus l xl = all (`elem` l) xl && all validMapping xl
  where
    validMapping (Range var1 a1 b1) = any (\(Range var2 a2 b2) -> var1 == var2 && a2 <= a1 && b1 <= b2) l


findGoodLocus [] xl = Nothing
findGoodLocus ((x,v):l) xl = if checkLocus x xl then Just (x,v) else findGoodLocus l xl

-- DONE: find locus in env in e (e is a list of stmt), stmt that can appear inside qif is SCall, QIf, (::=:), and (:*=:). 
-- checkLocus l xl

codegenStmt
  :: ( Has (Reader KEnv) sig m -- we only need Kind env, not need tenv
     , Has (Reader TEnv) sig m -- type env
     , Has (State TState)  sig m -- 
     , Has (Error Builder) sig m
     , Has Trace sig m
     )
  => Stmt
  -> m [DStmt]
{-
codegenStmt s@([Range x a b] :*=: EHad) =
       do TQReg v <- runReader @KEnv x
          env <- get @TEnv -- get the env
          state   <- get @TState -- first deal with H when it is typed as Nor
          let a' = simpAExp a
          let b' = simpAExp b
          let l = findLocus env x a'
          case l of Nothing -> -- TODO: throw an error
                    Just (xl,t) ->
                case t of TNor | THad -> -- TODO: if t is of type TNor and THad, then xl should only be in the form x[a',d), 
                                         -- we need to cut it into x[a', a'+1), and x[a'+1,d), if d == a'+1 then we can remove the cut, and we note that x[a', d) is at the end of list
                          TEn d ->
                            let v = findState state xl -- v is in the form of sum_j sum_k sum_d ... B_{j,k,d,...}
                            ret TEn (d+1), addSum v x a b
                          -- TODO: type of l is moved to TEn (d+1), return the type of xl to be TEN (d+1), and TState to in the form of sum(d+1) omega(old(k) * k,2^(b'-a')) |k>

-}


codegenStmt s@([Range x a b] :*=: EHad) = do
  env <- get @TEnv -- get the env
  state <- get @TState -- first deal with H when it is typed as Nor
  let a' = simpAExp a
  let b' = simpAExp b
  let l = findLocus env x a'
  case l of
    Nothing -> throwError "Locus not found"
    Just (xl, t) -> case t of
      TNor | THad -> 
        let v = findState state xl 
        in return (TEn 1, addSum v x a b)
      TEn d ->
        let v = findState state xl 
        in return (TEn (d + 1), addSum v x a b)













{-

codegenStmt s@(l :*=: ELambda xl q vl) =
       if length l == length xl then
       do TQReg v <- runReader @KEnv x
          env <- get @TEnv -- get the env
          state   <- get @TState -- first deal with H when it is typed as Nor
          ll <- collectLocus env (collectVars l) --collect the locus in env that might contain the locus l
          case findGoodLocus ll l of 
            Nothing -> --throw errors
            Just (xv,t) -> 
            let lxl = zip l xl
              case t of TNor | TEn d -> --TODO: divide lxl into different ranges and variable (x[a,b),y), 
                                         -- for each range, we assume that one can find an seq corresponding to x[a,b),
                                         -- we use its corresponding variable y in vl as aexp expr v(y) in vl, then
                                         -- we replace the state seq corresponds to x[a,b) with the v(y)
                                         -- we do this for every pair in lxl
              case t of THad -> -- do the same as above, but now, we will first cast the state from THad to TEn 1
          
      else --throw errors
-}






codegenStmt s@(l :*=: ELambda xl q vl) =
       if length l == length xl then
       do TQReg v <- runReader @KEnv x
          env <- get @TEnv -- get the env
          state <- get @TState -- first deal with H when it is typed as Nor
          ll <- collectLocus env (collectVars l) -- collect the locus in env that might contain the locus l
          case findGoodLocus ll l of 
            Nothing -> error "Locus not found" -- throw errors
            Just (xv, t) -> 
              let lxl = zip l xl
              in case t of 
                TNor | TEn d -> mapM_ (updateStateWithVar vl) lxl
                THad -> do
                  -- Cast the state from THad to TEn 1
                  let newState = map (\(Range var a b) -> (Range var a b, TEn 1)) l
                  put @TState (newState ++ state)
                  mapM_ (updateStateWithVar vl) lxl
       else error "Length mismatch between l and xl" -- throw errors

updateStateWithVar :: [AExp] -> (Range, Var) -> m ()
updateStateWithVar vl (Range x a b, y) = do
  let expr = lookupVar y vl
  modify @TState (map (\(Range var a' b', t) -> if var == x && a' == a && b' == b then (Range var a' b', expr) else (Range var a' b', t)))

lookupVar :: Var -> [AExp] -> AExp
lookupVar y vl = case lookup y (zip (map show [0..]) vl) of
  Just expr -> expr
  Nothing -> error $ y ++ " not found"



{-
--TODO
codegenStmt s@(QIf (GEPar x a) e) =
         env <- get @TEnv -- get the env
         let ra = Range x a (AOp2 OAdd a (ANat 1)) -- construct x[a,a+1)
           -- we first need to discover the locus mentioned in e as l
           -- we then find the locus l' in env containing ra, we then join l and l' as l ++ l' as la
           -- we have a pre-define function pattern in Dafny for conditional.
           -- we first find the locus la's range in env for ra, 
           -- for each basis-ket in the state of ra, if the bit is 0, then for every other basis-kets in the locus, the result state is the same
           -- otherwise, if the bit is 1, for every other basis-kets in the locus, we recursively apply e to the locus.
        -}

codegenStmt s@(QIf (GEPar x a) e) = do
  env <- get @TEnv
  let ra = Range x a (AOp2 OAdd a (ANat 1))
  let l = collectVars e 
  let l' = findLocus env x (simpAExp a)
  case l' of
    Nothing -> throwError "Locus not found in environment"
    Just (xl, t) -> do
      let la = xl ++ l
      let conditionalPattern = \state -> 
            case state of
              (Range var a' b', guard) -> 
                if isZeroBit (Range var a' b')
                then (Range var a' b', guard)
                else (Range var a' b', applyExp e guard)
      let raRange = findRange la x (simpAExp a)      let raRange = findRange la x (simpAExp a)
      case raRange of
        Nothing -> throwError "Range not found in locus"
        Just _ -> do
          state <- get @TState
          let newState = map (applyConditional e) state
          put @TState newState
          return [s]
  where
    applyConditional :: [Stmt] -> (Range, GuardExp) -> (Range, GuardExp)
    applyConditional e (range, guard) =
      if isZeroBit range
      then (range, guard)
      else (range, applyExp e guard)

    isZeroBit :: Range -> Bool
    isZeroBit (Range _ _ (ANat 0)) = True
    isZeroBit _ = False

    applyExp :: [Stmt] -> GuardExp -> GuardExp
    applyExp e guard = undefined 

      
      

--TODO
-- include a new operation upEn(partition, flag)
-- for any en(n) type, it pushes the type to en(n+1), and then we set up the flag to smartly modify the state.
-- if the flag is period(r), it takes the period function f(x) = f(x+r), and partition the basis-kets into r blocks,
-- each block is labeled with an l number as l in [0,r), it store the bitstring number kt+l, we can replace bitstring x in every place with kt+l.
-- the upEn function can be an operation in our system to allow users to input
-- once an upEn is inputed for a period function, when measurment according to |kt+l>|f(kt+l)>, we will get one possible l value such that the ket is outputed f(kt+l),
-- and the remainily ket structure has the form |kt+l> for one l. 
codegenStmt s@(xl *= measure(l)) =
         env <- get @TEnv -- get the env
              

codegenStmt' s@(SVar (Binding v t) Nothing)  = return [s]
codegenStmt' s@(SVar (Binding v t) (Just e)) = do
  te <- typingExp e
  -- check if `t` agrees with the type of `e`
  checkSubtype t te
  codegenAlloc v e t <&> (: [])

codegenStmt' s@(_ :*=: _) =
  codegenStmt'Apply s

codegenStmt' (SIf e seps b) = do
  -- resolve the type of the guard
  (stG'@Locus{qty=qtG, degrees=ptysG}, pG) <- typingGuard e
  -- perform a split to separate the focused guard range from parition
  (stG, maySplit) <- splitSchemePartition stG' pG
  let stmtsSplit = codegenSplitEmitMaybe maySplit
  -- resolve and collect partitions in the body of the IfStmt
  -- analogous to what we do in SepLogic
  stB'@Locus{part=sB, qty=qtB, degrees=ptysB} <-
    resolvePartitions . leftPartitions . inBlock $ b
  let annotateCastB = qComment $
        printf "Cast Body Partition %s => %s" (show qtB) (show TEn)
  (stmtsCastB, stB) <- case qtB of
    TEn -> return ([], stB')
    _   ->
      (,) . (annotateCastB :)
      <$> castPartitionEN stB' <*> resolvePartition (denorm sB)
  -- act based on the type of the guard
  stmts <- case qtG of
    THad    -> codegenStmt'If'Had stG stB b
    _nothad -> undefined
  return $ stmtsSplit ++ stmtsCastB ++ stmts

codegenStmt' s@(SFor {}) =
  codegenStmt'For s

codegenStmt' (SAssert e) =
  (SAssert <$>) <$> codegenAssertion e










-- TODO: Handle arguments in the method call in one pass to codegen Repr.
codegenStmt' (SCall x eargs) = do
  mtyMaybe <- asks @TEnv (^. kEnv . at x) <&> (>>= projMethodTy)
  mty <- maybe errNoAMethod return mtyMaybe
  (rMap, pureArgs, qArgs, schemes, loci) <-
    resolveMethodApplicationArgs eargs mty
  stmtsSC <- forM schemes $ uncurry codegenSplitThenCastEmit
  -- because locis has been called, their type are no longer accurate. the
  -- actual type is then extracted from the method's `ensures` clauses
  forM_ loci removeTStateByLocus
  rets <- resolveMethodApplicationRets rMap mty
  pure $ concat stmtsSC ++
    [SEmit (fsts rets :*:=: [EEmit (ECall x (pureArgs ++ qArgs))])]
  where
    errNoAMethod = throwError' $
      "The variable"<+>x<+>"is not referring to a method."

codegenStmt' s@(SDafny {}) = return [s]

codegenStmt' s = error $ "Unimplemented:\n\t" ++ show s ++ "\n"


-}









