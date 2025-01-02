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

module Qafny.Syntax.Codegen where

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
--import           GHC.Generics          hiding
--    ((:+:))
--import           Text.Printf




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
-- if the fltag is period(r), it takes the period function f(x) = f(x+r), and partition the basis-kets into r blocks,
-- each block is labeled wih an l number as l in [0,r), it store the bitstring number kt+l, we can replace bitstring x in every place with kt+l.
-- the upEn function can be an operation in our system to allow users to input
-- once an upEn is inputed for a period function, when measurment according to |kt+l>|f(kt+l)>, we will get one possible l value such that the ket is outputed f(kt+l),
-- and the remainily ket structure has the form |kt+l> for one l. 
codegenStmt s@(xl :*=: measure l) = do
  env <- get @TEnv
  state <- get @TState
  let measuredLoci = findLoci env l
  case measuredLoci of
    Nothing -> throwError "Loci not found"
    Just loci -> do
      (newState, measuredValues) <- performMeasurement loci state
      put @TState newState
      let assignments = zipWith (\x v -> SAssign x (ELit (LInt v))) xl measuredValues
      return $ assignments ++ [SComment "Measurement performed"]
              




              

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