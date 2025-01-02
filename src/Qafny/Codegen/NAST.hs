{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiWayIf
  , NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
  #-}

module Qafny.Codegen.NCodegen (codegenAST) where
-- | Code Generation through Fused Effects

-- Effects
import qualified Control.Carrier.Error.Church as ErrC
import qualified Control.Carrier.Error.Either as ErrE
import           Control.Carrier.Reader
    (runReader)
import           Control.Carrier.State.Strict
    (runState)
import           Control.Effect.Lens
import           Qafny.Effect

-- Handlers
import qualified Carrier.Gensym.Emit          as GEmit
import           Qafny.Gensym
    (resumeGensym)

-- Utils
import           Control.Lens
    (at, non, (%~), (?~), (^.))
import           Control.Monad
    (forM_, void)
import           Qafny.Utils.Common

import           Data.List.NonEmpty
    (NonEmpty (..))
import qualified Data.Map.Strict              as Map
import           Data.Maybe
    (catMaybes, mapMaybe)
import qualified Data.Sum                     as Sum
import           Text.Printf
    (printf)

-- Qafny
import           Qafny.Codegen.Utils
    (runWithCallStack)

import           Qafny.Analysis.Interval
    (Interval (Interval))
import           Qafny.Analysis.Partial
    (Reducible (reduce))
import           Qafny.Config
import           Qafny.Syntax.AST
import           Qafny.Syntax.ASTFactory
import           Qafny.Syntax.Emit
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Typing
    (appkEnvWithBds, castScheme, checkSubtype, collectConstraints,
    mergeCandidateHad, mergeMatchedTState, mergeScheme, removeTStateByLocus,
    resolvePartition, resolvePartitions, splitScheme, splitSchemePartition,
    splitThenCastScheme, tStateFromPartitionQTys, typingExp, typingGuard,
    typingPartition)
import           Qafny.Typing.Utils
    (emitTypeFromDegree, isEn)

import           Data.Sum
    (Injection (inj))
import           Data.Tuple
    (swap)
import           Qafny.Analysis.Normalize
    (Normalizable (normalize))
import           Qafny.Codegen.Common
    (codegenAssignEmitData, codegenAssignEmitDataF)
import           Qafny.Codegen.Had
    (codegenNorToHad)
import           Qafny.Codegen.Lambda
import           Qafny.Codegen.Merge
    (codegenMergeScheme)
import           Qafny.Codegen.Method
    (codegenMethodParams, codegenMethodReturns, genEmitSt)
import           Qafny.Codegen.Phase
    (codegenApplyQft)
import           Qafny.Codegen.Predicates
    (codegenAssertion, codegenEnsures, codegenRequires)
import           Qafny.Codegen.SplitCast      hiding
    (throwError')
import           Qafny.Syntax.Subst
import           Qafny.Typing.Error
import           Qafny.Typing.Method
    (collectMethodTypes, resolveMethodApplicationArgs,
    resolveMethodApplicationRets)
import           Qafny.Typing.Predicates
    (dropSignatureSpecs, wfSignatureFromPredicates)
import           Qafny.Typing.Typing
    (matchLocusEmitData, matchLocusEmitDataFromTStates)
import           Qafny.Utils.EmitBinding
import           Qafny.Utils.TraceF
    (Traceable (tracef))
import           Qafny.Utils.Utils
    (both, bothM, dumpSt, gensymLoc, getMethodType, tracep)

--------------------------------------------------------------------------------
-- * Introduction
-- $doc
-- Qafny-to-Dafny translation is organized into two stages:
--
--   * Type Inference/Checking
--   * Code Generation
--
-- The 'Qafny.Codegen' module implements the translation and is responsible of
-- calling typing functions to provide hints and perform type checkings.
-- $doc
--------------------------------------------------------------------------------
throwError'
  :: ( Has (Error Builder) sig m, DafnyPrinter s )
  => s -> m a
throwError' = throwError . ("[Codegen]" <+>)


--------------------------------------------------------------------------------
-- * AST
--------------------------------------------------------------------------------

data AExp
  = ANat Int
  | AVar Var
  | AAdd AExp AExp
  | AMul AExp AExp
  | ADiv AExp AExp
  | APow AExp AExp
  | ASqrt AExp
  | ASin AExp
  | ACos AExp
  | AOmega AExp AExp
  deriving (Show, Eq, Ord)
  
data Ty
  = TNat
  | TReal
  | TInt
  | TBool
  | TSeq Ty
  | TArrow [Ty] Ty
  | TMeasured
  | TQReg AExp
  -- | TMethod [Ty] [Ty] -- parameter and return types
  | TEmit EmitTy
  deriving (Show, Eq, Ord)
  

data QTy
  = TNor
  | THad
  | TEn Int
  | TEn01
  | TAA
  | TQft
  deriving (Show, Eq, Ord)

type Var = String

data DGuardExp
  = DEq AExp AExp -- guard partition with a split at
  | DLt AExp AExp
  | DNeg DGuardExp
  deriving (Show, Eq)
  
data GuardExp
  = GEPar Var AExp
  | GEq AExp AExp 
  --- GEPartition Partition (Maybe (Exp ())) -- guard partition with a split at
  | Guard DGuardExp
  | EmptyGE
  deriving (Show, Eq)

data SpecExpF f
  = SESpecNor (SpecNorF f)
    -- ^ `⊗ id . e`
  | SESpecHad (SpecHadF f)
    -- ^ `⊗ id . ω`
  | SESpecEn (SpecEnF f)
    -- ^ `Σ id ∈ intv . ω ~ e`
  | SESpecEn01 (SpecEn01F f)
    -- ^ `Σ id1 ∈ intv1 . ⊗ id2 . ω ~ e`
  | SEWildcard
    -- ^ `_`
  deriving (Functor, Foldable, Traversable)

deriving instance Generic (SpecExpF f)
deriving instance (Show f) => Show (SpecExpF f)
deriving instance (Eq f) => Eq (SpecExpF f)
deriving instance (Ord f) => Ord (SpecExpF f)
type SpecExp = SpecExpF Exp'

data Binding x = Binding (XRec x Var) Ty

data PredExp
  = DPred DGuardExp
  | EForall (Binding x) (Maybe (XRec x (PredExp x))) (XRec x (PredExp x))
  | ESpec Partition QTy [SpecExpF (XRec x (Exp x))]

-- the exp is not reversible
data Exp x
  = EHad
  | EQft Bool
  | ELambda (LambdaF (XRec x (Exp x)))
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
  | EOp1 Op1 (XRec x (Exp x))
  | EOp2 Op2 (XRec x (Exp x)) (XRec x (Exp x))
-- | EForall (Binding x) (Maybe (XRec x (Exp x))) (XRec x (Exp x))
--  | ECPec (Maybe (XRec x (Exp x))) (XRec x (Exp x))
  | EDafny String
--  | EEmit EmitExp
--  | ERange Range
--  | ESpec Partition QTy [SpecExpF (XRec x (Exp x))]
--  | ERepr Range
--  | ELambda (LambdaF (XRec x (Exp x)))
  
data Stmt x where
  SAssert :: PredExp -> Stmt x
  SCall :: Var -> [AExp] -> Stmt x
  SVar :: (XRec x (Binding x)) -> AExp -> Stmt x
  (::=:) :: Var -> (XRec x (Exp x)) -> Stmt x
  (:*=:) :: Partition -> (XRec x (Exp x)) -> Stmt x
  SMea :: Var -> Var -> (XRec x (Exp x)) -> Stmt x
  SDafny :: String -> Stmt x
  SIf :: GuardExp -> Partition -> [Stmt x] -> Stmt x
  -- TODO: Refactor 'For' with a record
  --     id      left    right     guard       invarants             separates Body
  SFor :: Var -> AExp -> AExp -> GuardExp -> [PredExp] -> [Stmt x] -> Stmt x
  SEmit :: EmitStmt -> Stmt x

deriving instance Show (Stmt ())
deriving instance Show (Stmt Source)
deriving instance Eq (Stmt ())
deriving instance Eq (Stmt Source)



  
data DStmt where
  DAssert :: AExp -> DStmt
  DCall :: Var -> [AExp] -> DStmt
  --SVar :: (XRec x (Binding x)) -> (Maybe (XRec x (Exp x))) -> Stmt x
  DAssign :: Var -> AExp -> DStmt
 -- (:*=:) :: Partition -> (XRec x (Exp x)) -> Stmt x
 -- SMea :: Var -> Var -> (XRec x (Exp x)) -> Stmt x
  DDafny :: String -> Stmt x
  DIf :: DGuardExp -> [DStmt] -> DStmt
  -- TODO: Refactor 'For' with a record
  --     id      left                right               guard       invarants             separates Body
 -- SFor :: Var -> (XRec x (Exp x)) -> (XRec x (Exp x)) -> GuardExp -> [(XRec x (Exp x))] -> Maybe Partition -> (Block x) -> Stmt x
 -- SEmit :: EmitStmt -> Stmt x
  deriving  (Show, Eq)


type Stmt' = Stmt ()
type Exp' = Exp ()
type Binding' = Binding ()
type TEnv = [([Range],QTy)]
type TState = [GuardExp]  --genereating list of predicates for the current state.