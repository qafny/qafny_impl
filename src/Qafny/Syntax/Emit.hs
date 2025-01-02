{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  #-}

module Qafny.Syntax.Emit where

-- Text
import qualified Data.Text                as TS
import qualified Data.Text.Lazy           as TL

-- Qafny
import           Qafny.Syntax.AST
import           Qafny.Syntax.EmitBinding
import           Qafny.Syntax.IR
import           Qafny.Syntax.Render
-- PP
import           Prettyprinter
    (lbrace, rbrace, space)
import qualified Prettyprinter            as P

import           Data.Bifunctor
    (Bifunctor (second))
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map.Strict          as Map
import           Data.Maybe
    (maybeToList)
import           Data.Sum
import           Qafny.Analysis.Interval
    (Interval (..))

-------------------- Builder --------------------
type Builder = P.Doc TS.Text

align :: DafnyPrinter a => a -> Builder
align = P.align . pp

viaShow :: Show e => e -> Builder
viaShow = P.viaShow

line :: Builder
line = P.line

incr2 :: DafnyPrinter a => a -> Builder
incr2 = P.indent 2 . pp

incr4 :: DafnyPrinter a => a -> Builder
incr4 = P.indent 4 . pp


indent :: DafnyPrinter a => Int -> a -> Builder
indent i = P.indent i . pp


vsep :: DafnyPrinter a => [a] -> Builder
vsep = P.vsep . (pp <$>)

encloseSep
  :: (DafnyPrinter delim, DafnyPrinter sep, DafnyPrinter body)
  => delim -> delim -> sep -> [body] -> Builder
encloseSep l r s bs =
  P.encloseSep (pp l) (pp r) (pp s) (pp <$> bs)

parens :: DafnyPrinter a => a -> Builder
parens = P.parens . pp

brackets :: DafnyPrinter a => a -> Builder
brackets = P.brackets . pp

braces :: DafnyPrinter a => a -> Builder
braces = P.braces . pp

byComma :: DafnyPrinter a => [a] -> Builder
byComma = P.cat . P.punctuate P.comma . (pp <$>)

list :: DafnyPrinter a => [a] -> Builder
list = P.list . (pp <$>)

tupled :: DafnyPrinter a => [a] -> Builder
tupled = P.tupled . (pp <$>)

debugOnly' :: DafnyPrinter a => a -> Builder
debugOnly' a = do
  P.annotate (TS.pack "/* (DEBUG) */") (pp a)

debugOnly :: (Show e, DafnyPrinter a) => e -> a -> Builder
debugOnly e a = do
  P.annotate (TS.pack "/* (DEBUG)" <> TS.pack (show e) <> TS.pack "*/") (pp a)

infixr 6 <!>
infixr 6 <+>
infixr 6 <%>

class DafnyPrinter a where
  pp :: a -> Builder

(<!>) :: (DafnyPrinter a, DafnyPrinter b) => a -> b -> Builder
a <!> b = pp a <> pp b
{-# INLINE (<!>) #-}

(<+>) :: (DafnyPrinter a, DafnyPrinter b) => a -> b -> Builder
a <+> b = pp a P.<+> pp b
{-# INLINE (<+>) #-}

(<%>) :: (DafnyPrinter a, DafnyPrinter b) => a -> b -> Builder
a <%> b = pp a <> P.flatAlt (line<>incr2 pb) (space<>pb)
  where pb = pp b

instance DafnyPrinter Builder where
  pp = id
  {-# INLINE pp #-}

instance DafnyPrinter Int where
  pp = P.pretty

instance DafnyPrinter Char where
  pp = P.pretty

instance DafnyPrinter String where
  pp = P.pretty

instance DafnyPrinter TS.Text where
  pp = P.pretty

instance DafnyPrinter TL.Text where
  pp = P.pretty

instance DafnyPrinter AST where
  pp ast = vsep ast <> line

instance DafnyPrinter Ty where
  pp TNat             = pp "nat"
  pp TInt             = pp "int"
  pp TBool            = pp "bool"
  pp (TArrow tys ty)  = tupled tys <+> "->" <+> ty
  pp TMeasured        = debugOnly' $ pp "measured"
  pp (TQReg n)        = debugOnly' $ "qreg" <+> n
  pp (TSeq ty)        = "seq<" <!> ty <!> ">"
  pp (TEmit (TAny s)) = pp s

instance DafnyPrinter MethodType where
  pp ty@MethodType {mtSrcParams=ts, mtSrcReturns=ts'} = debugOnly ty $
    tupled ts <+> "->" <+> tupled ts'

instance DafnyPrinter MethodElem where
  pp tt = debugOnly tt $ ppSub tt
    where
      ppSub (MTyPure x ty)   = debugOnly tt $ x <+> ":" <+> ty
      ppSub (MTyQuantum x e) = x <!> "[" <!> e <!> "]"

instance DafnyPrinter AExp where
  pp (ANat n) = pp n
  pp (AVar v) = pp v

instance DafnyPrinter QTy where
  pp TNor  = pp "nor"
  pp THad  = pp "had"
  pp TEn   = pp "en"
  pp TEn01 = pp "en01"
  pp TQft  = pp "qf"

instance DafnyPrinter (Binding ()) where
  pp (Binding x ty) = x <+> ":" <+> ty

instance DafnyPrinter QDafny where
  pp (QDafny s) = pp s

instance DafnyPrinter (QMethod ()) where
  pp (QMethod idt bds rets reqs ens blockMaybe) = vsep
    [ P.group $ "method" <+> idt <+> align (tupled bds) <%> ppRets rets
    , incr2 (vsep reqEns)
    , maybe mempty pp blockMaybe
    ]
    where
      ppRets [] = mempty
      ppRets x  = "returns" <+> align (tupled x)
      reqEns =
        (("requires" <+>) <$> reqs) <>
        (("ensures"  <+>) <$> ens)

instance DafnyPrinter (Block ()) where
  pp (Block b) = vsep
    [ lbrace
    , incr2 (vsep b)
    , rbrace]


instance (DafnyPrinter a, DafnyPrinter b) => DafnyPrinter (a :+: b) where
  pp (Inl a) = pp a
  pp (Inr b) = pp b

instance DafnyPrinter (Toplevel ()) where
  pp = pp . unTop

ppInvs :: [Exp'] -> Builder
ppInvs = vsep . (("invariant" <+>) <$> )

instance DafnyPrinter (Stmt ()) where
  pp (SEmit (SBlock b)) = pp b
  pp (SEmit f@(SForEmit idf initf bound invs b)) = vsep
    [ "for " <!> idf <+> ":=" <+> initf <!> " to " <!> bound
    , incr2 $ ppInvs invs
    , pp b
    ]
  pp (SDafny s') = pp s'

  pp s@(SFor idx boundl boundr eG invs seps body) = debugOnly s $ vsep
    [ "for" <+> idx <+> "∈" <+> brackets (boundl <+> ".." <+> boundr) <+>
      "with" <+> eG
    , incr2 $ vsep (pSep ++ pInvs)
    , pp body
    ]
    where
      pSep  = ("separates" <+>) <$> maybeToList seps
      pInvs = ("invariant" <+>) <$> invs

  -- Statements that end with a SemiColon
  pp s@(SIf eg sep block) = debugOnly s $ vsep
    [ "if" <+> parens eg
    , incr2 ("seperates" <+> sep)
    , pp block
    ]

  pp s = ppStmt s <> pp ';'
    where
      ppStmt :: Stmt' -> Builder
      ppStmt (SVar bd Nothing) = "var " <!> bd
      ppStmt (SVar bd (Just e)) = "var " <!> bd <+> ":=" <+> e
      ppStmt (v ::=: e) = P.group $ v <+> ":=" <%> e
      ppStmt (SCall v es) = v <!> tupled es
      ppStmt (SEmit s') = ppEmit s'
      ppStmt (SAssert e) = "assert " <!> e
      ppStmt (e1 :*=: e2) = debugOnly s $
        e1 <+> "*=" <+> λHuh e2
      ppStmt e = "// undefined pper for Stmt : " <!> viaShow e

      λHuh e@(ELambda {}) = "λ" <+> e
      λHuh e              = pp e

      ppEmit :: EmitStmt -> Builder
      ppEmit (SVars bds e) = "var" <+> byComma bds <+> ":=" <+> e
      ppEmit (vs :*:=: rhs)  = case rhs of
        []   -> mempty
        _rhs -> byComma vs <+> ":=" <+> byComma rhs
      -- ppEmit (SIfDafny e b) = "if " <!> withParen (pp e) <!> b
      ppEmit _             = error "Should have been handled!!"

instance DafnyPrinter GuardExp where
  pp (GEPartition p _) = pp p

instance DafnyPrinter (Exp ()) where
  pp (ENum n) = pp n
  pp (EVar v) = pp v
  pp (EBool b) = pp $ if b then "true" else "false"
  pp (EEmit e) = pp e
  pp (EOp1 ONeg e1) = "-" <+> e1
  pp (EOp2 op e1 e2) = ppOp2 op (pp e1) (pp e2)
  -- parentheses are critical to forall expressions!
  pp (EForall x eb e) =
    P.group . parens $ "forall " <!> x  <!> beb eb <+> "::" <%> e
    where
      beb (Just eb') = " | " <!> eb'
      beb Nothing    = mempty
  pp e@EHad = debugOnly e "H"
  pp e@(ESpec p qt specs) = debugOnly e . braces' . pp $
    (P.group (p <+> ":" <+> qt)
     <!> P.flatAlt (line<!>"  ") P.space <!> "↦"
     <!> line<!> P.group(byComma specs))
    where
      braces' x = P.group . vsep $
        [ P.lbrace
        , P.flatAlt (incr2 x) x
        , P.rbrace
        ]

  pp e@(EApp v es) = v <!> tupled es
  pp e@(EMeasure s) = debugOnly e $
    "measure" <+> s
  pp EWildcard = pp "_"
  pp (ELambda el) = pp el
  pp e = "//" <!> viaShow e <!> " should not be in emitted form!"

instance (DafnyPrinter f) => DafnyPrinter (LambdaF f) where
  pp e@(LambdaF{ bPhase, bBases, ePhase, eBases }) =
    case (bPhase, ePhase) of
      (PhaseWildCard, PhaseWildCard) ->
        P.group $ tupled bBases <+> "=>" <%> align (tupled eBases)
      (_, _) -> debugOnly' $
        bPhase <+>
        "~" <+> tupled bBases <+>
        "=>" <+>
        ePhase <+> "~" <+> tupled eBases

instance DafnyPrinter Intv where
  pp e@(Intv e1 e2) = debugOnly e $
    brackets $ e1 <+> ".." <+> e2

instance DafnyPrinter s => DafnyPrinter (Interval s) where
  pp e@(Interval e1 e2) = debugOnly' $
    brackets $ e1 <+> ".." <+> e2


instance (DafnyPrinter f) => DafnyPrinter (SpecExpF f) where
  pp s = debugOnly' ppSubterm
    where
      ppSubterm = case s of
        SEWildcard -> pp "_"
        SESpecNor n -> pp n
        SESpecHad h -> pp h
        SESpecEn e -> pp e
        SESpecEn01 e -> pp e

instance DafnyPrinter f => DafnyPrinter (SpecNorF f) where
  pp (SpecNorF v1 e2) = "⊗" <+> v1 <+> '.' <+> e2

instance DafnyPrinter f => DafnyPrinter (SpecHadF f) where
  pp (SpecHadF v1 p) = "⊗" <+> v1 <+> '.' <+> p

instance DafnyPrinter f => DafnyPrinter (SpecEnF f) where
  pp (SpecEnF v1 intv a p es) =
    "Σ" <+> v1 <+> "∈" <+> intv <+> '.' <+> a <+> p <+> tupled es
  
instance DafnyPrinter f => DafnyPrinter (SpecEn01F f) where
  pp (SpecEn01F v1 intv1 a p v2 intv2 e5) =
    "Σ" <+> v1 <+> "∈" <+> intv1 <+> '.' <+>
    "⊗" <+> v2 <+> "∈" <+> intv2 <+> '.' <+>
    a <+> p <+> tupled e5



instance DafnyPrinter f => DafnyPrinter (Maybe f) where
  pp Nothing  = pp "_"
  pp (Just x) = pp x

instance (DafnyPrinter f) => DafnyPrinter (PhaseExpF f) where
  pp p = debugOnly' $ case p of
    PhaseZ                -> pp "1"
    PhaseWildCard         -> pp "_"
    PhaseOmega e1 e2      -> "ω" <!> tupled [e1, e2]
    PhaseSumOmega i e1 e2 -> "Ω" <+> i <+> "." <+> tupled [e1, e2]

instance (DafnyPrinter f) => DafnyPrinter (AmpExpF f) where
  pp p = debugOnly' $ case p of
    ADefault     -> mempty
    AISqrt en ed -> "isqrt" <+> tupled [en, ed]
    ASin e       -> "sin" <!> parens e
    ACos e       -> "cos" <!> parens e


instance DafnyPrinter EmitExp where
  pp (e1 :@: e2) = e1 <!> "[" <!> e2 <!> "]"
  pp (e1 :@@: (e2, e3)) =
    P.group $ e1 <!> "[" <!> e2 <!> ".." <!> e3 <!> "]"
  pp EMtSeq = pp "[]"
  pp (EMakeSeq ty e ee) =
    "seq<" <!> ty <!> ">" <!> align (tupled [e, ee])
  pp (EDafnyVar s) = pp s
  pp (EOpChained e eos) =
    foldl (\el (op, er) -> ppOp2 op el (pp er)) (pp e) eos
  pp (ECard e) = "|" <!> e <!> "|"
  pp (ECall v es) = v <!> align (tupled es)
  pp (EMultiLambda vs e) = tupled vs <+> "=>" <+> align e
  pp (EAsReal e) =
    parens (parens e <+> "as real")

instance DafnyPrinter Range where
  pp rr@(Range v l r) = debugOnly rr $ pp (EVar v :@@: (l, r))

instance DafnyPrinter Partition where
  pp par@(Partition p) = debugOnly par $ byComma p

instance DafnyPrinter Loc where
  pp = pp . deref

instance DafnyPrinter Locus where
  pp st@(Locus {loc=l, part=p, qty, degrees=dgrs}) = debugOnly st $
    l <+> "↦" <+> p <+> "::" <+> qty <+> list dgrs

instance DafnyPrinter PhaseRef where
  pp PhaseRef{prBase, prRepr} =
    prRepr <+> "/" <+> prBase


instance DafnyPrinter EmitData where
  pp EmitData{evPhaseRef, evBasis, evAmp} = P.align . list $
    phase ++
    [ "ket:" <+> evBasis
    , "amp:" <+> evAmp
    ]
    where
      phase = concatMap ppPhase evPhaseRef
      ppPhase (PhaseRef{prBase, prRepr}, ty) =
        [ "phase:" <+> (prRepr, ty)
        , "base:"  <+> prBase ]

instance DafnyPrinter LocusEmitData' where
  pp (LocusEmitData' (ed, red)) =
    align (ed <!> line <!> align (list (go <$> red)))
    where
      go (r, ed) = r <+> "→" <+> ed

instance (DafnyPrinter a, DafnyPrinter b) => DafnyPrinter (a, b) where
  pp t'@(a, b) = align (tupled [pp a, pp b])

instance ( DafnyPrinter a
         , DafnyPrinter b
         , DafnyPrinter c) => DafnyPrinter (a, b, c) where
  pp t'@(a, b, c) =
    align (tupled [pp a, pp b, pp c])

-- instance (Show f, DafnyPrinter f) => DafnyPrinter (QSpecF f) where
--   pp q@QSpecF{amp, phase, spec} = debugOnly q $
--     amp <+> phase <+> "~" <+> spec


instance ( DafnyPrinter k, DafnyPrinter v
         ) => DafnyPrinter (Map.Map k v) where
  pp m' = debugOnly' $
    vsep (row <$> m)
    where
      m = Map.toList m'
      row (a, b) = a <+> "↦" <!> (P.softline :: Builder) <!> b

instance DafnyPrinter MTy where
  pp (MTy (Inl ty)) = pp ty
  pp (MTy (Inr m)) =
    byComma (mtSrcParams m) <+> "↪" <+> byComma (mtSrcReturns m)

instance DafnyPrinter a => DafnyPrinter (Normalized a) where
  pp = debugOnly' . denorm

instance DafnyPrinter SRel where
  pp = go
    where
      ppl = align . list
      go  (RNor ns) = ppl ns

-- | Warning: don't emit parentheses in `ppOp2` because `EOpChained` relies
-- on this function not to be parenthesized
-- TODO: I want to get the precedence right here.
ppOp2 :: Op2 -> Builder -> Builder -> Builder
ppOp2 ONor b1 b2 = "nor" <!> align (tupled [b1, b2])
ppOp2 op b1 b2 =  P.group $
  align (parenOpt b1 <+> opSign <> line <> parenOpt b2)
  where
    parenOpt :: Builder -> Builder
    parenOpt =
      case op of
        OAnd -> parens
        OOr  -> parens
        OMod -> parens -- mod is a fragile operator
        ODiv -> parens
        _    -> id

    opSign = pp $
      case op of
        OAnd -> "&&"
        OOr  -> "||"
        OAdd -> "+"
        OSub -> "-"
        OMul -> "*"
        OMod -> "%"
        ODiv -> "/"
        OEq  -> "=="
        OLt  -> "<"
        OLe  -> "<="
        OGt  -> ">"
        OGe  -> ">="

ppConds :: DafnyPrinter a => a -> [Exp'] -> [Builder]
ppConds s = map ((s <+> (space :: Builder)) <!>)

runBuilder :: DafnyPrinter a => Int -> Bool -> a -> TL.Text
runBuilder i debug =
  renderLazy debug
  . P.layoutPretty P.defaultLayoutOptions
  . P.indent i
  . pp

-- | Prettyprint the program into a Lazy Text
texify :: DafnyPrinter a => a -> TL.Text
texify = runBuilder 0 False

showEmit :: DafnyPrinter a => a -> String
showEmit = TL.unpack . texify

-- * Debug modes

-- | Prettyprint the term in debugging mode as String
showEmitI :: DafnyPrinter a => Int -> a -> String
showEmitI i = TL.unpack . runBuilder i True

-- | Prettyprint the term in debugging mode as String with 0 indentation
showEmit0 :: DafnyPrinter a => a -> String
showEmit0 = showEmitI 0

-- | Prettyprint the source code to IO.
prettyIO :: DafnyPrinter a => a -> IO ()
prettyIO = putDoc True . pp


-- Regex for OverloadedStrings
-- \(\"[^\"]+?\"\) → t\1

--------------------------------------------------------------------------------
-- Debugging Instances
--------------------------------------------------------------------------------

instance DafnyPrinter [Int] where
  pp = list

ppSst :: DafnyPrinter a => (a, (QTy, [Int])) -> Builder
ppSst (p, (q, i)) = p <+> "::" <+> q <+> "~" <+> list i

instance DafnyPrinter TState where
  pp TState{_sSt, _xSt, _emitSt} = vsep
    [ pp "Partition Reference State:"
    , ppincr2 (vsep <$> _xSt)
    , pp "Partition State:"
    , ppincr2 (ppSst <$> _sSt)
    , pp "Renaming State:"
    , ppincr2 _emitSt
    ]
    where
      ppincr2 :: DafnyPrinter a => a -> Builder
      ppincr2 = pp . incr2

ppIEnv :: IEnv -> Builder
ppIEnv = list . (second (list . NE.toList) <$>)
