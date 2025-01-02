{
{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , NamedFieldPuns

  #-}


module Qafny.Syntax.Parser(scanAndParse) where
import qualified Qafny.Syntax.Lexer as L
import           Qafny.Syntax.ParserUtils
import           Qafny.Syntax.AST
-- import           Qafny.Codegen.NCodegen
import           Control.Monad
import           Data.Sum
import           Data.Maybe
}

%name runParser
%tokentype { L.SToken }
%error { parseError }
%errorhandlertype explist
%monad { Parser }{ >>= }{ return }

%token
'_'                   { ( _, L.TWildcardName ""  ) }
'1'                 { ( _, L.TWildcardName "1" ) }
'S'                   { ( _, L.TWildcardName "_S" ) }
'T'                   { ( _, L.TWildcardName "_T" ) }
'o'                   { ( _, L.TWildcardName "_o" ) }
'O'                   { ( _, L.TWildcardName "_O" ) }

namedW                { ( _, L.TWildcardName $$ ) }
digits                { ( _, L.TLitInt $$ ) }
dafny                 { ( _, L.TDafny $$  ) }
"method"              { ( _, L.TMethod    ) }
"function"            { ( _, L.TFunction  ) }
"ensures"             { ( _, L.TEnsures   ) }
"requires"            { ( _, L.TRequires  ) }
"separates"           { ( _, L.TSeparates ) }
"invariant"           { ( _, L.TInv       ) }
"with"                { ( _, L.TWith      ) }
"at"                  { ( _, L.TAt        ) }
"split"               { ( _, L.TSplit     ) }
"for"                 { ( _, L.TFor       ) }
"returns"             { ( _, L.TReturns   ) }
"not"                 { ( _, L.TNot       ) }
"nat"                 { ( _, L.TNat       ) }
"real"                { ( _, L.TReal      ) }
"int"                 { ( _, L.TInt       ) }
"in"                  { ( _, L.TIn        ) }
"bool"                { ( _, L.TBool      ) }
"seq"                 { ( _, L.TSeq       ) }
"nor"                 { ( _, L.TNor       ) }
"had"                 { ( _, L.THad       ) }
"H"                   { ( _, L.THApp      ) }
"Qft"                 { ( _, L.TQFT       ) }
"iQft"                { ( _, L.TRQFT      ) }
"repr"                { ( _, L.TRepr      ) }
"measure"             { ( _, L.TMeasure   ) }
"measured"            { ( _, L.TMeasured  ) }
"en"                  { ( _, L.TEn        ) }
"taa"                  { ( _, L.TAA        ) }
"Q"                { ( _, L.TQReg      ) }
"ena"                { ( _, L.TEn01      ) }
"var"                 { ( _, L.TVar       ) }
"if"                  { ( _, L.TIf        ) }
"sqrt"               { ( _, L.TSqrt     ) }
"sin"                 { ( _, L.TSin       ) }
"cos"                 { ( _, L.TCos       ) }


"λ"                   { ( _, L.TCl            ) }
"Σ"                   { ( _, L.TUnicodeSum    ) }
"⊕"                   { ( _, L.TUnicodeOPlus ) }
"⊗"                   { ( _, L.TUnicodeTensor ) }
"ω"                   { ( _, L.TUnicodeOmega  ) }
-- "Ω"                   { ( _, L.TUnicodeSumOmega ) }
"∈"                   { ( _, L.TUnicodeIn     ) }
"⟩"                    { ( _, L.TKet     )           }
"↦"                   { ( _, L.TUnicodeMap    ) }
"assert"              { ( _, L.TAssert    ) }
"forall"              { ( _, L.TForall    ) }
"||"                  { ( _, L.TOr        ) }
"&&"                  { ( _, L.TAnd       ) }
'+'                   { ( _, L.TAdd       ) }
'/'                   { ( _, L.TDiv       ) }
'-'                   { ( _, L.TSub       ) }
'*'                   { ( _, L.TMul       ) }
'^'                   { ( _, L.TPow       ) }
'\%'                  { ( _, L.TMod       ) }
'|'                   { ( _, L.TBar       ) }
'('                   { ( _, L.TLPar      ) }
')'                   { ( _, L.TRPar      ) }
'<'                   { ( _, L.TLAng      ) }
'>'                   { ( _, L.TRAng      ) }
'['                   { ( _, L.TLBracket  ) }
']'                   { ( _, L.TRBracket  ) }
'{'                   { ( _, L.TLBrace    ) }
'}'                   { ( _, L.TRBrace    ) }
id                    { ( _, L.TId $$     ) }
','                   { ( _, L.TComma     ) }
"::"                   { ( _, L.TDColon     ) }
':'                   { ( _, L.TColon     ) }
'.'                   { ( _, L.TDot       ) }
';'                   { ( _, L.TSemi      ) }
"=="                  { ( _, L.TEq        ) }
'->'                  { ( _, L.TTyArrow   ) }
"=>"                  { ( _, L.TArrow     ) }
"==>"                  { ( _, L.TImply     ) }
">="                  { ( _, L.TGe        ) }
"<="                  { ( _, L.TLe        ) }
":="                  { ( _, L.TAssign    ) }
"*="                  { ( _, L.TApply     ) }
'~'                   { ( _, L.TTilde     ) }
'\@'                   { ( _, L.TStore     ) } 

%expect 0
%right '->' 
%left "⊕"
%left '\%'
%left '+' '-'
%left '*'
%left '/'
%left '^'

%%
AST
  : toplevels                         { $1                                   }
                                                                          
toplevels :: { [QMethod] }                                                     
  : many(toplevel)                    { $1                                   }
             
many(p)
  : {- empty -}                       { []      }
  | p many(p)                         { $1 : $2 }    
                                                                          
toplevel  :: { QMethod }
  :  "method" id '(' bindings ')' returns conds '{' stmts '}'                 
    { QMethod $2 $4 $6 (fst $7) (snd $7) $9 }

returns :: { [Binding] }
  : {- empty -}                       { [] }
  | "returns" '(' bindings ')'        { $3 }
  
manyComma(p)                                                                  
  : manyComma_(p)                     { $1 }
  | {- empty -}                       { []         }
                                                                          
manyComma_(p)
  : p ',' manyComma_(p)               { $1 : $3 }
  | p                                 { [$1]    }
  
bindings
  : manyComma(binding)                { $1 }

binding                                                                   
  : id ':' ty                         { Binding $1 $3                        }
               
conds :: { [ PredExp ], [PredExp] }
  : {- empty -}                                   { ([],[])                                   }
  | "requires" spec conds                { (($2 : (fst $3)), (snd $3)) }
  | "ensures" spec conds                { ((fst $3), $2 : (snd $3)) }
  | "invariant" spec conds                { (($2 : (fst $3)), (snd $3)) }
                                                               
ty :: { Ty }  
  : baseTy                            { $1             }
  | baseTy '->' ty                    { TArrow [$1] $3 }
 -- | tuple(ty) '->' ty   %shift        { TArrow $1   $3 }

intv :: { Intv }
  : '[' arithExpr ',' arithExpr ')'            { Intv $2 $4 }
  | '[' arithExpr ']'                          { Intv $2 (AOp2 OAdd $2 (ANat 1)) }
          
qty :: { QTy }
  : "nor"                                       { TNor                            }
  | "had"                                       { THad                            }
  | "en"                                       { TEn   (ANat 0)               }
  | "en" '(' arithExpr ')'                      { TEn   $3               }
  | "taa"                                       { TAA                           }
  
baseTy
  : "nat"                             { TNat              }
  | "real"                            { TReal              }
  | "int"                             { TInt              }
  | "bool"                            { TBool             }
  | '[' ty ']'                        { TSeq $2           }
  | "Q" '[' arithExpr ']'             { TQReg $3   }
--  | parens(ty)                        { $1                }
-- so far, don't allow higher order functions

partition :: { [Range] }
  : manyComma_(range)                  { $1 }
                                                                          
range                                                                     
  : id '[' arithExpr ',' arithExpr ')'         { Range $1 $3 $5 }
  | id '[' arithExpr ']'                       { Range $1 $3 (AOp2 OAdd $3 (ANat 1)) }
                                                            
spec ::   { PredExp }
  :  '{' partition ':'  qty "↦" qspec '}'
                                      { ESpec $2 $4 $6                     }

  | "forall" binding "::" logicOrExp "==>" logicOrExp
                                      { EForall $2 (Just $4) $6 }
  | logicImply {  DPred $1 }

symT : alt("⊗", 'T') { $1 }
symS : alt("Σ", 'S') { $1 }

alt(p, q)
 : p { $1 }
 | q { $1 }
 
mayket(p)
  : '|' p "⟩"                            {[$2]}
  | '|' p "⟩" mayket(p)                  {$2 : $4}

ketexpr 
  : addarith                             {}
  
maySum
  : symS id "∈" intv '.'                {[($2,$4)] }
  | symS id "∈" intv '.' maySum         {($2,$4):$6 }
                  
qstate :: {AExp}
  : arithExpr {$1}
  | addarith  {if $1 == OAdd then AHad True else AHad False }
                  
qspec ::  { SpecExp }
  : symT id '.' mayket(qstate) 
                                      { SESpecTen $2 Nothing $4 }
  | symT id "∈" intv '.' mayket(qstate)   { SESpecTen $2 (Just $4) $6 }
  | mayket(qstate)                         { SESpecTen "null" Nothing $1 }

  | maySum arithExpr mayket(arithExpr)         {% omegaCase $1 $2 $3 }


logicImply ::  { DGuardExp } 
  : logicOrExp {$1}
  |  logicOrExp "==>" logicOrExp {DImply $1 $3}

logicOrExp :: { DGuardExp } 
  : logicAndExp "||" logicOrExp       { DOr $1 $3         }
  | logicAndExp                       { $1 } 

logicAndExp :: { DGuardExp } 
  : logicNotExp "&&" logicAndExp        { DAnd $1 $3        }
  | logicNotExp                         { $1 }

logicNotExp :: {DGuardExp}
  : "not" logicNotExp                 { DNeg $2        }
  | id '(' manyComma(arithExpr) ')'    {DFun $1 $3 }
  | chainBExp                         { $1 }

chainBExp
  : arithExpr "==" arithExpr           { DEq $1 $3 }
  | arithExpr '<'  arithExpr           { DLt $1 $3 }
  | arithExpr "<=" arithExpr           { DLe $1 $3 }
  | arithExpr ">=" arithExpr           { DLe $3 $1 }
  | arithExpr "==" chainBExp           { DAnd (DEq $1 (getFirst $3)) $3 }
  | arithExpr '<'  chainBExp           { DAnd (DLt $1 (getFirst $3)) $3 }
  | arithExpr "<=" chainBExp           { DAnd (DLe $1 (getFirst $3)) $3 }
  | arithExpr ">=" chainBExp           { DAnd (DLe (getFirst $3) $1) $3 }
  
arithExpr :: { AExp }
 : arithAtomic arith arithExpr       { AOp2 $2 $1 $3 }
 | arithAtomic                       { $1 }
 | arithInd                          { AOp2 OInd (AVar (fst $1)) (snd $1)}
 
addarith :: {Op2}
 : '+'                      { OAdd }
 | '-'                      { OSub }
 
arith :: { Op2 }
 : addarith                     { $1 }
 | '/'                      { ODiv }
 | '*'                      { OMul }
 | '\%'                     { OMod }
 | '^'                      { OPow }
 | "⊕"                     { OXor }

arithAtomic
 : digits                  {ANat $1 }
 | id                      { AVar $1 }
 | '(' arithExpr ')'       { $2 }
 | id '(' manyComma(arithExpr) ')' {AFun $1 $3 }
 | '|' arithExpr '|'       { AOp1 OLen $2 }
 | "sin" '(' arithExpr ')'           {AOp1 OSin $3}
 | "cos" '(' arithExpr ')'           {AOp1 OCos $3}
 | "sqrt" '(' arithExpr ')'          {AOp1 OSqrt $3}
 | "ω" '(' arithExpr ',' arithExpr ')'              { AOp2 Omega $3 $5       }

arithInd :: { (Var, AExp) }
 : id '[' arithExpr ']'     { ($1, $3)}
 
expr
  : "H"                               { EHad                   }
  | "Qft"                             { EQft False             }
  | "iQft"                            { EQft True              }
  | "λ" '(' manyComma_(id) "=>" "ω" '(' arithExpr ',' arithExpr ')' mayket(arithExpr) ')' { ELambda $3 (Just (AOp2 Omega $7 $9)) $11  }
  | "λ" '(' manyComma_(id) "=>" mayket(arithExpr) ')' { ELambda $3 Nothing $5  }
  | "λ" '(' manyComma_(id) "=>" "ω" '(' arithExpr ',' arithExpr ')' ')' { ELambda $3 (Just (AOp2 Omega $7 $9)) []  }
  
qbool
  : arithInd                                         { GEPar(fst $1) (snd $1) }
  | arithExpr "==" arithExpr '\@' arithInd       { GEq $1 $3 (fst $5) (snd $5) }
  | arithExpr '<' arithExpr '\@' arithInd        { GLt $1 $3 (fst $5) (snd $5) }
  | "not" qbool                       { GNeg $2 }

bexp
  : logicOrExp { BE $1 }
  | qbool      { QE $1 }

stmts                                                                     
  : many(stmt)                        { $1                                   }
                                                                          
                                                                          
stmt :: { Stmt }
  : dafny                             { SDafny $1                            }
  | "assert" spec ';'                    { SAssert $2                           }
  | '(' qty ')' partition ';'         { SCast $4 $2                          }
  | "var" binding ';'                    { SVar $2 Nothing                      }
  | "var" binding ":=" arithExpr ';'       { SVar $2 (Just $4)                    }
  | id ":=" arithExpr ';'                  { $1 ::=: $3                           }
  | manyComma_(id) "*=" "measure" '(' partition ')' ';'           { SMea (head $1) (tail $1) $5      }
  | partition "*=" expr ';'           { $1 :*=: $3                           }
  | "if" '(' bexp ')' '{' stmts '}'  {% ifcase $3 $6                   }
  | "for" id "in" '[' arithExpr ',' arithExpr ')' conds '{' stmts '}'  { SFor $2 $5 $7 (fst $9) $11 }
  | id '(' manyComma(arithExpr) ')' ';'                { SCall $1 $3 }
  
--  | "if" '(' logicOrExp ')' cond block
--    {% do sep <- separatesOnly $5; return $ SIf (Guard $3) sep $6                    }




 
 
{
--- converts a string to tokens and then parses it into our AST
scanAndParse :: String -> Parser AST
scanAndParse = runParser <=< L.runScanner
}
