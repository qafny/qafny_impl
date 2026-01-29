/* 
 * # Qafny Grammar:
 *
 * Operator Precedence (higher items bind tighter):
 * function calls, ranges, in, member access (e.g. a.Length)
 * unary subtraction (-), unitary not expr (not)
 * exponential/xor (^, ⊕)
 * product/quotient/remainder (*, /, %)
 * sum/difference (+, -)
 * relational (<, <=, >, >=)
 * equality (==, !=)
 * &&
 * ||
 * assignment (=, :=, etc.)
 * ,
 * 
 */

grammar Exp;

// Root node for the ANTLR version of the Qafny AST
program: (topLevel)+ EOF;

// Top-level statements concist of include statements, methods, functions, lemmas, and predicates
topLevel: TInclude | method | function | lemma | predicate;

method: 'method' ('{' ':' Axiom '}')? ID '(' bindings ')' ('returns' returna)? conds ('{' stmts '}')?;

function : Function ('{' ':' Axiom '}')? ID '(' bindings ')' (':' typeT)? ('{' (arithExpr | qspec) '}')?;

lemma : Lemma ('{' ':' Axiom '}')? ID '(' bindings ')' conds;

// a function that exclusively returns a boolean, can be used for conditions, including invariants
predicate : Predicate ID '(' bindings ')' '{' qspec '}';

returna: '(' bindings ')';

conds: (reen spec | Decreases arithExpr)*;

reen: Ensures | Requires;

loopConds : (Invariant spec | Decreases arithExpr | Separates locus)*;

stmts : stmt*;

stmt: asserting | casting | varcreate | assigning | qassign | qcreate | measure | measureAbort | ifexp | forexp | whileexp | (fcall ';') | returnStmt | breakStmt;

spec : qunspec | logicImply | chainBExp | '{' (qunspec | logicImply | chainBExp) '}';

bexp: logicExpr | qbool | ID | boolLiteral;

qbool: qrange | '{' locus '}' comOp arithExpr | arithExpr comOp arithExpr '@' idindex | 'not' qbool;

logicImply: allspec | allspec '==>' logicImply | qunspec;

allspec: logicExpr | 'forall' typeOptionalBinding '::' chainBExp '==>' logicImply | 'forall' typeOptionalBinding TIn crange '==>' logicImply;

logicExpr: logicExpr '||' logicExpr | logicExpr '&&' logicExpr | 'not' logicExpr | chainBExp | logicInExpr | qunspec | arithExpr;

logicInExpr: arithExpr TIn arithExpr;

chainBExp: arithExprWithSum (comOp arithExprWithSum)+;

// comparison operators
comOp :  GE | LE | EQ | NE | LT | GT;

// see SWAPTest.qfy for an instance where the amplitude is specified before the qspecs
qtypeCreate: qty '↦' (arithExpr '.')? qspec ('+' qspec)*;

qunspec : locus ':' qtypeCreate ('⊗' locus ':' qtypeCreate)*;

qspec : tensorall | arithExpr? manyketpart | sumspec;

// 4 different calls for the partition function:
// 1. part(n, function_predicate, true_amplitude, false_amplitude)
// 2. part(n, predicate_1, predicate_2)
// 3. part(function_predicate, true, amplitude) + part(function_predicate, false, amplitude)
// 4. part(function_predicate, amplitude) - typically inside a ketCifexp
partspec: 'part' '(' (arithExpr ',' arithExpr ',' arithExpr ',' arithExpr | (arithExpr ',')? partpred ',' partpred | ID ',' boolLiteral ',' arithExpr | arithExpr ',' arithExpr | partsections) ')';

// custom predicate used in the part function
// amplitide ':' predicate
partpred: amplitude=arithExpr ':' pred=bexp;

// custom used in the part function
partsection: amplitude=arithExpr ':' ket pred=fcall;
partsections: partsection ('+' partsection);

tensorall: '⊗' ID (TIn crange)? '.' (manyket | omegaExpr);

sumspec: maySum (arithExpr? manyketpart | '(' arithExpr? manyketpart ')' | (arithExpr '.')? tensorall ) | maySum (arithExpr '.')? sumspec | '(' sumspec ')';

maySum: TSum ID TIn crange (('on' | '@') '(' bexp ')')? '.';

asserting: 'assert' spec ';';

casting: '(' qty ')' locus ';';

varcreate : 'var' bindings ';' | 'var' typeOptionalBindings ':=' arithExpr ';';

assigning : idindices ':=' arithExpr ';';

ids : ID (',' ID)* ;

idindices: (ID | idindex) (',' (ID | idindex))*;

qassign : (locus | ID) '*=' expr ';';

qcreate : 'var' qrange '*=' 'init' '(' arithExpr ')' ';';

// enforce only locuses in measure, no IDs.
measure : 'var'? idindices '*=' 'measure' '(' locus (',' arithExpr)? ')' ';' ;

// see SWAPTest.qfy
measureAbort: 'var'? idindices '*=' 'measA' '(' locus (',' arithExpr)? ')' ';';

returnStmt: Return ids ';';

breakStmt: 'break' ';';

ifexp: If ('(' bexp ')' | bexp) 'then'? '{' stmts '}' (Else '{' stmts '}')?;

cifexp : If bexp 'then' (arithExpr | '{' arithExpr '}') Else (arithExpr | '{' arithExpr '}');

manyketpart: (manyketchild)+;

manyketchild: ket | partspec | '(' ket (',' ket)* ')' | fcall | ID | idindex;

forexp : 'for' ID TIn crange (('with' | '&&') bexp)? loopConds '{' stmts '}';

whileexp: 'while' ('(' bexp ')' | bexp) loopConds '{' stmts '}';

fcall : ID '^{-1}'? '(' arithExprsOrKets ')';

arithExprsOrKets : (arithExpr | ket) (',' (arithExpr | ket))*;

// ANTLR will choose the first branch for a list of options, so for operator precendence to work properly, the operators must be split by their levels of precendence.
// Put another way, higher precendence operators (evaluate these first) must come before lower precedence operators
arithExprWithSum: arithExprWithSum exponentialOp arithExprWithSum | arithExprWithSum multiplicativeOp arithExprWithSum | arithExprWithSum additiveOp arithExprWithSum | maySum arithExprWithSum | '(' arithExprWithSum ')' | arithAtomic;

arithExpr: cifexp | arithExpr exponentialOp arithExpr | arithExpr multiplicativeOp arithExpr | arithExpr additiveOp arithExpr | arithAtomic;

arithAtomic: numexp | ID | TSub arithAtomic | boolLiteral
          | '(' arithExpr ')' | '(' logicExpr ')'
          | fcall |  absExpr | sinExpr | cosExpr | sqrtExpr | omegaExpr | rotExpr | notExpr | setInstance | qrange | ketCallExpr | memberAccess;

sinExpr : 'sin' ('^' numexp)? '(' arithExpr ')' | 'sin' arithAtomic;

cosExpr : 'cos' ('^' numexp)? '(' arithExpr ')' | 'cos' arithAtomic;

sqrtExpr : 'sqrt' ('^' numexp)? '(' arithExpr ')' | 'sqrt' arithAtomic;

notExpr : 'not' '(' arithExpr ')';

absExpr : '|' arithExpr '|' ;

omegaExpr : ('ω' | 'omega') '(' arithExpr (',' arithExpr)? (',' arithExpr)? ')' ;

rotExpr : 'rot' '(' arithExpr ')' ;

ketCallExpr : 'ket' '(' arithExpr ')';

setInstance : '[' (arithExpr (',' arithExpr)*)? ']';

memberAccess: ID (TDot ID)+;

expr : SHad | SQFT | RQFT | lambdaT | dis | ID;

// we could make these take typeOptionalBindings?
lambdaT: 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' omegaExpr manyket ')'
       | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' manyket ')'
       | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' omegaExpr ')'
       | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' rotExpr ')' ;
       // | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' fcall ')'
       // | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' logicOrExp ')'; // phase kick-back

dis : 'dis' '(' expr ',' arithExpr ',' arithExpr ')';

manyket: (ket)+;

// TODO: what does the subtraction mean?
ket : TSub? '|' qstate (',' qstate)* '⟩' | '⊗' arithAtomic;

ketsum : maySum arithExpr;

qstate: arithExpr | additiveOp | ketsum;

bindings : binding ( ',' binding)*;

binding: ID ':' typeT;ket

typeOptionalBindings : typeOptionalBinding (',' typeOptionalBinding)*;

typeOptionalBinding: ID (':' typeT)?;

// i.e. q[0], z[0, n)
locus : qrange (',' qrange)*;

crange : '[' arithExpr ',' arithExpr ')';

index: '[' arithExpr ']';

idindex : ID index;

qrange: (ID | fcall) (index | index crange | crange); // multiple cranges used in StateDistinguishing.qfy

// element : numexp | ID;

numexp: Number | TSub Number;
        
 // Lexical Specification of this Programming Language
 //  - lexical specification rules start with uppercase

typeT : baseTy | (baseTy | '(' baseTy (',' baseTy)* ')') '->' typeT;

baseTy: TNat # NaturalType 
      | TReal # RealType
      | TInt # IntType
      | TBool # BoolType
      | TBV # BitVectorType
      | '[' baseTy ']' # ArrayType
      | 'array' '<' baseTy '>' # DynamicArrayType
      | 'set' '<' baseTy '>' # SetType
      | '[' baseTy ','  arithExpr ']' # ArrayWithSizeType
      | baseTy '[' arithExpr ']' # ArrayWithSizeType
      | 'Q' '[' arithExpr ']' # QBitStringType;

qty : Nor | Had | En | En '(' arithExpr ')' | aaType;

aaType : AA | AA '(' arithExpr ')';

additiveOp: TAdd | TSub | OPlus;

multiplicativeOp: TMul | TDiv | TMod | TDot;

exponentialOp: TExp | TXor;

// op : addOp | TDiv | TMul | TMod | OPlus | TExp | TXor | Dot;

boolLiteral: TrueLiteral | FalseLiteral;

//----------------------------------------------------------------
// Lexer Tokens (in ANTLR, all tokens start with capital letters)
//----------------------------------------------------------------

Axiom: 'axiom';

Function: 'function';

Lemma: 'lemma';

Predicate: 'predicate';

// keywords used in conditions
Ensures : 'ensures';

Requires : 'requires';

Decreases : 'decreases';

Separates : 'separates';

Returns : 'returns';

Return : 'return';

Forall : 'forall';

// ────────── Classical Types ──────────
TNat : 'nat';

TReal : 'real';

TInt : 'int';

TBool : 'bool' | 'Bool';

TBV : 'bv' DIGIT+;

// ────────── Operators ──────────
TAdd : '+';

TSub : '-';

TMul : '*';

TDiv : '/';

TMod : '%';

TExp : '^';

OPlus: '⊕';

TXor : 'xor';

TDot : '.' ;

// ────────── Quantum Types ──────────
Nor : 'nor' | 'Nor';

Had : 'had' | 'Had';

AA : 'aa';

En : 'en' | 'En';

// qafny gates
SHad : 'H';

SQFT : 'QFT';

RQFT : 'RQFT';

// keywords
If : 'if';

Else : 'else';

For : 'for';

While : 'while';

TrueLiteral : 'true' | 'True';

FalseLiteral : 'false' | 'False';

TCl : 'λ';

TKet : '⟩';

TIn : 'in' | '∈';

TSum : 'Σ' | '∑';

Invariant : 'invariant';

// Comparison operators
And : '&&';

OR : '||';

GE : '>=';

LE : '<=';

EQ : '==';

NE : '!=';

LT : '<';

GT : '>';

ARROW : '=>';


Number : DIGIT+ ('.' DIGIT+)?;

ID :   Letter LetterOrDigit*;

Letter :   [a-zA-Z$_];

LetterOrDigit: [a-zA-Z0-9$_'];

// token for the include rule
TInclude: 'include ' PATH;

fragment PATH: [a-zA-Z0-9/\\$_.]+;

fragment DIGIT: ('0'..'9');

AT : '@';
ELLIPSIS : '...';
WS  :  [ \t\r\n\u000C]+ -> channel(HIDDEN);
Comment :   '/*' .*? '*/' -> channel(HIDDEN);
Line_Comment :   '//' ~[\r\n]* -> channel(HIDDEN);