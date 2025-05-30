grammar Exp;

// Root node for the ANTLR version of the Qafny AST
program: (topLevel)+ EOF;

// Top-level statements concist of include statements, methods, functions, lemmas, and predicates
topLevel: TInclude | method | function | lemma | predicate;

method: 'method' ('{' ':' Axiom '}')? ID '(' bindings ')' ('returns' returna)? conds ('{' stmts '}')?;

function : Function ('{' ':' Axiom '}')? ID '(' bindings ')' (':' typeT)? ('{' arithExpr '}')?;

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

bexp: logicOrExp | qbool | ID | boolLiteral;

qbool: qrange | '{' locus '}' comOp arithExpr | arithExpr comOp arithExpr '@' idindex | 'not' qbool;

logicImply: allspec | allspec '==>' logicImply | qunspec;

allspec: logicOrExp | 'forall' typeOptionalBinding '::' chainBExp '==>' logicImply | 'forall' typeOptionalBinding TIn crange '==>' logicImply;

logicOrExp: logicAndExp '||' logicOrExp | logicAndExp;

logicAndExp: logicNotExp '&&' logicAndExp | logicNotExp;

logicNotExp: 'not' logicNotExp | fcall | chainBExp | logicInExpr;

logicInExpr: ID TIn ID;

chainBExp: arithExprWithSum (comOp arithExprWithSum)+;

// comparison operators
comOp :  GE | LE | EQ | NE | LT | GT;

qtypeCreate: qty '↦' qspec ('+' qspec)*;

qunspec : locus ':' qtypeCreate ('⊗' locus ':' qtypeCreate)*;

// see SWAPTest.qfy for an instance where the amplitude is specified before the sum spec
qspec : tensorall | arithExpr? manyketpart | (arithExpr '.')? sumspec;

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

tensorall: '⊗' ID '.' manyket | '⊗' ID TIn crange '.' manyket;

sumspec: maySum (arithExpr? manyketpart ('&&' bexp)? | '(' arithExpr? manyketpart ('&&' bexp)? ')') | maySum (arithExpr '.')? sumspec | '(' sumspec ')';

maySum: TSum ID TIn crange (('on' | '@') '(' bexp ')')? '.';

asserting: 'assert' spec ';';

casting: '(' qty ')' locus ';';

varcreate : 'var' bindings ';' | 'var' typeOptionalBindings ':=' arithExpr ';';

assigning : idindices ':=' arithExpr ';';

ids : ID (',' ID)* ;

idindices: (ID | idindex) (',' (ID | idindex))*;

qassign : (locus | ID) '*=' expr ';';

qcreate : 'var' (locus | ID) '*=' arithExpr ';';

measure : idindices '*=' 'measure' '(' (locus | ID) ')' ';' | idindices '*=' 'measure' '(' (locus | ID) ',' arithExpr ')' ';' ;

// see SWAPTest.qfy
measureAbort: ids '*=' 'measA' '(' (locus | ID) ')' ';' | ids '*=' 'measA' '(' (locus | ID) ',' arithExpr ')' ';';

returnStmt: Return ids ';';

breakStmt: 'break' ';';

ifexp: If ('(' bexp ')' | bexp) 'then'? '{' stmts '}' (Else '{' stmts '}')?;

cifexp : If bexp 'then' (arithExpr | '{' arithExpr '}') Else (arithExpr | '{' arithExpr '}');

// allows partspecs as as nodes in sum spec expressions (see test16.qfy for an example)
ketArithExpr: ketCifexp | partspec | '(' ketArithExpr ')';

// allows partspecs for sum spec expressions
ketCifexp: If bexp 'then' ketArithExpr 'else' ketArithExpr;

manyketpart: (ket | ketArithExpr | '(' ket (',' ket)* ')' | fcall | ID | idindex)+;

forexp : 'for' ID TIn crange (('with' | '&&') bexp)? loopConds '{' stmts '}';

whileexp: 'while' ('(' bexp ')' | bexp) loopConds '{' stmts '}';

fcall : ID '^{-1}'? '(' arithExprsOrKets ')';

arithExprsOrKets : (arithExpr | ket) (',' (arithExpr | ket))*;

arithExprWithSum: arithExpr | maySum arithExprWithSum | arithExpr op arithExprWithSum | '(' arithExprWithSum ')';

arithExpr: cifexp | arithAtomic op arithExpr | arithAtomic | arithExpr (index | sliceExpr | crange); // | sumspec | qtypeCreate;

// arithExprNoSum: cifexpNoSum | arithAtomic op arithExprNoSum | arithAtomic | arithExprNoSum (index | sliceExpr | crange) | '(' arithExprNoSum ')';

arithAtomic: numexp | ID | TSub arithExpr | boolLiteral
          | '(' arithExpr ')'
          | fcall |  absExpr | sinExpr | cosExpr | sqrtExpr | omegaExpr | rotExpr | notExpr | setInstance | qrange | ketCallExpr;

sinExpr : 'sin' ('^' Number)? '(' arithExpr ')' | 'sin' arithAtomic;

cosExpr : 'cos' ('^' Number)? '(' arithExpr ')' | 'cos' arithAtomic;

sqrtExpr : 'sqrt' ('^' Number)? '(' arithExpr ')' | 'sqrt' arithAtomic;

notExpr : 'not' '(' arithExpr ')';

absExpr : '|' arithExpr '|' ;

omegaExpr : ('ω' | 'omega') '(' arithExpr (',' arithExpr)? (',' arithExpr)? ')' ;

rotExpr : 'rot' '(' arithExpr ')' ;

ketCallExpr : 'ket' '(' arithExpr ')';

setInstance : '[' (arithExpr (',' arithExpr)*)? ']';

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
ket : TSub? '|' qstate (',' qstate)* '⟩' | '⊗' arithExpr;

ketsum : maySum arithExpr;

qstate: arithExpr | addOp | ketsum;

bindings : binding ( ',' binding)*;

binding: ID ':' typeT;

typeOptionalBindings : typeOptionalBinding (',' typeOptionalBinding)*;

typeOptionalBinding: ID (':' typeT)?;

// i.e. q[0], z[0, n)
locus : qrange (',' qrange)*;

crange : '[' arithExpr ',' arithExpr ')';

index: '[' arithExpr ']';

sliceExpr: '[' left=arithExpr? '..' right=arithExpr? ']';

idindex : ID index;

// rangeT: ID crange;

qrange: ID (index | crange)+;

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

aaType : AA | AA '(' qrange ')';

addOp: TAdd | TSub;

op : addOp | TDiv | TMul | TMod | OPlus | TExp | TXor | Dot;

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

// qafny classical types 
TNat : 'nat';

TReal : 'real';

TInt : 'int';

TBool : 'bool' | 'Bool';

TBV : 'bv' DIGIT+;

// qafny operators
TAdd : '+';

TSub : '-';

TMul : '*';

TDiv : '/';

TMod : '%';

TExp : '^';

TXor : 'xor';

// qafny quantum types
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

OPlus : '⊕';

Invariant : 'invariant';

Dot : '.' ;

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
