grammar Exp;

// root-node of the qafny AST
program: (topLevel)+ EOF;

// include statements or methods
topLevel: TInclude | method | function | lemma;

method: 'method' ('{' ':' Axiom '}')? ID '(' bindings ')' ('returns' returna)? conds ('{' stmts '}')?;

function : Function ('{' ':' Axiom '}')? ID '(' bindings ')' (':' typeT)? ('{' arithExpr '}')?;

lemma : Lemma ('{' ':' Axiom '}')? ID '(' bindings ')' conds;

returna: '(' bindings ')';

conds: (reen spec | Decreases arithExpr)*;

reen: Ensures | Requires;

loopConds : (Invariant spec | Decreases arithExpr | Separates locus)*;

stmts : (stmt)*;

stmt: asserting | casting | varcreate | assigning | qassign | qcreate | measure | measureAbort | ifexp | forexp | whileexp | (fcall ';') | return | break;

spec : qunspec | logicImply | chainBExp;

// allows quantum inner loops
qallspec : 'forall' ID '::' chainBExp '==>';

bexp: logicOrExp | qbool | ID | boolLiteral;

qbool: qrange | '{' locus '}' comOp arithExpr | arithExpr comOp arithExpr '@' idindex | 'not' qbool;

logicImply: allspec | allspec '==>' logicImply;

allspec : logicOrExp | 'forall' ID '::' chainBExp '==>' logicImply | 'forall' ID TIn crange '==>' logicImply;

logicOrExp: logicAndExp '||' logicOrExp | logicAndExp;

logicAndExp: logicNotExp '&&' logicAndExp | logicNotExp;

logicNotExp: 'not' logicNotExp | fcall | chainBExp | logicInExpr;

logicInExpr: ID TIn ID;

chainBExp: arithExpr (comOp arithExpr)+;

// comparison operators
comOp :  GE | LE | EQ | NE | LT | GT;

qtypeCreate: qty '↦' qspec ('+' qspec)*;

qunspec : '{' (qallspec* locus ':' qtypeCreate ('⊗' qallspec* locus ':' qtypeCreate)* | logicImply) '}';

// see SWAPTest.qfy for an instance where the amplitude is specified before the sum spec
qspec : tensorall | manyketpart | arithExpr manyketpart | (arithExpr '.')? sumspec | arithExpr;

// 4 different calls for the partition function:
// 1. part(n, function_predicate, true_amplitude, false_amplitude)
// 2. part(n, predicate_1, predicate_2)
// 3. part(function_predicate, true, amplitude) + part(function_predicate, false, amplitude)
// 4. part(function_predicate, amplitude) - typically inside a ketCifexp
partspec: 'part' '(' (arithExpr ',' arithExpr ',' arithExpr ',' arithExpr | arithExpr ',' partpred ',' partpred | partpred ',' partpred | ID ',' boolLiteral ',' arithExpr | arithExpr ',' arithExpr | partsections) ')';

// custom predicate used in the part function
// amplitide ':' predicate
partpred: amplitude=arithExpr ':' predicate=bexp;

// custom used in the part function
partsection: amplitude=arithExpr ':' ket predicate=fcall;
partsections: partsection ('+' partsection);

tensorall: '⊗' ID '.' manyket | '⊗' ID TIn crange '.' manyket;

sumspec: maySum (arithExpr? manyketpart? ('&&' bexp)? | '(' arithExpr? manyketpart? ('&&' bexp)? ')') | maySum (arithExpr '.')? sumspec | '(' sumspec ')';

maySum: TSum ID TIn crange ('on' '(' bexp ')')? '.';

asserting: 'assert' spec ';';

casting: '(' qty ')' locus ';';

varcreate : 'var' bindings ';' | 'var' typeOptionalBindings ':=' arithExpr ';';

assigning : idindices ':=' arithExpr ';';

ids : ID (',' ID)* ;

idindices: (ID | idindex) (',' (ID | idindex))*;

qassign : (locus | ID) '*=' (expr | dis) ';';

qcreate : 'var' (locus | ID) '*=' arithExpr ';';

measure : idindices '*=' 'measure' '(' (locus | ID) ')' ';' | idindices '*=' 'measure' '(' (locus | ID) ',' arithExpr ')' ';' ;

// see SWAPTest.qfy
measureAbort: ids '*=' 'measA' '(' (locus | ID) ')' ';' | ids '*=' 'measA' '(' (locus | ID) ',' arithExpr ')' ';';

return: Return ids ';';

break: 'break' ';';

ifexp: If ('(' bexp ')' | bexp) 'then'? '{' stmts '}' (Else '{' stmts '}')?;

cifexp : If bexp 'then' (arithExpr | '{' arithExpr '}') Else (arithExpr | '{' arithExpr '}');

// allows partspecs as as nodes in sum spec expressions (see test16.qfy for an example)
ketArithExpr: ketCifexp | partspec | '(' ketArithExpr ')';

// allows partspecs for sum spec expressions
ketCifexp: If bexp 'then' ketArithExpr 'else' ketArithExpr;

manyketpart: (ket | ketArithExpr | '(' arithExpr? ket (',' ket)* ')' | fcall)+;

forexp : 'for' ID TIn crange (('with' | '&&') bexp)? loopConds '{' stmts '}';

whileexp: 'while' ('(' bexp ')' | bexp) loopConds '{' stmts '}';

fcall : ID '^{-1}'? '(' arithExprsOrKets ')';

arithExprsOrKets : (arithExpr | ket) (',' (arithExpr | ket))*;

arithExpr: cifexp | arithAtomic op arithExpr | arithAtomic | arithExpr (index | slice | crange) | sumspec | qtypeCreate;

arithAtomic: numexp | ID | TSub arithExpr | boolLiteral
          | '(' arithExpr ')'
          | fcall |  absExpr | sinExpr | cosExpr | sqrtExpr | omegaExpr | notExpr | setInstance | qrange;

sinExpr : 'sin' '(' arithExpr ')' | 'sin' arithAtomic;

cosExpr : 'cos' '(' arithExpr ')' | 'cos' arithAtomic;

sqrtExpr : 'sqrt' '(' arithExpr ')' | 'sqrt' arithAtomic;

notExpr : 'not' '(' arithExpr ')';

absExpr : '|' arithExpr '|' ;

omegaExpr : ('ω' | 'omega') '(' arithExpr (',' arithExpr)? (',' arithExpr)? ')' ;

setInstance : '[' (arithExpr (',' arithExpr)*)? ']';

expr : SHad | SQFT | RQFT | lambdaT | ID;

lambdaT : 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' omegaExpr manyket ')'
       | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>'  manyket ')'
       | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' omegaExpr ')'
       | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' fcall ')'
       | 'λ' '^{-1}'? '(' (ids | '(' bindings ')') '=>' logicOrExp ')'; // phase kick-back

dis : 'dis' '(' expr ',' arithExpr ',' arithExpr ')';

manyket: (ket)+;

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

slice: '[' arithExpr? '..' arithExpr? ']';

idindex : ID index;

// rangeT: ID crange;

qrange: ID (index | crange)+;

// element : numexp | ID;

numexp: Number | TSub Number;
        
 // Lexical Specification of this Programming Language
 //  - lexical specification rules start with uppercase

typeT : baseTy | (baseTy | '(' baseTy (',' baseTy)* ')') '->' typeT;

baseTy : TNat | TReal | TInt | TBool | TBV | '[' baseTy ']' | 'array' '<' baseTy '>' | 'set' '<' baseTy '>' | '[' baseTy ','  arithExpr ']' | baseTy '[' arithExpr ']' | 'Q' '[' arithExpr ']';

qty : Nor | Had | En | En '(' arithExpr ')' | AA;

addOp: TAdd | TSub;

op : addOp | TDiv | TMul | TMod | OPlus | TExp | TXor | Dot;

boolLiteral: TrueLiteral | FalseLiteral;

//----------------------------------------------------------------
// Lexer Tokens (in ANTLR, all tokens start with capital letters)
//----------------------------------------------------------------

Axiom: 'axiom';

Function: 'function';

Lemma: 'lemma';

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
