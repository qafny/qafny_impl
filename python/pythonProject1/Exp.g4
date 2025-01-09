grammar Exp;

program: (method)+ EOF;

method: 'method' ('{' ':' Axiom '}')? ID '(' bindings ')' ('returns' returna)? conds '{' stmts '}';

returna: '(' bindings ')';

conds: (reen spec)*;

invariants : (Invariant spec)*;

stmts : (stmt)*;

stmt: asserting | casting | varcreate | assigning | qassign | measure | ifexp | forexp | fcall;

spec : qunspec | allspec | logicImply | chainBExp;

allspec : 'forall' binding '::' logicImply;

bexp: logicOrExp | qbool;

qbool: qindex | arithExpr comOp arithExpr '@' qindex | 'not' qbool;

logicImply: logicOrExp | logicOrExp '==>' logicImply;

logicOrExp: logicAndExp '||' logicOrExp | logicAndExp;

logicAndExp: logicNotExp '&&' logicAndExp | logicNotExp;

logicNotExp: 'not' logicNotExp | fcall | chainBExp;

chainBExp: arithExpr (comOp arithExpr)+;

comOp :  GE | LE | EQ | LT | GT;

qunspec : '{' locus ':' qty '↦' qspec '}' ;

qspec : tensorall | manyket | arithExpr manyket | sumspec | partspec;

partspec: 'part' '(' arithExpr ',' arithExpr ',' arithExpr ',' arithExpr ')';

tensorall : '⊗' ID '.' manyket | '⊗' ID TIn crange '.' manyket;

sumspec : maySum arithExpr manyket;

maySum : TSum ID TIn crange '.' (TSum ID TIn crange '.')*;

asserting: 'assert' spec ';';

casting: '(' qty ')' locus ';';

varcreate : 'var' binding ';' | 'var' binding ':=' arithExpr ';';

assigning : ID ':=' arithExpr ';';

ids : ID (',' ID)* ;

qassign : locus '*=' expr ';';

measure : ids '*=' 'measure' '(' locus ')' | ids '*=' 'measure' '(' locus ',' arithExpr ')' ;

ifexp: 'if' '(' bexp ')' '{' stmts '}' ;

forexp : 'for' ID TIn crange invariants '{' stmts '}';

fcall : ID '(' arithExprs ')';

arithExprs : arithExpr (',' arithExpr)*;

arithExpr: arithAtomic op arithExpr | arithAtomic;

arithAtomic: numexp | ID
          | '(' arithExpr ')'
          | fcall |  absExpr | sinExpr | cosExpr | sqrtExpr | omegaExpr | qindex;

sinExpr : 'sin' '(' arithExpr ')' ;

cosExpr : 'cos' '(' arithExpr ')' ;

sqrtExpr : 'sqrt' '(' arithExpr ')' ;

absExpr : '|' arithExpr '|' ;

omegaExpr : 'ω' '(' arithExpr ',' arithExpr ')' ;

expr : SHad | SQFT | RQFT | lambdaT;

lambdaT : 'λ' '(' ids '=>' omegaExpr manyket ')'
       | 'λ' '(' ids '=>'  manyket ')'
       | 'λ' '(' ids '=>' omegaExpr ')';

manyket: (ket)+;

ket : '|' qstate '⟩' | '⊗' arithExpr;

qstate: arithExpr | addOp;

bindings : binding ( ',' binding)*;

binding: ID ':' typeT;

locus : (qrange)+;

crange : '[' arithExpr ',' arithExpr ')';

index: '[' arithExpr ']';

qindex : ID index;

rangeT: ID crange ;

qrange: qindex | rangeT;

// element : numexp | ID;

numexp: Number | TSub Number;
        
 // Lexical Specification of this Programming Language
 //  - lexical specification rules start with uppercase

typeT : baseTy | baseTy '->' typeT ;

baseTy : TNat | TReal | TInt | TBool | '[' baseTy ','  arithExpr ']' | 'Q' '[' arithExpr ']';

qty : Nor | Had | En | En '(' arithExpr ')' | AA;

addOp: TAdd | TSub;

op : addOp | TDiv | TMul | TMod | OPlus | TExp;

//boolexp: TrueLiteral | FalseLiteral;

Axiom: 'axiom';

Function: 'function';

reen: Ensures | Requires;

Ensures : 'ensures';

Requires : 'requires';

Returns : 'returns';

Forall : 'forall';

TNat : 'nat';

TReal : 'real';

TInt : 'int';

TBool : 'bool';

TAdd : '+';

TSub : '-';

TMul : '*';

TDiv : '/';

TMod : '%';

TExp : '^';

Nor : 'nor';

Had : 'had';

SHad : 'H';

SQFT : 'QFT';

RQFT : 'RQFT';

AA : 'aa';

En : 'en';

If : 'if';

For : 'for';

TCl : 'λ';

TKet : '⟩';

TIn : 'in' | '∈';

TSum : 'Σ' | '∑';

 OPlus : '⊕';

 Invariant : 'invariant';

 Dot : '.' ;

 And : '&&';

 OR : '||';

 GE : '>=';

 LE : '<=';

 EQ : '==';

 LT : '<';
 
 GT : '>';

 ARROW : '=>';


 Number : DIGIT+ ;

 ID :   Letter LetterOrDigit*;

 Letter :   [a-zA-Z$_];

 LetterOrDigit: [a-zA-Z0-9$_];

 fragment DIGIT: ('0'..'9');

 AT : '@';
 ELLIPSIS : '...';
 WS  :  [ \t\r\n\u000C]+ -> skip;
 Comment :   '/*' .*? '*/' -> skip;
 Line_Comment :   '//' ~[\r\n]* -> skip;
