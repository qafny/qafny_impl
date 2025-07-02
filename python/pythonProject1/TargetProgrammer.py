import AbstractTargetVisitor


# Dafny's AST

class DXTop:

    def accept(self, visitor : AbstractTargetVisitor):
        pass

    def __repr__(self):
        return f'DXTop()'

class DXType(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass

    def __repr__(self):
        return f'DXType()'

class DXSpec(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass

    def __repr__(self):
        return f'DXSpec()'

class DXStmt(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass

    def __repr__(self):
        return f'DXStmt()'

class DXAExp(DXTop):
    '''Parent class of all arithmetic operations representable in Dafny'''

    def accept(self, visitor : AbstractTargetVisitor):
        pass

    def __repr__(self):
        return f'DXAExp()'

class DXConds(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass 

    def __repr__(self):
        return f'DXConds()'

class DXBool(DXSpec, DXType):

    def accept(self, visitor : AbstractTargetVisitor):
        pass 

    def __repr__(self):
        return f'DXBool()'

# SType could be bv1, real, nat,
class SType(DXType):

    def __init__(self, name: str, qafny_line_number: int = None):
        self._name = name
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSType(self)

    def type(self):
        return self._name

    def __repr__(self):
        return f'SType(name={self._name})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class SeqType(DXType):

    def __init__(self, ty: DXType, qafny_line_number: int = None):
        self._ty = ty
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSeqType(self)

    def type(self):
        return self._ty

    def __repr__(self):
        return f'SeqType(ty={self._ty})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class FunType(DXType):

    def __init__(self, left: DXType, right:DXType, qafny_line_number: int = None):
        self._left = left
        self._right = right
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSeqType(self)

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'FunType(left={self._left}, right={self._right})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXBin(DXAExp):

    def __init__(self, op: str, left:DXAExp, right: DXAExp, qafny_line_number: int = None):
        self._op = op
        self._left = left
        self._right = right
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitBin(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'DXBin(op={self._op}, left={self._left}, right={self._right})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXIfExp(DXAExp):

    def __init__(self, bexp: DXBool, left:DXAExp, right: DXAExp, qafny_line_number: int = None):
        self._bexp = bexp
        self._left = left
        self._right = right
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitIfExp(self)

    def bexp(self):
        return self._bexp

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'DXIfExp(bexp={self._bexp}, left={self._left}, right={self._right})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXLogic(DXBool):

    def __init__(self, op: str, left: DXBool, right: DXBool, qafny_line_number: int = None):
        self._op = op
        self._left = left
        self._right = right
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitLogic(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'DXLogic(op={self._op}, left={self._left}, right={self._right})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXComp(DXBool):

    def __init__(self, op: str, left: DXAExp, right: DXAExp, qafny_line_number: int = None):
        self._op = op
        self._left = left
        self._right = right
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitComp(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'DXComp(op={self._op}, left={self._left}, right={self._right})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXUni(DXAExp):

    def __init__(self, op: str, next:DXAExp, qafny_line_number: int = None):
        self._op = op
        self._next = next
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitUni(self)

    def op(self):
        return self._op

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXUni(op={self._op}, next={self._next})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXCast(DXAExp):
    '''Represents a dafny cast, i.e. x as real'''

    def __init__(self, aexp: DXAExp, type: DXType, qafny_line_number: int = None):
        # <aexp> as <type>
        self._aexp = aexp
        self._type = type
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitCast(self)

    def aexp(self) -> DXAExp:
        return self._aexp

    def type(self) -> DXType:
        return self._type

    def __repr__(self):
        return f'DXCast(aexp={self._aexp}, type={self._type})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXNum(DXType, DXAExp):
    '''Represents an integer literal value for Dafny syntax.'''

    def __init__(self, num: int, qafny_line_number: int = None):
        self._num = num
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitNum(self)

    def num(self):
        return self._num

    def as_real(self):
        return DXReal(float(self._num))

    def __repr__(self):
        return f'DXNum(num={self._num})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXReal(DXType, DXAExp):
    '''Represents a real literal value for Dafny syntax'''

    def __init__(self, value: float, qafny_line_number: int = None):
        self._value = value
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitReal(self)

    def real(self):
        return self._value

    def as_num(self) -> DXNum:
        return DXNum(int(self._value))

    def __repr__(self):
        return f'DXReal(value={self._value})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXNot(DXBool):

    def __init__(self, next: DXBool, qafny_line_number: int = None):
        self._next = next
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitNot(self)

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXNot(next={self._next})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXBind(DXAExp):

    def __init__(self, id: str, ty: DXType = None, num: int = None, qafny_line_number: int = None):
        self._id = id
        self._type = ty
        self._num = num
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitBind(self)

    def ID(self):
        return str(self._id)

    def type(self):
        return self._type

    def num(self):
        return self._num

    def newBind(self):
        if self._num is None:
            return DXBind(self._id, self._type, 0)
        else:
            return DXBind(self._id, self._type, self._num + 1)

    def __repr__(self):
        return f'DXBind(id={self._id}, type={self._type}, num={self._num})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXVar(DXBind):

    def __init__(self, id : str, ty: DXType = None, qafny_line_number: int = None):
        self._id = id
        self._type = ty
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitVar(self)

    def ID(self):
        return self._id

    def type(self):
        return self._type

    def __repr__(self):
        return f'DXVar(id={self._id}, type={self._type})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXList(DXAExp):

    def __init__(self, exprs: [DXAExp] = [], qafny_line_number: int = None):
        self._exprs = exprs
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitList(self)

    def exprs(self):
        return self._exprs

    def __repr__(self):
        return f'DXList(exprs={self._exprs})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXLength(DXAExp):

    def __init__(self, var : DXVar, qafny_line_number: int = None):
        self._var = var
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitLength(self)

    def var(self):
        return self._var

    def __repr__(self):
        return f'DXLength(var={self._var})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXRequires(DXConds):

    def __init__(self, spec: DXSpec, qafny_line_number: int = None):
        self._spec = spec
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitRequires(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXRequires(spec={self._spec})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXEnsures(DXConds):

    def __init__(self, spec: DXSpec, qafny_line_number: int = None):
        self._spec = spec
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitEnsures(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXEnsures(spec={self._spec})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXCall(DXStmt, DXAExp):

    def __init__(self, id: str, exps: [DXAExp], end: bool = False, qafny_line_number: int = None):
        self._id = id
        self._exps = exps
        self._end = end #variable to check if this is just a function call without assignment so that we can add a semi-colon at the end in PrinterVisitor
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitCall(self)

    def ID(self):
        return self._id

    def exps(self):
        return self._exps

    def end(self):
        return self._end

    def __repr__(self):
        return f'DXCall(id={self._id}, exps={self._exps}, end={self._end})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXInit(DXStmt):

    def __init__(self, binding: DXBind, exp: DXAExp = None, qafny_line_number: int = None):
        self._binding = binding
        self._exp = exp
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitInit(self)

    def binding(self):
        return self._binding

    def exp(self):
        return self._exp

    def __repr__(self):
        return f'DXInit(binding={self._binding}, exp={self._exp})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXIndex(DXAExp):

    def __init__(self, id: DXAExp, index: DXAExp, qafny_line_number: int = None):
        self._id = id
        self._index = index
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitIndex(self)

    def bind(self):
        return self._id 

    def index(self):
        return self._index

    def __repr__(self):
        return f'DXIndex(id={self._id}, index={self._index})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXCast(DXAExp):

    def __init__(self, type: SType, next: DXAExp, qafny_line_number: int = None):
        self._type = type
        self._next = next
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitCast(self)

    def type(self):
        return self._type

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXCast(type={self._type}, next={self._next})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXInRange(DXBool):

    def __init__(self, x: DXBind, left: DXAExp, right: DXAExp, qafny_line_number: int = None):
        self._id = x
        self._left = left
        self._right = right
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitInRange(self)

    def bind(self):
        return self._id

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'DXInRange(id={self._id}, left={self._left}, right={self._right})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXAll(DXBool, DXSpec):

    def __init__(self, bind: DXBind, next: DXSpec, qafny_line_number: int = None):
        self._bind = bind
        self._next = next
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitAll(self)

    def bind(self):
        return self._bind

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXAll(bind={self._bind}, next={self._next})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXWhile(DXStmt):

    def __init__(self, cond: DXBool, stmts: [DXStmt], inv: [DXSpec] = None, qafny_line_number: int = None):
        self._cond = cond
        self._stmts = stmts
        self._inv = inv
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitWhile(self)

    def cond(self):
        return self._cond

    def stmts(self):
        return self._stmts

    def inv(self):
        return self._inv

    def __repr__(self):
        return f'DXWhile(cond={self._cond}, stmts={self._stmts}, inv={self._inv})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXIf(DXStmt):

    def __init__(self, cond: DXBool, left: [DXStmt], right:[DXStmt], qafny_line_number: int = None):
        self._cond = cond
        self._left = left
        self._right = right
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitIf(self)

    def cond(self):
        return self._cond

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'DXIf(cond={self._cond}, left={self._left}, right={self._right})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXAssert(DXStmt):

    def __init__(self, spec: DXSpec, qafny_line_number: int = None):
        self._spec = spec
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitAssert(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXAssert(spec={self._spec})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXAssign(DXStmt):

    def __init__(self, ids: [DXAExp], exp : DXAExp, init: bool = None, qafny_line_number: int = None):
        self._ids = ids
        self._exp = exp
        self._init = init
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitAssign(self)

    def ids(self):
        return self._ids

    def exp(self):
        return self._exp

    def init(self):
        return self._init

    def __repr__(self):
        return f'DXAssign(ids={self._ids}, exp={self._exp}, init={self._init})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXMethod(DXTop):

    def __init__(self, id: str, axiom: bool, bindings: [DXBind], returns : [DXBind], conds: [DXConds], stmts: [DXStmt], qafny_line_number: int = None):
        self._id = id
        self._axiom = axiom
        self._bindings = bindings
        self._returns = returns
        self._conds = conds
        self._stmts = stmts
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitMethod(self)

    def ID(self):
        return self._id

    def axiom(self):
        return self._axiom

    def bindings(self):
        return self._bindings

    def returns(self):
        return self._returns

    def conds(self):
        return self._conds

    def stmts(self):
        return self._stmts

    def __repr__(self):
        return f'DXMethod(id={self._id}, axiom={self._axiom}, bindings={self._bindings}, returns={self._returns}, conds={self._conds}, stmts={self._stmts})'
    
    def qafny_line_number(self):
        return self._qafny_line_number

class DXProgram(DXTop):

    def __init__(self, exps: [DXMethod], qafny_line_number: int = None):
        self._exps = exps
        self._qafny_line_number = qafny_line_number

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitProgram(self)

    def method(self):
        return self._exps

    def __repr__(self):
        return f'DXProgram(exps={self._exps})'
    
    def qafny_line_number(self):
        return self._qafny_line_number