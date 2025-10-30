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

    def __init__(self, name: str, line: int = None):
        self._name = name
        self._line_number = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSType(self)

    def type(self):
        return self._name

    def __repr__(self):
        return f'SType(name={self._name})'
    
    def line(self):
        return self._line

class SeqType(DXType):

    def __init__(self, ty: DXType, line: int = None):
        self._ty = ty
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSeqType(self)

    def type(self):
        return self._ty

    def __repr__(self):
        return f'SeqType(ty={self._ty})'
    
    def line(self):
        return self._line

class FunType(DXType):

    def __init__(self, left: DXType, right:DXType, line: int = None):
        self._left = left
        self._right = right
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitFunType(self)

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'FunType(left={self._left}, right={self._right})'
    
    def line(self):
        return self._line

class DXBin(DXAExp):

    def __init__(self, op: str, left:DXAExp, right: DXAExp, line: int = None):
        self._op = op
        self._left = left
        self._right = right
        self._line = line

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
    
    def line(self):
        return self._line

class DXIfExp(DXAExp):

    def __init__(self, bexp: DXBool, left:DXAExp, right: DXAExp, line: int = None):
        self._bexp = bexp
        self._left = left
        self._right = right
        self._line = line

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
    
    def line(self):
        return self._line

class DXLogic(DXBool):

    def __init__(self, op: str, left: DXBool, right: DXBool, line: int = None):
        self._op = op
        self._left = left
        self._right = right
        self._line = line

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
    
    def line(self):
        return self._line

class DXComp(DXBool):

    def __init__(self, op: str, left: DXAExp, right: DXAExp, line: int = None):
        self._op = op
        self._left = left
        self._right = right
        self._line = line

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
    
    def line(self):
        return self._line

class DXUni(DXAExp):

    def __init__(self, op: str, next:DXAExp, line: int = None):
        self._op = op
        self._next = next
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitUni(self)

    def op(self):
        return self._op

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXUni(op={self._op}, next={self._next})'
    
    def line(self):
        return self._line

# class DXCast(DXAExp):
#     '''Represents a dafny cast, i.e. x as real'''

#     def __init__(self, aexp: DXAExp, type: DXType, line: int = None):
#         # <aexp> as <type>
#         self._aexp = aexp
#         self._type = type
#         self._line = line

#     def accept(self, visitor: AbstractTargetVisitor):
#         return visitor.visitCast(self)

#     def aexp(self) -> DXAExp:
#         return self._aexp

#     def type(self) -> DXType:
#         return self._type

#     def __repr__(self):
#         return f'DXCast(aexp={self._aexp}, type={self._type})'
    
#     def line(self):
#         return self._line

class DXNum(DXType, DXAExp):
    '''Represents an integer literal value for Dafny syntax.'''

    def __init__(self, val: int|float, line: int = None):
        self._val = val
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitNum(self)

    def val(self):
        return self._val

    def as_real(self):
        return DXReal(float(self._val))

    def __repr__(self):
        return f'DXNum(val={self._val})'
    
    def line(self):
        return self._line

class DXReal(DXType, DXAExp):
    '''Represents a real literal value for Dafny syntax'''

    def __init__(self, value: float, line: int = None):
        self._value = value
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitReal(self)

    def real(self):
        return self._value

    def as_num(self) -> DXNum:
        return DXNum(int(self._value))

    def __repr__(self):
        return f'DXReal(value={self._value})'
    
    def line(self):
        return self._line

class DXNot(DXBool):

    def __init__(self, next: DXBool, line: int = None):
        self._next = next
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitNot(self)

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXNot(next={self._next})'
    
    def line(self):
        return self._line

class DXBind(DXAExp):

    def __init__(self, id: str, ty: DXType = None, num: int = None, line: int = None):
        self._id = id
        self._type = ty
        self._num = num
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitBind(self)

    def ID(self):
        return str(self._id)

    def type(self):
        return self._type

    def num(self):
        return self._num

    def newBind(self, n:int):
        return DXBind(self._id, self._type, n, self._line)


    def newBindType(self, t:DXType, n:int):
        return DXBind(self._id, t, n, self._line)

    def __repr__(self):
        return f'DXBind(id={self._id}, type={self._type}, num={self._num})'
    
    def line(self):
        return self._line

class DXBoolValue(DXBool):

    def __init__(self, v:bool, line: int = None):
        self._bool = v

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitBind(self)

    def value(self):
        return self._bool

    def __repr__(self):
        return f'DXBoolValue(value={self._bool})'
    
    def line(self):
        return self._line
    


class DXList(DXAExp):

    def __init__(self, exprs: [DXAExp] = [], line: int = None):
        self._exprs = exprs
        self._line = line

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitList(self)

    def exprs(self):
        return self._exprs

    def __repr__(self):
        return f'DXList(exprs={self._exprs})'
    
    def line(self):
        return self._line

class DXLength(DXAExp):

    def __init__(self, var : DXAExp, line: int = None):
        self._var = var
        self._line = line

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitLength(self)

    def var(self):
        return self._var

    def __repr__(self):
        return f'DXLength(var={self._var})'
    
    def line(self):
        return self._line

class DXRequires(DXConds):

    def __init__(self, spec: DXSpec, line: int = None):
        self._spec = spec
        self._line = line

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitRequires(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXRequires(spec={self._spec})'
    
    def line(self):
        return self._line

class DXEnsures(DXConds):

    def __init__(self, spec: DXSpec, line: int = None):
        self._spec = spec
        self._line = line

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitEnsures(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXEnsures(spec={self._spec})'
    
    def line(self):
        return self._line

class DXCall(DXStmt, DXAExp):

    def __init__(self, id: str, exps: [DXAExp], end: bool = False, line: int = None):
        self._id = id
        self._exps = exps
        self._end = end #variable to check if this is just a function call without assignment so that we can add a semi-colon at the end in PrinterVisitor
        self._line = line

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
    
    def line(self):
        return self._line

class DXInit(DXStmt):

    def __init__(self, binding: DXBind, exp: DXAExp = None, line: int = None):
        self._binding = binding
        self._exp = exp
        self._line = line

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitInit(self)

    def binding(self):
        return self._binding

    def exp(self):
        return self._exp

    def __repr__(self):
        return f'DXInit(binding={self._binding}, exp={self._exp})'
    
    def line(self):
        return self._line

class DXIndex(DXAExp):

    def __init__(self, id: DXAExp, index: DXAExp, line: int = None):
        self._id = id
        self._index = index
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitIndex(self)

    def bind(self):
        return self._id 

    def index(self):
        return self._index

    def __repr__(self):
        return f'DXIndex(id={self._id}, index={self._index})'
    
    def line(self):
        return self._line
    
class DXSlice(DXAExp):

    def __init__(self, id: DXAExp, low: DXAExp | None, high: DXAExp | None, line: int = None):
        self._id = id
        self._low = low
        self._high = high
        self._line = line

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitSlice(self)

    def bind(self):
        return self._id

    def low(self):
        return self._low

    def high(self):
        return self._high

    def __repr__(self):
        return f'DXSlice(id={self._id}, low={self._low}, high={self._high})'
    
    def line(self):
        return self._line

class DXCast(DXAExp):

    def __init__(self, type: SType, next: DXAExp, line: int = None):
        self._type = type
        self._next = next
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitCast(self)

    def type(self):
        return self._type

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXCast(type={self._type}, next={self._next})'
    
    def line(self):
        return self._line

class DXInRange(DXBool):

    def __init__(self, x: DXBind, left: DXAExp, right: DXAExp, line: int = None):
        self._id = x
        self._left = left
        self._right = right
        self._line = line

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
    
    def line(self):
        return self._line

class DXAll(DXBool, DXSpec):

    def __init__(self, bind: DXBind, next: DXSpec, line: int = None):
        self._bind = bind
        self._next = next
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitAll(self)

    def bind(self):
        return self._bind

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXAll(bind={self._bind}, next={self._next})'
    
    def line(self):
        return self._line

class DXWhile(DXStmt):

    def __init__(self, cond: DXBool, stmts: [DXStmt], inv: [DXSpec] = None, line: int = None):
        self._cond = cond
        self._stmts = stmts
        self._inv = inv
        self._line = line

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
    
    def line(self):
        return self._line

class DXIf(DXStmt):

    def __init__(self, cond: DXBool, left: [DXStmt], right:[DXStmt], line: int = None):
        self._cond = cond
        self._left = left
        self._right = right
        self._line = line

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
    
    def line(self):
        return self._line

class DXAssert(DXStmt):

    def __init__(self, spec: DXSpec, line: int = None):
        self._spec = spec
        self._line = line

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitAssert(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXAssert(spec={self._spec})'
    
    def line(self):
        return self._line

class DXAssign(DXStmt):

    def __init__(self, ids: [DXAExp], exp : DXAExp, init: bool = True, line: int = None):
        self._ids = ids
        self._exp = exp
        self._init = init
        self._line = line

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
    
    def line(self):
        return self._line

class DXMethod(DXTop):

    def __init__(self, id: str, axiom: bool, bindings: [DXBind], returns : [DXBind], conds: [DXConds], stmts: [DXStmt], is_function: False, line: int = None):
        self._id = id
        self._axiom = axiom
        self._bindings = bindings
        self._returns = returns
        self._conds = conds
        self._stmts = stmts
        self._is_function = is_function
        self._line = line

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
    
    def is_function(self):
        return self._is_function

    def __repr__(self):
        if self._is_function:
            return f'DXFunction(id={self._id}, axiom={self._axiom}, bindings={self._bindings}, returns={self._returns}, conds={self._conds}, stmts={self._stmts})'
        else:
            return f'DXMethod(id={self._id}, axiom={self._axiom}, bindings={self._bindings}, returns={self._returns}, conds={self._conds}, stmts={self._stmts})'
    
    def line(self):
        return self._line

class DXProgram(DXTop):

    def __init__(self, exps: [DXMethod], line: int = None):
        self._exps = exps
        self._line = line

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitProgram(self)

    def method(self):
        return self._exps

    def __repr__(self):
        return f'DXProgram(exps={self._exps})'
    
    def line(self):
        return self._line
    
class DXSeqComp(DXAExp):
    def __init__(self, size: DXLength|DXBind, idx: DXBind, spec: DXRequires | None, lambd: DXIf | DXIndex):
        self._size = size
        self._idx = idx
        self._spec = spec
        self._lambd = lambd
    
    def accept(self, visitor):
        return visitor.visitSeqComp(self)

    def size(self):
        return self._size
    
    def idx(self):
        return self._idx
    
    def spec(self):
        return self._spec
    
    def lambd(self):
        return self._lambd
    
    def __repr__(self):
        return f'DXSeqComp(length={self._size}, value={self._lambd})'

class DXWitness(DXAExp):
    def __init__(self, bind: DXBind, constrs: DXAExp, init: bool = None):
        self._bind = bind
        self._constrs = constrs
        self._init = init
    
    def accept(self, visitor):
        return visitor.visitWitness(self)

    def bind(self):
        return self._bind
    
    def init(self):
        return self._init
    
    def constrs(self):
        return self._constrs
    
    # def line(self):
    #     return self._line
    
    def __repr__(self):
        return f'DXWitness(bind={self._bind}, constrs={self._constrs}, init={self._init})'
