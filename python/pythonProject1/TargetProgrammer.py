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

    def __init__(self, name: str):
        self._name = name

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSType(self)

    def type(self):
        return self._name

    def __repr__(self):
        return f'SType(name={self._name})'

class SeqType(DXType):

    def __init__(self, ty: DXType):
        self._ty = ty

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSeqType(self)

    def type(self):
        return self._ty

    def __repr__(self):
        return f'SeqType(ty={self._ty})'

class FunType(DXType):

    def __init__(self, left: DXType, right:DXType):
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSeqType(self)

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f'FunType(left={self._left}, right={self._right})'

class DXBin(DXAExp):

    def __init__(self, op: str, left:DXAExp, right: DXAExp):
        self._op = op
        self._left = left
        self._right = right

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

class DXIfExp(DXAExp):

    def __init__(self, bexp: DXBool, left:DXAExp, right: DXAExp):
        self._bexp = bexp
        self._left = left
        self._right = right

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

class DXLogic(DXBool):

    def __init__(self, op: str, left: DXBool, right: DXBool):
        self._op = op
        self._left = left
        self._right = right

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

class DXComp(DXBool):

    def __init__(self, op: str, left: DXAExp, right: DXAExp):
        self._op = op
        self._left = left
        self._right = right

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

class DXUni(DXAExp):

    def __init__(self, op: str, next:DXAExp):
        self._op = op
        self._next = next

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitUni(self)

    def op(self):
        return self._op

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXUni(op={self._op}, next={self._next})'

class DXCast(DXAExp):
    '''Represents a dafny cast, i.e. x as real'''

    def __init__(self, aexp: DXAExp, type: DXType):
        # <aexp> as <type>
        self._aexp = aexp
        self._type = type

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitCast(self)

    def aexp(self) -> DXAExp:
        return self._aexp

    def type(self) -> DXType:
        return self._type

    def __repr__(self):
        return f'DXCast(aexp={self._aexp}, type={self._type})'

class DXNum(DXType, DXAExp):
    '''Represents an integer literal value for Dafny syntax.'''

    def __init__(self, num: int):
        self._num = num

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitNum(self)

    def num(self):
        return self._num

    def as_real(self):
        return DXReal(float(self._num))

    def __repr__(self):
        return f'DXNum(num={self._num})'

class DXReal(DXType, DXAExp):
    '''Represents a real literal value for Dafny syntax'''

    def __init__(self, value: float):
        self._value = value

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitReal(self)

    def real(self):
        return self._value

    def as_num(self) -> DXNum:
        return DXNum(int(self._value))

    def __repr__(self):
        return f'DXReal(value={self._value})'

class DXNot(DXBool):

    def __init__(self, next: DXBool):
        self._next = next

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitNot(self)

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXNot(next={self._next})'

class DXBind(DXAExp):

    def __init__(self, id: str, ty: DXType = None, num: int = None):
        self._id = id
        self._type = ty
        self._num = num

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

class DXVar(DXBind):

    def __init__(self, id : str, ty: DXType = None):
        self._id = id
        self._type = ty

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitVar(self)

    def ID(self):
        return self._id

    def type(self):
        return self._type

    def __repr__(self):
        return f'DXVar(id={self._id}, type={self._type})'

class DXList(DXAExp):

    def __init__(self, exprs: [DXAExp] = []):
        self._exprs = exprs

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitList(self)

    def exprs(self):
        return self._exprs

    def __repr__(self):
        return f'DXList(exprs={self._exprs})'

class DXLength(DXAExp):

    def __init__(self, var : DXVar):
        self._var = var

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitLength(self)

    def var(self):
        return self._var

    def __repr__(self):
        return f'DXLength(var={self._var})'

class DXRequires(DXConds):

    def __init__(self, spec: DXSpec):
        self._spec = spec

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitRequires(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXRequires(spec={self._spec})'

class DXEnsures(DXConds):

    def __init__(self, spec: DXSpec):
        self._spec = spec

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitEnsures(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXEnsures(spec={self._spec})'

class DXCall(DXStmt, DXAExp):

    def __init__(self, id: str, exps: [DXAExp], end: bool = False):
        self._id = id
        self._exps = exps
        self._end = end #variable to check if this is just a function call without assignment so that we can add a semi-colon at the end in PrinterVisitor

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

class DXInit(DXStmt):

    def __init__(self, binding: DXBind, exp: DXAExp = None):
        self._binding = binding
        self._exp = exp

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitInit(self)

    def binding(self):
        return self._binding

    def exp(self):
        return self._exp

    def __repr__(self):
        return f'DXInit(binding={self._binding}, exp={self._exp})'

class DXIndex(DXAExp):

    def __init__(self, id: DXAExp, index: DXAExp):
        self._id = id
        self._index = index

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitIndex(self)

    def bind(self):
        return self._id 

    def index(self):
        return self._index

    def __repr__(self):
        return f'DXIndex(id={self._id}, index={self._index})'

class DXCast(DXAExp):

    def __init__(self, type: SType, next: DXAExp):
        self._type = type
        self._next = next

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitCast(self)

    def type(self):
        return self._type

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXCast(type={self._type}, next={self._next})'

class DXInRange(DXBool):

    def __init__(self, x: DXBind, left: DXAExp, right: DXAExp):
        self._id = x
        self._left = left
        self._right = right

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

class DXAll(DXBool, DXSpec):

    def __init__(self, bind: DXBind, next: DXSpec):
        self._bind = bind
        self._next = next

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitAll(self)

    def bind(self):
        return self._bind

    def next(self):
        return self._next

    def __repr__(self):
        return f'DXAll(bind={self._bind}, next={self._next})'

class DXWhile(DXStmt):

    def __init__(self, cond: DXBool, stmts: [DXStmt], inv: [DXSpec] = None):
        self._cond = cond
        self._stmts = stmts
        self._inv = inv

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

class DXIf(DXStmt):

    def __init__(self, cond: DXBool, left: [DXStmt], right:[DXStmt]):
        self._cond = cond
        self._left = left
        self._right = right

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

class DXAssert(DXStmt):

    def __init__(self, spec: DXSpec):
        self._spec = spec

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitAssert(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f'DXAssert(spec={self._spec})'

class DXAssign(DXStmt):

    def __init__(self, ids: [DXAExp], exp : DXAExp, init: bool = None):
        self._ids = ids
        self._exp = exp
        self._init = init

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

class DXMethod(DXTop):

    def __init__(self, id: str, axiom: bool, bindings: [DXBind], returns : [DXBind], conds: [DXConds], stmts: [DXStmt]):
        self._id = id
        self._axiom = axiom
        self._bindings = bindings
        self._returns = returns
        self._conds = conds
        self._stmts = stmts

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

class DXProgram(DXTop):

    def __init__(self, exps: [DXMethod]):
        self._exps = exps

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitProgram(self)

    def method(self):
        return self._exps

    def __repr__(self):
        return f'DXProgram(exps={self._exps})'