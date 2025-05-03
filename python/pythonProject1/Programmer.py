import AbstractProgramVisitor

import utils # for make_repr(...)

# Qafny's AST


class QXTop:
    """Parent class of all Qafny tree nodes"""

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class QXType(QXTop):
    """QXType refers kinds.
    """

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class QXQExp(QXTop):

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class QXHad(QXQExp):

    def __init__(self, state: str):
        self._state = state

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitHad(self)

    def state(self):
        return self._state

    def __repr__(self):
        return f"QXHad(state={repr(str(self._state))})"


class QXAExp(QXQExp, QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class TyArray(QXType):

    def __init__(self, ty: QXType, flag: QXAExp):
        self._type = ty
        self._flag = flag

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSingleT(self)

    def type(self):
        return self._type

    def num(self):
        return self._flag

    def __repr__(self):
        return f"TyArray(ty={self._type}, flag={self._flag})"


class TySet(QXType):
    '''
    Represents a set in dafny: set<xxx>
    '''

    def __init__(self, ty: QXType):
        self.__type = ty

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitSingleT(self)

    def type(self):
        return self.__type

    def __repr__(self):
        return f'TySet(ty={self.__type})'


class TySingle(QXType):

    def __init__(self, name: str):
        self._name = name

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSingleT(self)

    def type(self):
        return str(self._name) if self._name else None

    def __repr__(self):
        return f"TySingle(name={repr(str(self._name))})"


class TyQ(QXType):

    def __init__(self, flag: QXAExp):
        self._flag = flag

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQ(self)

    def flag(self):
        return self._flag

    def __repr__(self):
        return f"TyQ(flag={self._flag})"


class TyFun(QXType):

    def __init__(self, parameters: [QXType], return_type: QXType):
        self._left = parameters
        self._right = return_type

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitFun(self)

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"TyFun(left={self._left}, right={self._right})"


class QXQTy(QXTop):
    """QXQTy refers to actual quantum types
    """

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class TyHad(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitTyHad(self)

    def __repr__(self):
        return f"TyHad()"


class TyEn(QXQTy):

    def __init__(self, flag: QXAExp):
        self._flag = flag

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitEn(self)

    def flag(self):
        return self._flag

    def __repr__(self):
        return f"TyEn(flag={self._flag})"

# Specialized version of the EN type where the grouping of basis vectors are important
class TyAA(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitAA(self)

    def __repr__(self):
        return f"TyAA()"


class TyNor(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitNor(self)

    def __repr__(self):
        return f"TyNor()"


class QXBExp(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXSpec(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass

class QXCond(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXBind(QXAExp):

    def __init__(self, id: str, ty: QXType = None):
        self._id = id
        self._type = ty

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitBind(self)

    def ID(self):
        # return self._id if self._id is str else self._id.getText()
        return self._id if isinstance(self._id, str) else self._id.getText()

    def type(self):
        return self._type

    def __repr__(self):
        return utils.make_repr('QXBind', {'id': self._id, 'ty': self._type})


class QXBool(QXBExp, QXSpec):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXLogic(QXBool):

    def __init__(self, op: str, left: QXBool, right: QXBool):
        self._op = op
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitLogic(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"QXLogic(op={repr(str(self._op))}, left={self._left}, right={self._right})"


class QXCNot(QXBool):

    def __init__(self, next: QXBool):
        self._next = next

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitCNot(self)

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXCNot(next={self._next})"


class QXComp(QXBool):

    def __init__(self, op: str, left: QXAExp, right: QXAExp):
        self._op = op
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitBool(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"QXComp(op={repr(str(self._op))}, left={self._left}, right={self._right})"


class QXAll(QXSpec):

    def __init__(self, bind: QXBind, next: QXSpec):
        self._bind = bind
        self._next = next

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitAll(self)

    def bind(self):
        return self._bind

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXAll(bind={self._bind}, next={self._next})"


class QXQBool(QXBExp):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXQIndex(QXQBool, QXAExp):

    def __init__(self, id: str, index: QXAExp):
        self._id = id
        self._index = index

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQIndex(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def index(self):
        return self._index

    def __repr__(self):
        return f"QXQindex(id={repr(str(self._id))}, index={self._index})"

class QXIfExp(QXAExp):

    def __init__(self, bexp: QXLogic, left: QXAExp, right: QXAExp):
        self._bexp = bexp
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitIfExp(self)

    def bexp(self):
        return self._bexp

    def left(self):
        return self._left

    def right(self):
        return self._right

class QXBin(QXAExp):

    def __init__(self, op: str, left: QXAExp, right: QXAExp):
        self._op = op
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitBin(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"QXBin(op={repr(str(self._op))}, left={self._left}, right={self._right})"


class QXUni(QXAExp):

    def __init__(self, op: str, next:QXAExp):
        self._op = op
        self._next = next

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitUni(self)

    def op(self):
        return self._op

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXUni(op={repr(str(self._op))}, next={self._next})"


class QXNum(QXAExp):

    def __init__(self, num: int):
        self._num = num

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitNum(self)

    def num(self):
        return self._num

    def __repr__(self):
        return f"QXNum(num={self._num})"


class QXBoolLiteral(QXAExp):

    def __init__(self, value: bool):
        self._value = value

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitBoolLiteral(self)

    def value(self):
        return self._value

    def __repr__(self):
        return f'QXBoolLiteral(value={self._value})'


class QXQComp(QXQBool):

    def __init__(self, op: str, left:QXAExp, right: QXAExp, index: QXQIndex):
        self._op = op
        self._left = left
        self._right = right
        self._index = index

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQComp(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def index(self):
        return self._index

    def __repr__(self):
        return f"QXQComp(op={repr(str(self._op))}, left={self._left}, right={self._right}, index={self._index})"


class QXQNot(QXQBool):

    def __init__(self, next: QXQBool):
        self._next = next

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXQNot(next={self._next})"


class QXExp(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXSingle(QXExp):

    def __init__(self, op: str):
        self._op = op

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSingle(self)

    def op(self):
        return self._op

    def __repr__(self):
        return f"QXSingle(op={repr(str(self._op))})"


class QXKet(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXSKet(QXKet):

    def __init__(self, vector: QXQExp):
        self._vector = vector

    def vector(self):
        return self._vector

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSKet(self)

    def __repr__(self):
        return f"QXSKet(vector={self._vector})"


class QXVKet(QXKet):

    def __init__(self, vector: QXAExp):
        self._vector = vector

    def vector(self):
        return self._vector

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitVKet(self)

    def __repr__(self):
        return f"QXVKet(vector={self._vector})"


class QXOracle(QXExp):

    def __init__(self, ids: [str], omega: QXAExp, kets: [QXKet]):
        self._ids = [str(x) for x in ids]
        self._omega = omega
        self._kets = kets

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitOracle(self)

    def ids(self):
        return self._ids

    def phase(self):
        return self._omega

    def vectors(self):
        return self._kets

    def __repr__(self):
        return f"QXOracle(ids={self._ids}, omega={self._omega}, kets={self._kets})"


class QXCRange(QXTop):

    def __init__(self, left: QXAExp, right: QXAExp):
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitCRange(self)

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"QXCRange(left={self._left}, right={self._right})"


class QXQRange(QXTop):

    def __init__(self, id: str, cranges: [QXCRange]):
        self._id = id
        self._cranges = cranges

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQRange(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def cranges(self):
        return self._cranges

    def __repr__(self):
        return f"QXQRange(id={repr(str(self._id))}, cranges={self._cranges})"


class QXQState(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXCon(QXTop):

    def __init__(self, id: str, crange: QXCRange):
        self._id = id
        self._crange = crange

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def range(self):
        return self._crange

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCon(self)

    def __repr__(self):
        return f"QXCon(id={repr(str(self._id))}, crange={self._crange})"


class QXTensor(QXQState):

    def __init__(self, kets: [QXKet], id: str = None, crange: QXCRange = None, amp: QXAExp = None):
        self._kets = kets
        self._id = id
        self._crange = crange
        self._amp = amp

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitTensor(self)

    def ID(self):
        return str(self._id) if self._id else None

    def range(self):
        return self._crange

    def kets(self):
        return self._kets

    def amp(self):
        return self._amp

    def __repr__(self):
        return f"QXTensor(kets={self._kets}, id={repr(str(self._id))}, crange={self._crange}, amp={self._amp})"


class QXSum(QXQState):

    def __init__(self, sums: [QXCon], amp: QXAExp, kets: [QXKet]):
        self._sums = sums
        self._amp = amp
        self._kets = kets

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSum(self)

    def sums(self):
        return self._sums

    def amp(self):
        return self._amp

    def kets(self):
        return self._kets

    def __repr__(self):
        return f"QXSum(sums={self._sums}, amp={self._amp}, kets={self._kets})"


class QXPart(QXQState):

    def __init__(self, num : QXAExp, fname: QXAExp, tamp: QXAExp, famp : QXAExp):
        self._num = num
        self._fname = fname
        self._tamp = tamp
        self._famp = famp

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitPart(self)

    def qnum(self):
        return self._num

    def fname(self):
        return self._fname

    def trueAmp(self):
        return self._tamp

    def falseAmp(self):
        return self._famp

    def __repr__(self):
        return f"QXPart(num={self._num}, fname={self._fname}, tamp={self._tamp}, famp={self._famp})"


class QXQSpec(QXSpec):

    def __init__(self, locus: [QXQRange], qty: QXType, states: [QXQState]):
        self._locus = locus
        self._qty = qty
        self._states = states

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitQSpec(self)

    def locus(self):
        return self._locus

    def qty(self):
        return self._qty

    def states(self):
        return self._states

    def __repr__(self):
        return f"QXQSpec(locus={self._locus}, qty={self._qty}, states={self._states})"


class QXRequires(QXCond):

    def __init__(self, spec: QXSpec):
        self._spec = spec

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitRequires(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXRequires(spec={self._spec})"


class QXEnsures(QXCond):

    def __init__(self, spec: QXSpec):
        self._spec = spec

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitEnsures(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXEnsures(spec={self._spec})"


class QXInvariant(QXCond):

    def __init__(self, spec: QXSpec):
        self._spec = spec

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitInvariant(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXInvariant(spec={self._spec})"


class QXDecreases(QXCond):
    '''
    Represents a decreases loop invariant.
    example:
    ...
    while ci_ub - ci_lb > 2 * eps
        ╭─────────────────────────╮
        │ decreases ci_ub - ci_lb │
        ╰─────────────────────────╯
        invariant i < |K| && i < |k|
    ...
    '''

    def __init__(self, arith_expr: QXAExp):
        self._arith_expr = arith_expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitDecreases(self)

    def aexp(self):
        return self._arith_expr

    def __repr__(self):
        return f"QXDecreases(arith_expr={self._arith_expr})"


class QXSeparates(QXCond):
    '''
    Represents a separates loop invariant.
    example:
    ...
    for i in [0, n) with q[i]
        ╭────────────────────────────╮
        │ separates q[0, i), p[0, n) │
        ╰────────────────────────────╯
        invariant {
    ...
    '''

    def __init__(self, locus: [QXQRange]):
        self._locus = locus

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitEnsures(self)

    def locus(self):
        return self._locus

    def __repr__(self):
        return f"QXSeparates(locus={self._spec})"


class QXStmt(QXTop):
    '''Parent class of all statements.'''

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class QXAssert(QXStmt):

    def __init__(self, spec: QXSpec):
        self._spec = spec

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitAssert(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXAssert(spec={self._spec})"


class QXCast(QXStmt):

    def __init__(self, qty :QXQTy, locus: [QXQRange]):
        self._qty = qty
        self._locus = locus

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCast(self)

    def qty(self):
        return self._qty

    def locus(self):
        return self._locus

    def __repr__(self):
        return f"QXCast(qty={self._qty}, locus={self._locus})"


class QXInit(QXStmt):

    def __init__(self, binding :QXBind):
        self._binding = binding

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitInit(self)

    def binding(self):
        return self._binding

    def __repr__(self):
        return f"QXInit(binding={self._binding})"


class QXCAssign(QXStmt):

    def __init__(self, id: str, expr : QXAExp):
        self._id = id
        self._expr = expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCAssign(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def aexp(self):
        return self._expr

    def __repr__(self):
        return f"QXCAssign(id={repr(str(self._id))}, expr={self._expr})"


class QXQAssign(QXStmt):

    def __init__(self, locus: [QXQRange], expr : QXExp):
        self._locus = locus
        self._expr = expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitQAssign(self)

    def locus(self):
        return self._locus

    def exp(self):
        return self._expr

    def __repr__(self):
        return f"QXQAssign(locus={self._locus}, expr={self._expr})"


class QXMeasure(QXStmt):

    def __init__(self, ids: [str], locus : [QXQRange], res: QXAExp = None):
        self._ids = ids
        self._locus = locus
        self._res = res

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitMeasure(self)

    def ids(self):
        return self._ids

    def locus(self):
        return self._locus

    def res(self):
        return self._res

    def __repr__(self):
        return f"QXMeasure(ids={self._ids}, locus={self._locus}, res={self._res})"


class QXIf(QXStmt):

    def __init__(self, bexp: QXBExp, stmts : [QXStmt]):
        self._bexp = bexp
        self._stmts = stmts

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitIf(self)

    def bexp(self):
        return self._bexp

    def stmts(self):
        return self._stmts

    def __repr__(self):
        return f"QXIf(bexp={self._bexp}, stmts={self._stmts})"


class QXFor(QXStmt):

    def __init__(self, id: str, crange: QXCRange, invs: [QXSpec], stmts: [QXStmt]):
        self._id = id
        self._crange = crange
        self._invs = invs
        self._stmts = stmts

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitFor(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def crange(self):
        return self._crange

    def inv(self):
        return self._invs

    def stmts(self):
        return self._stmts

    def __repr__(self):
        return f"QXFor(id={repr(str(self._id))}, crange={self._crange}, invs={self._invs}, stmts={self._stmts})"


class QXCall(QXStmt, QXBool, QXAExp):

    def __init__(self, id: str, exps: [QXAExp]):
        self._id = id
        self._exps = exps

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCall(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def exps(self) -> [QXAExp]:
        return self._exps

    def __repr__(self):
        return f"QXCall(id={repr(str(self._id))}, exps={self._exps})"


class QXReturn(QXStmt):
    '''
    Represents a return statement, with a number of ids indicating which variables to return.

    example:
    ...
        ╭──────────────────╮
        │ return a_l, a_u; │
        ╰──────────────────╯
    }
    '''

    def __init__(self, ids: [str]):
        self._ids = ids

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitReturn(self)

    def IDs(self):
        return self._ids

    def __repr__(self):
        return f'QXReturn(ids={self._ids})'


class QXBreak(QXStmt):
    '''
    Represents a break statement.

    example:
    ...
    for i in [0, N)
        ...
    {
        if (y == 1){
            ╭────────╮
            │ break; │
            ╰────────╯
        }
    ...
    '''
    pass


class QXInclude(QXTop):

    def __init__(self, path: str):
        self.__path = path

    def path(self) -> str:
        '''Path to the file to be included'''
        return self.__path


class QXMethod(QXTop):

    def __init__(self, id: str, axiom: bool, bindings: [QXBind], returns: [QXBind], conds: [QXCond], stmts: [QXStmt]):
        self._id = id
        self._axiom = axiom
        self._bindings = bindings
        self._returns = returns
        self._conds = conds
        self._stmts = stmts

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitMethod(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

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
        return f"QXMethod(id={repr(str(self._id))}, axiom={self._axiom}, bindings={self._bindings}, returns={self._returns}, conds={self._conds}, stmts={self._stmts})"

class QXFunction(QXTop):

    def __init__(self, id: str, axiom: bool, bindings: [QXBind], return_type: QXQTy, arith_expr: QXAExp):
        self._id = id
        self._axiom = axiom
        self._bindings = bindings
        self._type = return_type
        self._arith_expr = arith_expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitFunction(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def axiom(self):
        return self._axiom

    def bindings(self):
        return self._bindings

    def return_type(self):
        return self._type

    def arith_expr(self):
        return self._arith_expr

    def __repr__(self):
        return f'QXFunction(id={self._id}, axiom={self._axiom}, bindings={self._bindings}, return_type={self._type}, arith_expr={self._arith_expr})'


class QXLemma(QXTop):

    def __init__(self, id: str, axiom: bool, bindings: [QXBind], conds: [QXCond]):
        self._id = id
        self._axiom = axiom
        self._bindings = bindings
        self._conds = conds

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitLemma(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def axiom(self):
        return self._axiom

    def bindings(self):
        return self._bindings

    def conds(self):
        return self._conds

    def __repr__(self):
        return f'QXLemma(id={self._id}, axiom={self._axiom}, bindings={self._bindings}, conds={self._conds})'

class QXPredicate(QXTop):

    def __init__(self, id: str, bindings: [QXBind], arith_expr: QXAExp):
        self._id = id
        self._bindings = bindings
        self._arith_expr = arith_expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPredicate(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def bindings(self):
        return self._bindings

    def arith_expr(self):
        return self._arith_expr

    def __repr__(self):
        return f'QXPredicate(id={self._id}, bindings={self._bindings}, arith_expr={self._arith_expr})'

class QXProgram(QXTop):

    def __init__(self, exps: [QXMethod]):
        self._exps = exps

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitProgram(self)

    def method(self):
        return self._exps

    def __repr__(self):
        return f"QXProgram(exps={self._exps})"
