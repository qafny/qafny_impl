import AbstractProgramVisitor

# Qafny's AST

class QXTop:

    def accept(self, visitor):
        pass

class QXType(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass

class QXQExp(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass

class QXHad(QXQExp):

    def __init__(self, state: str):
        self._state = state

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitHad(self)

    def state(self):
        return self._state


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


class TySingle(QXType):

    def __init__(self, name: str):
        self._name = name

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSingleT(self)

    def type(self):
        return self._name

class TyQ(QXType):

    def __init__(self, flag: QXAExp):
        self._flag = flag

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQ(self)

    def flag(self):
        return self._flag

class TyFun(QXType):

    def __init__(self, left: QXType, right: QXType):
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitFun(self)

    def left(self):
        return self._left

    def right(self):
        return self._right

class QXQTy(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass

class TyHad(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitTyHad(self)


class TyEn(QXQTy):

    def __init__(self, flag: QXAExp):
        self._flag = flag

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitEn(self)

    def flag(self):
        return self._flag

class TyAA(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitAA(self)

class TyNor(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitNor(self)


class QXBExp(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass

class QXSpec(QXTop):

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


class QXCNot(QXBool):

    def __init__(self, next: QXBool):
        self._next = next

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitCNot(self)

    def next(self):
        return self._next


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
        return self._id

    def index(self):
        return self._index

class QXBin(QXAExp):

    def __init__(self, op: str, left:QXAExp, right: QXAExp):
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


class QXUni(QXAExp):

    def __init__(self, op: str, next:QXAExp):
        self._op = op
        self._next = next

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitUni(self)

    def op(self):
        return self._op

    def next(self):
        return self._next


class QXNum(QXAExp):

    def __init__(self, num: int):
        self._num = int

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitNum(self)

    def num(self):
        return self._num


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


class QXQNot(QXQBool):

    def __init__(self, next: QXQBool):
        self._next = next

    def next(self):
        return self._next


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


class QXVKet(QXKet):

    def __init__(self, vector: QXAExp):
        self._vector = vector

    def vector(self):
        return self._vector

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitVKet(self)

class QXOracle(QXExp):

    def __init__(self, ids: [str], omega: QXAExp, kets: [QXKet]):
        self._ids = ids
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


class QXQRange(QXTop):

    def __init__(self, id: str, crange: QXCRange):
        self._id = id
        self._crange = crange

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQRange(self)

    def ID(self):
        return self._id

    def crange(self):
        return self._crange


class QXQState(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXCon(QXTop):

    def __init__(self, id: str, crange: QXCRange):
        self._id = id
        self._crange = crange

    def ID(self):
        return self._id

    def range(self):
        return self._crange

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitCon(self)





class QXTensor(QXQState):

    def __init__(self, kets: [QXKet], id : str = None, crange: QXCRange = None, amp : QXAExp = None):
        self._kets = kets
        self._id = id
        self._crange = crange
        self._amp = amp

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitTensor(self)

    def ID(self):
        return self._id if self._id is str else self._id.getText()

    def range(self):
        return self._crange

    def kets(self):
        return self._kets

    def amp(self):
        return self._amp


class QXSum(QXQState):

    def __init__(self, sums : [QXCon], amp: QXAExp, kets: [QXKet]):
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


class QXQSpec(QXSpec):

    def __init__(self, locus: [QXQRange], qty: QXQTy, state: QXQState):
        self._locus = locus
        self._qty = qty
        self._state = state

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQSpec(self)

    def locus(self):
        return self._locus

    def qty(self):
        return self._qty

    def state(self):
        return self._state


class QXRequires(QXTop):

    def __init__(self, spec: QXSpec):
        self._spec = spec

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitRequires(self)

    def spec(self):
        return self._spec


class QXEnsures(QXTop):

    def __init__(self, spec: QXSpec):
        self._spec = spec

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitEnsures(self)

    def spec(self):
        return self._spec


class QXStmt(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


class QXAssert(QXStmt):

    def __init__(self, spec: QXSpec):
        self._spec = spec

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitAssert(self)

    def spec(self):
        return self._spec


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


class QXInit(QXStmt):

    def __init__(self, binding :QXBind):
        self._binding = binding

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitInit(self)

    def binding(self):
        return self._binding


class QXCAssign(QXStmt):

    def __init__(self, id: str, expr : QXAExp):
        self._id = id
        self._expr = expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCAssign(self)

    def ID(self):
        return self._id

    def aexp(self):
        return self._expr


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


class QXFor(QXStmt):

    def __init__(self, id: str, crange: QXCRange, invs : [QXSpec], stmts: [QXStmt]):
        self._id = id
        self._crange = crange
        self._invs = invs
        self._stmts = stmts

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitFor(self)

    def ID(self):
        return self._id

    def crange(self):
        return self._crange

    def inv(self):
        return self._invs

    def stmts(self):
        return self._stmts


class QXCall(QXStmt, QXBool, QXAExp):

    def __init__(self, id: str, exps: [QXAExp]):
        self._id = id
        self._exps = exps

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCall(self)

    def ID(self):
        return self._id

    def exps(self):
        return self._exps

class QXMethod(QXTop):

    def __init__(self, id: str, axiom:bool, bindings: [QXBind], returns : [QXBind], conds: [QXSpec], stmts: [QXStmt]):
        self._id = id
        self._axiom = axiom
        self._bindings = bindings
        self._returns = returns
        self._conds = conds
        self._stmts = stmts

    def accept(self, visitor : AbstractProgramVisitor):
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

class QXProgram(QXTop):

    def __init__(self, exps: [QXMethod]):
        self._exps = exps

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitProgram(self)

    def method(self):
        return self._exps



