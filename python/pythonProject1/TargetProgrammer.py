import AbstractTargetVisitor


# Dafny's AST

class DXTop:

    def accept(self, visitor : AbstractTargetVisitor):
        pass


class DXType(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass


class DXSpec(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass

class DXStmt(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass

class DXAExp(DXTop):

    def accept(self, visitor : AbstractTargetVisitor):
        pass

# SType could be bv1, real, nat,
class SType(DXType):

    def __init__(self, name: str):
        self._name = name

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSType(self)

    def type(self):
        return self._name

class SeqType(DXType):

    def __init__(self, ty: DXType):
        self._ty = ty

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitSeqType(self)

    def type(self):
        return self._ty

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


class DXUni(DXType):

    def __init__(self, op: str, next:DXType):
        self._op = op
        self._next = next

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitUni(self)

    def op(self):
        return self._op

    def next(self):
        return self._next


class DXNum(DXType):

    def __init__(self, num: int):
        self._num = int

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitNum(self)

    def num(self):
        return self._num


class DXBind(DXAExp):

    def __init__(self, id: str, ty: DXType = None, num: int = None):
        self._id = id
        self._type = ty
        self._num = num

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitBind(self)

    def ID(self):
        return self._id if self._id is str else self._id.getText()

    def type(self):
        return self._type

    def num(self):
        return self._num

    def newBind(self):

        if self._num is None:
            return DXBind(self._id, self._type, 0)
        else:
            return DXBind(self._id, self._type, self._num + 1)


class DXIndex(DXAExp):

    def __init__(self, id: DXAExp, index: DXAExp):
        self._id = id
        self._index = index

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitQIndex(self)

    def bind(self):
        return self._id

    def index(self):
        return self._index


class DXBool(DXSpec):

    def accept(self, visitor : AbstractTargetVisitor):
        pass


class DXCNot(DXBool):

    def __init__(self, next: DXBool):
        self._next = next

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitCNot(self)

    def next(self):
        return self._next


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


class DXRequires(DXTop):

    def __init__(self, spec: DXSpec):
        self._spec = spec

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitRequires(self)

    def spec(self):
        return self._spec


class DXEnsures(DXTop):

    def __init__(self, spec: DXSpec):
        self._spec = spec

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitEnsures(self)

    def spec(self):
        return self._spec


class DXAssert(DXStmt):

    def __init__(self, spec: DXSpec):
        self._spec = spec

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitAssert(self)

    def spec(self):
        return self._spec

class DXCall(DXStmt,DXBool, DXAExp):

    def __init__(self, id: str, exps: [DXAExp]):
        self._id = id
        self._exps = exps

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitCall(self)

    def ID(self):
        return self._id

    def exps(self):
        return self._exps


class DXAssign(DXAExp):

    def __init__(self, ids: [DXBind], exp : DXAExp):
        self._ids = ids
        self._exp = exp

    def accept(self, visitor: AbstractTargetVisitor):
        return visitor.visitAssign(self)

    def ids(self):
        return self._ids

    def exp(self):
        return self._exp


class DXMethod(DXTop):

    def __init__(self, id: str, axiom:bool, bindings: [DXBind], returns : [DXBind], conds: [DXSpec], stmts: [DXStmt]):
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



class DXProgram(DXTop):

    def __init__(self, exps: [DXMethod]):
        self._exps = exps

    def accept(self, visitor : AbstractTargetVisitor):
        return visitor.visitProgram(self)

    def method(self):
        return self._exps


