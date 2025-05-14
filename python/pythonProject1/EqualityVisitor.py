import TargetProgrammer
from TargetProgrammer import *

from TargetProgramVisitor import TargetProgramVisitor

class EqualityVisitor:

    def visit(self, ctx, ctx1):
        if ctx is None and ctx1 is None:
            return True
        if ctx is None or ctx1 is None:
            return False
        if type(ctx) != type(ctx1):
            return False

        match ctx:
            case DXBin():
                return self.visitBin(ctx, ctx1)
            case DXUni():
                return self.visitUni(ctx, ctx1)
            case DXAll():
                return self.visitAll(ctx, ctx1)
            case DXAssert():
                return self.visitAssert(ctx, ctx1)
            case DXAssign():
                return self.visitAssign(ctx, ctx1)
            case DXComp():
                return self.visitComp(ctx, ctx1)
            case DXEnsures():
                return self.visitEnsures(ctx, ctx1)
            case FunType():
                return self.visitFunType(ctx, ctx1)
            case DXIf():
                return self.visitIf(ctx, ctx1)
            case DXInit():
                return self.visitInit(ctx, ctx1)
            case DXInRange():
                return self.visitInRange(ctx, ctx1)
            case DXLogic():
                return self.visitLogic(ctx, ctx1)
            case DXMethod():
                return self.visitMethod(ctx, ctx1)
            case DXNot():
                return self.visitNot(ctx, ctx1)
            case DXProgram():
                return self.visitProgram(ctx, ctx1)
            case DXRequires():
                return self.visitRequires(ctx, ctx1)
            case SeqType():
                return self.visitSeqType(ctx, ctx1)
            case SType():
                return self.visitSType(ctx, ctx1)
            case DXWhile():
                return self.visitWhile(ctx, ctx1)
            case DXCall():
                return self.visitCall(ctx, ctx1)
            case DXBind():
                return self.visitBind(ctx, ctx1)
            case DXVar():
                return self.visitVar(ctx, ctx1)
            case DXIfExp():
                return self.visitIfExp(ctx, ctx1)
            case DXCast():
                return self.visitCast(ctx, ctx1)
            case DXIndex():
                return self.visitIndex(ctx, ctx1)
            case DXNum():
                return self.visitNum(ctx, ctx1)
            case DXReal():
                return self.visitReal(ctx, ctx1)
            case DXList():
                return self.visitList(ctx, ctx1)
            case DXLength():
                return self.visitLength(ctx, ctx1)
            case _:
                return self.generic_visit(ctx, ctx1)

    def generic_visit(self, a, b):
        return a == b

    def visitProgram(self, a, b):
        return self._compare_lists(a.method(), b.method())

    def visitMethod(self, a, b):
        return (
            a.ID() == b.ID() and
            a.axiom() == b.axiom() and
            self._compare_lists(a.bindings(), b.bindings()) and
            self._compare_lists(a.returns(), b.returns()) and
            self._compare_lists(a.conds(), b.conds()) and
            self._compare_lists(a.stmts(), b.stmts())
        )

    def visitBind(self, a, b):
        return (
            a.ID() == b.ID() and
            self.visit(a.type(), b.type()) and
            a.num() == b.num()
        )

    def visitVar(self, a, b):
        return (
            a.ID() == b.ID() and
            self.visit(a.type(), b.type())
        )

    def visitNum(self, a, b):
        return a.num() == b.num()

    def visitReal(self, a, b):
        return a.real() == b.real()

    def visitBin(self, a, b):
        return (
            a.op() == b.op() and
            self.visit(a.left(), b.left()) and
            self.visit(a.right(), b.right())
        )

    def visitIfExp(self, a, b):
        return (
            self.visit(a.bexp(), b.bexp()) and
            self.visit(a.left(), b.left()) and
            self.visit(a.right(), b.right())
        )

    def visitLogic(self, a, b):
        return (
            a.op() == b.op() and
            self.visit(a.left(), b.left()) and
            self.visit(a.right(), b.right())
        )

    def visitCast(self, a, b):
        return (
            self.visit(a.type(), b.type()) and
            self.visit(a.next(), b.next())
        )

    def visitUni(self, a, b):
        return (
            a.op() == b.op() and
            self.visit(a.next(), b.next())
        )

    def visitNot(self, a, b):
        return self.visit(a.next(), b.next())

    def visitList(self, a, b):
        return self._compare_lists(a.exprs(), b.exprs())

    def visitLength(self, a, b):
        return self.visit(a.var(), b.var())

    def visitAssign(self, a, b):
        return (
            self._compare_lists(a.ids(), b.ids()) and
            self.visit(a.exp(), b.exp()) and
            a.init() == b.init()
        )

    def visitIf(self, a, b):
        return (
            self.visit(a.cond(), b.cond()) and
            self._compare_lists(a.left(), b.left()) and
            self._compare_lists(a.right(), b.right())
        )

    def visitWhile(self, a, b):
        return (
            self.visit(a.cond(), b.cond()) and
            self._compare_lists(a.stmts(), b.stmts()) and
            self._compare_lists(a.inv(), b.inv())
        )

    def visitIndex(self, a, b):
        return (
            self.visit(a.bind(), b.bind()) and
            self.visit(a.index(), b.index())
        )

    def visitInit(self, a, b):
        return (
            self.visit(a.binding(), b.binding()) and
            self.visit(a.exp(), b.exp())
        )

    def visitCall(self, a, b):
        return (
            a.ID() == b.ID() and
            self._compare_lists(a.exps(), b.exps()) and
            a.end() == b.end()
        )

    def visitAssert(self, a, b):
        return self.visit(a.spec(), b.spec())

    def visitAll(self, a, b):
        return (
            self.visit(a.bind(), b.bind()) and
            self.visit(a.next(), b.next())
        )

    def visitInRange(self, a, b):
        return (
            self.visit(a.bind(), b.bind()) and
            self.visit(a.left(), b.left()) and
            self.visit(a.right(), b.right())
        )

    def visitComp(self, a, b):
        return (
            a.op() == b.op() and
            self.visit(a.left(), b.left()) and
            self.visit(a.right(), b.right())
        )

    def visitRequires(self, a, b):
        return self.visit(a.spec(), b.spec())

    def visitEnsures(self, a, b):
        return self.visit(a.spec(), b.spec())

    def visitSType(self, a, b):
        return a.type() == b.type()

    def visitSeqType(self, a, b):
        return self.visit(a.type(), b.type())

    def visitFunType(self, a, b):
        return (
            self.visit(a.left(), b.left()) and
            self.visit(a.right(), b.right())
        )

    def _compare_lists(self, list1, list2):
        if list1 is None and list2 is None:
            return True
        if list1 is None or list2 is None:
            return False
        if len(list1) != len(list2):
            return False
        return all(self.visit(x, y) for x, y in zip(list1, list2))