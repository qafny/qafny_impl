from TargetProgramVisitor import TargetProgramVisitor
from TargetProgrammer import *
from AbstractTargetVisitor import AbstractTargetVisitor
from TypeChecker import *


class SubstDAExp(TargetProgramVisitor):
    """
    A robust visitor that substitutes a variable with a given ID for a new
    expression within a Dafny AST. Every visit method correctly reconstructs
    and returns the visited node to preserve the AST's integrity.
    """

    def __init__(self, id: str, e : DXAExp):
        self._id = id
        self._exp = e

    def exp(self):
        return self._exp

    def visitBin(self, ctx: DXBin):
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return DXBin(ctx.op(), l, r, line=ctx.line())

    def visitUni(self, ctx: DXUni):
        n = ctx.next().accept(self)
        return DXUni(ctx.op(), n, line=ctx.line())

    def visitBind(self, ctx: DXBind):
        # This is the core substitution logic.
        if ctx.ID() == self._id:
            return self.exp()
        else:
            # If the ID doesn't match, return the original node.
            return ctx

    def visitNum(self, ctx: DXNum):
        return ctx

    def visitReal(self, ctx: DXReal):
        return ctx

    def visitIndex(self, ctx:DXIndex):
        b = ctx.bind().accept(self)
        i = ctx.index().accept(self)
        return DXIndex(b, i, line=ctx.line())

    def visitCall(self, ctx: DXCall):
        exps = [elem.accept(self) for elem in ctx.exps()]
        return DXCall(ctx.ID(), exps, ctx.end(), line=ctx.line())

    def visitAll(self, ctx: DXAll):
        b = ctx.bind().accept(self)
        n = ctx.next().accept(self)
        return DXAll(b, n, line=ctx.line())
    
    def visitComp(self, ctx: DXComp):
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return DXComp(ctx.op(), l, r, line=ctx.line())
    
    def visitLogic(self, ctx: DXLogic):
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return DXLogic(ctx.op(), l, r, line=ctx.line())
    
    def visitNot(self, ctx: DXNot):
        n = ctx.next().accept(self)
        return DXNot(n, line=ctx.line())
    
    def visitInRange(self, ctx: DXInRange):
        b = ctx.bind().accept(self)
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return DXInRange(b, l, r, line=ctx.line())
    
    def visitIfExp(self, ctx: DXIfExp):
        b = ctx.bexp().accept(self)
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return DXIfExp(b, l, r, line=ctx.line())
    
    def visitCast(self, ctx: DXCast):
        n = ctx.next().accept(self)
        return DXCast(ctx.type(), n, line=ctx.line())
    
    def visitLength(self, ctx: DXLength):
        v = ctx.var().accept(self)
        return DXLength(v, line=ctx.line())

    # The following methods visit nodes that do not contain expressions
    # that can be substituted, so they can simply return the original context.
    # However, for full robustness, a deepcopy might be considered if nodes
    # could be mutated elsewhere.

    def visitAssert(self, ctx: DXAssert):
        s = ctx.spec().accept(self)
        return DXAssert(s, line=ctx.line())
    
    def visitAssign(self, ctx: DXAssign):
        ids = [i.accept(self) for i in ctx.ids()]
        exp = ctx.exp().accept(self)
        return DXAssign(ids, exp, ctx.init(), line=ctx.line())
    
    def visitEnsures(self, ctx: DXEnsures):
        s = ctx.spec().accept(self)
        return DXEnsures(s, line=ctx.line())
    
    def visitFunType(self, ctx: FunType):
        return ctx
    
    def visitIf(self, ctx: DXIf):
        c = ctx.cond().accept(self)
        l = [s.accept(self) for s in ctx.left()]
        r = [s.accept(self) for s in ctx.right()]
        return DXIf(c, l, r, line=ctx.line())
    
    def visitInit(self, ctx: DXInit):
        b = ctx.binding().accept(self)
        e = ctx.exp().accept(self) if ctx.exp() else None
        return DXInit(b, e, line=ctx.line())
    
    def visitMethod(self, ctx: DXMethod):
        # Substitution should generally not dive into nested methods.
        return ctx
    
    def visitProgram(self, ctx: DXProgram):
        # Substitution should generally not dive into other methods in the program.
        return ctx
    
    def visitRequires(self, ctx: DXRequires):
        s = ctx.spec().accept(self)
        return DXRequires(s, line=ctx.line())
    
    def visitSeqType(self, ctx: SeqType):
        return ctx
    
    def visitSType(self, ctx: SType):
        return ctx
    
    # def visitQRange(self, ctx: QXQRange):
    #     if ctx.index():
    #         ctx.index().accept(self)
    #     ctx.crange().accept(self)

    # def visitCrange(self, ctx: QXCRange):
    #     l = ctx.left().accept(self)
    #     r = ctx.right().accept(self)
    
    def visitWhile(self, ctx: DXWhile):
        c = ctx.cond().accept(self)
        s = [st.accept(self) for st in ctx.stmts()]
        i = [inv.accept(self) for inv in ctx.inv()]
        return DXWhile(c, s, i, line=ctx.line())

