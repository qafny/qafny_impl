import Programmer
from Programmer import *

from AbstractProgramVisitor import AbstractProgramVisitor


class ProgramVisitor(AbstractProgramVisitor):

    def visit(self, ctx):
        match ctx:
            case QXMethod():
                return self.visitMethod(ctx)
            case QXProgram():
                return self.visitProgram(ctx)
            case QXAssert():
                return self.visitAssert(ctx)
            case QXInit():
                return self.visitInit(ctx)
            case QXCast():
                return self.visitCast(ctx)
            case QXBind():
                return self.visitBind(ctx)
            case QXQAssign():
                return self.visitQAssign(ctx)
            case QXMeasure():
                return self.visitMeasure(ctx)
            case QXCAssign():
                return self.visitCAssign(ctx)
            case QXIf():
                return self.visitIf(ctx)
            case QXFor():
                return self.visitFor(ctx)
            case QXCall():
                return self.visitCall(ctx)
            case TySingle():
                return self.visitSingleT(ctx)
            case TyArray():
                return self.visitArrayT(ctx)
            case TyFun():
                return self.visitFun(ctx)
            case TyQ():
                return self.visitQ(ctx)
            case QXCNot():
                return self.visitCNot(ctx)
            case TyNor():
                return self.visitNor(ctx)
            case TyHad():
                return self.visitTyHad(ctx)
            case TyEn():
                return self.visitEn(ctx)
            case TyAA():
                return self.visitAA(ctx)
            case QXQSpec():
                return self.visitQSpec(ctx)
            case QXTensor():
                return self.visitTensor(ctx)
            case QXSKet():
                return self.visitSKet(ctx)
            case QXVKet():
                return self.visitVKet(ctx)
            case QXSum():
                return self.visitSum(ctx)
            case QXPart():
                return self.visitPart(ctx)
            case QXVarState():
                return self.visitVarState(ctx)
            case QXComp():
                return self.visitBool(ctx)
            case QXLogic():
                return self.visitLogic(ctx)
            case QXCon():
                return self.visitCon(ctx)
            case QXQIndex():
                return self.visitQIndex(ctx)
            case QXQNot():
                return self.visitQNot(ctx)
            case QXQComp():
                return self.visitQComp(ctx)
            case QXAll():
                return self.visitAll(ctx)
            case QXBin():
                return self.visitBin(ctx)
            case QXUni():
                return self.visitUni(ctx)
            case QXSingle():
                return self.visitSingle(ctx)
            case QXOracle():
                return self.visitOracle(ctx)
            case QXNum():
                return self.visitNum(ctx)
            case QXHad():
                return self.visitHad(ctx)
            case QXQRange():
                return self.visitQRange(ctx)
            case _:
                raise NotImplementedError(f"No visit method defined for {type(ctx)}")


    def visitMethod(self, ctx: Programmer.QXMethod):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        for reelem in ctx.returns():
            reelem.accept(self)

        for condelem in ctx.conds():
            condelem.accept(self)

        for stmtelem in ctx.stmts():
            stmtelem.accept(self)

    def visitProgram(self, ctx: Programmer.QXProgram):
        for elem in ctx.method():
            elem.accept(self)


    def visitAssert(self, ctx: Programmer.QXAssert):
        return ctx.spec().accept(self)

    def visitRequires(self, ctx: Programmer.QXRequires):
        return ctx.spec().accept(self)

    def visitEnsures(self, ctx: Programmer.QXEnsures):
        return ctx.spec().accept(self)

    def visitInit(self, ctx: Programmer.QXInit):
        return ctx.binding().accept(self)

    def visitCast(self, ctx: Programmer.QXCast):
        ctx.qty().accept(self)
        for elem in ctx.locus():
            elem.accept(self)

    def visitCRange(self, ctx: Programmer.QXCRange):
        ctx.left().accept(self)
        ctx.right().accept(self)

    def visitBind(self, ctx: Programmer.QXBind):
        if ctx.type() is not None:
            ctx.type().accept(self)
        return ctx.ID()

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        for elem in ctx.locus():
            elem.accept(self)
        return ctx.exp().accept(self)

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        for elem in ctx.locus():
            elem.accept(self)
        if ctx.res() is not None:
            ctx.res().accept(self)
        return ctx.ids()

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        ctx.aexp().accept(self)
        return ctx.ID()

    def visitIf(self, ctx: Programmer.QXIf):
        ctx.bexp().accept(self)
        for elem in ctx.stmts():
            elem.accept(self)

    def visitFor(self, ctx: Programmer.QXFor):
        ctx.crange().accept(self)

        for ielem in ctx.inv():
            ielem.accept(self)

        for elem in ctx.stmts():
            elem.accept(self)
        return ctx.ID()

    def visitCall(self, ctx: Programmer.QXCall):
        for elem in ctx.exps():
            elem.accept(self)
        return ctx.ID()

    def visitSingleT(self, ctx:Programmer.TySingle):
        return ctx.type()

    def visitArrayT(self, ctx:Programmer.TyArray):
        return ctx.type().accept(self)

    def visitFun(self, ctx: Programmer.TyFun):
        ctx.left().accept(self)
        ctx.right().accept(self)


    def visitQ(self, ctx: Programmer.TyQ):
        return ctx.flag().accept(self)

    def visitCNot(self, ctx: Programmer.QXCNot):
        return ctx.next().accept(self)

    def visitNor(self, ctx: Programmer.TyNor):
        return "nor"

    def visitTyHad(self, ctx: Programmer.TyHad):
        return "had"

    def visitEn(self, ctx: Programmer.TyEn):
        return ctx.flag().accept(self)

    def visitQSpec(self, ctx: Programmer.QXQSpec):
        ctx.qty().accept(self)
        for elem in ctx.locus():
            elem.accept(self)
        return ctx.state().accept(self)

    def visitAA(self, ctx: Programmer.TyAA):
        return "aa"

    def visitTensor(self, ctx: Programmer.QXTensor):
        for elem in ctx.kets():
            elem.accept(self)

    def visitSKet(self, ctx: Programmer.QXSKet):
        return ctx.vector().accept(self)

    def visitVKet(self, ctx: Programmer.QXVKet):
        return ctx.vector().accept(self)


    def visitSum(self, ctx: Programmer.QXSum):
        for elem in ctx.kets():
            elem.accept(self)
        ctx.amp().accept(self)
        for elem in ctx.sums():
            elem.accept(self)

    def visitVarState(self, ctx: Programmer.QXQState):
        return ctx.amp().accept(self)

    def visitPart(self, ctx: Programmer.QXPart):
        ctx.qnum().accept(self)
        ctx.fname().accept(self)
        ctx.trueAmp().accept(self)
        ctx.falseAmp().accept(self)

    def visitLogic(self, ctx: Programmer.QXLogic):
        ctx.left().accept(self)
        ctx.right().accept(self)

    def visitBool(self, ctx: Programmer.QXComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitCon(self, ctx: Programmer.QXCon):
        ctx.range().accept(self)
        return ctx.ID()

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        ctx.index().accept(self)
        return ctx.ID()

    def visitQNot(self, ctx: Programmer.QXQNot):
        return ctx.next().accept(self)

    def visitQComp(self, ctx: Programmer.QXQComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        ctx.index().accept(self)

    def visitAll(self, ctx: Programmer.QXAll):
        ctx.bind().accept(self)
        ctx.next().accept(self)

    def visitBin(self, ctx: Programmer.QXBin):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitUni(self, ctx: Programmer.QXUni):
        ctx.next().accept(self)
        return ctx.op()

    def visitSingle(self, ctx: Programmer.QXSingle):
        return ctx.op()

    def visitOracle(self, ctx: Programmer.QXOracle):
        ctx.phase().accept(self)
        for elem in ctx.vectors():
            elem.accept(self)

    def visitNum(self, ctx: Programmer.QXNum):
        return ctx.num()

    def visitHad(self, ctx: Programmer.QXHad):
        return ctx.state()


    def visitQRange(self, ctx: Programmer.QXQRange):
        ctx.crange().accept(self)
        return ctx.ID()