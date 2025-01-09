import Programmer
from ProgramVisitor import ProgramVisitor
from Programmer import *
from TypeChecker import *


class LocusCollector(ProgramVisitor):

    def __init__(self, tenv: [([QXQRange], QXQTy)]):
        # need st --> state we are deling with
        # kind map from fun vars to kind maps
        #self.kenv = kenv
        # the checked type env at index
        self.tenv = tenv
        self.renv = []
        self.ty = None

    def visitAssert(self, ctx: Programmer.QXAssert):
        if isinstance(ctx.spec(), QXQSpec):
            self.renv += [(ctx.spec().locus(), ctx.spec().qty())]
        return True

    def visitInit(self, ctx: Programmer.QXInit):
        return True

    def visitCast(self, ctx: Programmer.QXCast):
        ty = ctx.qty()
        if isinstance(ty, TyAA):
            vs = sameLocus(ctx.locus(), self.renv)
            if vs is None:
                return False
            else:
                self.renv += [(ctx.locus(), TyAA())]
                return True

        re = subLocusGen(ctx.locus(), self.renv)
        if re is None:
            return False
        newLoc, newTy, vs = re
        self.renv += [(newLoc, ty)]
        return True

    def visitBind(self, ctx: Programmer.QXBind):
        if ctx.type() is not None:
            ctx.type().accept(self)
        return ctx.ID()

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        loc, ty, nenv = subLocusGen(ctx.locus(), self.renv)
        if isinstance(ctx.exp(), QXSingle):
            ty = addOneType(ty)
        if ty is None:
            return False

        self.renv += [(loc, ty)]
        return True

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        return False

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        return True

    def visitIf(self, ctx: Programmer.QXIf):
        if isinstance(ctx.bexp(), QXBool):
            for elem in ctx.stmts():
                elem.accept(self)
                return True
            else:
                return False

        if isinstance(ctx.bexp(), QXQBool):
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

    def visitCon(self, ctx: Programmer.QXCon):
        self.renv += [QXQRange(ctx.ID(),ctx.range())]
        return True

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        self.renv += [QXQRange(ctx.ID(), ctx.index(), QXBin("+", ctx.index(), QXNum(1)))]
        return True

    def visitQNot(self, ctx: Programmer.QXQNot):
        return ctx.next().accept(self)

    def visitQComp(self, ctx: Programmer.QXQComp):
        v1 = ctx.left().accept(self)
        v2 = ctx.right().accept(self)
        v3 = ctx.index().accept(self)
        return v1 and v2 and v3

    def visitBin(self, ctx: Programmer.QXBin):
        v1 = ctx.left().accept(self)
        v2 = ctx.right().accept(self)
        return v1 and v2

    def visitUni(self, ctx: Programmer.QXUni):
        v = ctx.next().accept(self)
        return v
