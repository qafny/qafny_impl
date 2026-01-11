import Programmer
from ProgramVisitor import ProgramVisitor
from copy import deepcopy

from Programmer import *


def compareAExp(a1: QXAExp, a2: QXAExp):
    if a1 is None or a2 is None:
        return False
    if isinstance(a1, QXBind) and isinstance(a2, QXBind):
        return str(a1.ID()) == str(a2.ID()) and compareType(a1.type(), a2.type())
    if isinstance(a1, QXQIndex) and isinstance(a2, QXQIndex):
        return str(a1.ID()) == str(a2.ID()) and compareAExp(a1.index(), a2.index())
    if isinstance(a1, QXBin) and isinstance(a2, QXBin):
        return a1.op() == a2.op() and compareAExp(a1.left(), a2.left()) and compareAExp(a1.right(), a2.right())
    if isinstance(a1, QXUni) and isinstance(a2, QXUni):
        return a1.op() == a2.op() and compareAExp(a1.next(), a2.next())
    if isinstance(a1, QXNum) and isinstance(a2, QXNum):
        return a1.num() == a2.num()
    return False


def compareType(t1: QXType, t2: QXType):
    if t1 is None and t2 is None:
        return True
    if t1 is None or t2 is None:
        return False
    if isinstance(t1, TySingle) and isinstance(t2, TySingle):
        return t1.type() == t2.type()
    if isinstance(t1, Programmer.TyQ) and isinstance(t2, TyQ):
        return compareAExp(t1.flag(), t2.flag())
    return False


# Collects the types of parameters to methods
class CollectKind(ProgramVisitor):
    """Implements a kind collector through the visitor pattern.
    The kenv is stored in env and can be retrieved through `get_kenv()`
    The visitor method returns a boolean indicating 
    """

    def __init__(self):
        # need st --> state we are deling with
        # form a map from a method variable to two maps, from variables to kinds
        # the first map contains all variables used in a method
        # the second map contain all variables as returns
        self.env = dict()
        # temp map for storing all variables used in a method, mapping from variable to kinds.
        self.tenv = dict()
        # temp map for storing all variables in returns, mapping from variable to kinds.
        self.xenv = dict()
        # check if a return variable is assigned
        # we use this list to remove variables
        self.reenv = []

    def visitMethod(self, ctx: Programmer.QXMethod):
        x = str(ctx.ID())
        self.tenv = dict()
        self.xenv = dict()

        for binding in ctx.bindings():
            y = str(binding.ID())
            tv = binding.type()
            if not tv.accept(self):
                return False
            self.tenv.update({y: tv})

        x_ = True
        for elem in ctx.returns():
            y = str(elem.ID())
            tv = elem.type()
            if not tv.accept(self):
                return False
            self.xenv.update({y: tv})

        self.reenv = list(self.xenv.keys())

        for condelem in ctx.conds():
            condelem.accept(self)

        tenvv = self.tenv.copy()

        for stmt in ctx.stmts():
            x_ = x_ and stmt.accept(self)

        self.env.update({x: (tenvv, self.xenv)})

        return x_

    def visitProgram(self, ctx: Programmer.QXProgram):
        for elem in ctx.topLevelStmts():
            v = elem.accept(self)
            if not v:
                return False
        return True

    def visitBind(self, ctx: Programmer.QXBind):
        ty = self.tenv.get(str(ctx.ID()))

        if ctx.type() is not None:
            return compareType(ty, ctx.type())
        elif ty is not None:
            return True
        else:
            ty1 = self.xenv.get(str(ctx.ID()))
            if ty1 is None:
                return False
            #    del self.reenv[str(ctx.ID())]
            if str(ctx.ID()) in self.reenv:
                #        print(f"{self.reenv} removing {str(ctx.ID())}")
                self.reenv.remove(str(ctx.ID()))

            if ctx.type() is not None:
                return compareType(ty1, ctx.type())
            else:
                return True

    def visitSingleT(self, ctx: Programmer.TySingle):
        return True

    def visitFun(self, ctx: Programmer.TyFun):
        return True

    def visitQComp(self, ctx: Programmer.QXQComp):
        return True

    def visitQ(self, ctx: Programmer.TyQ):
        return ctx.flag().accept(self)

    def visitBin(self, ctx: Programmer.QXBin):
        return ctx.left().accept(self) and ctx.right().accept(self)

    def visitUni(self, ctx: Programmer.QXUni):
        return ctx.next().accept(self)

    def visitNum(self, ctx: Programmer.QXNum):
        return True

    def visitInit(self, ctx: Programmer.QXInit):
        y = str(ctx.binding().ID())
        tv = ctx.binding().type()
        if tv.accept(self):
            self.tenv.update({y: tv})
            return True
        return False

    def visitCast(self, ctx: Programmer.QXCast):
        v = ctx.qty().accept(self)
        for elem in ctx.locus():
            v = v and elem.accept(self)
        return v

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        v = True
        for elem in ctx.locus():
            v = v and elem.accept(self)
        v = v and ctx.exp().accept(self)
        return v

    def visitOracle(self, ctx: Programmer.QXOracle):
        v = True
        for i in ctx.bindings():
            if isinstance(i, QXBind):
                i = i.ID()
            if str(i) not in self.tenv:
                self.tenv.update({str(i): QXQTy()})

        for i in ctx.kets():
            v = v and i.accept(self)
        return v

    def visitSKet(self, ctx: Programmer.QXSKet):
        return ctx.vector().accept(self)

    def visitQRange(self, ctx: Programmer.QXQRange):
        v = True
        v = v and ctx.crange().accept(self)

        return v

    def visitCRange(self, ctx: Programmer.QXCRange):
        return ctx.left().accept(self) and ctx.right().accept(self)

    def isBitType(self, t: QXType):
        if isinstance(t, TySingle):
            return t.type() == "nat" or t.type() == "real" or t.type() == "bool"

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        v = True
        for elem in ctx.locus():
            v = v and elem.accept(self)
        for id in ctx.ids():
            ty = self.tenv.get(id.ID())
            if ty is not None:
                v = v and self.isBitType(ty)
            else:
                ty1 = self.xenv.get(id.ID())
                if ty1 is None:
                    return False
                #                del self.reenv[id.ID()]
                #                print(self.reenv, id.ID())
                # if id.ID() in self.reenv:
                #     self.reenv.remove(id.ID())
                v = v and self.isBitType(ty1)
        return v

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        v = ctx.aexp().accept(self)
        v = v and str(ctx.ids())
        return v

    def visitIf(self, ctx: Programmer.QXIf):
        v = ctx.bexp().accept(self) if isinstance(ctx.bexp(), Programmer.QXBExp) else True
        for elem in ctx.stmts():
            v = v and elem.accept(self)
        return v

    def visitFor(self, ctx: Programmer.QXFor):
        v = ctx.crange().accept(self)
        self.tenv.update({str(ctx.ID()): TySingle("nat")})

        for ielem in ctx.inv():
            v = v and ielem.accept(self)

        #    print("\nvisiting for stmts", ctx.stmts())
        for elem in ctx.stmts():
            v = v and elem.accept(self)
        return v

    def visitCall(self, ctx: Programmer.QXCall):
        v = True
        for elem in ctx.exps():
            v = v and elem.accept(self)
        return v

        # if str(ctx.ID()) in self.env.keys():
        #     v = True
        #     for elem in ctx.exps():
        #         v = v and elem.accept(self)
        #         return v
        # return False

    def visitQSpec(self, ctx: Programmer.QXQSpec):
        for elem in ctx.locus():
            elem.accept(self)
        for state in ctx.states():
            state.accept(self)
        ctx.qty().accept(self)
        return True

    def visitAssert(self, ctx: Programmer.QXAssert):
        if isinstance(ctx.spec(), QXQSpec):
            return True
        return ctx.spec().accept(self)

    def visitSingle(self, ctx: Programmer.QXSingle):
        """Visit a single quantum operation"""
        # We assume that the type of a single quantum operation is always valid
        return True

    def visitInclude(self, ctx):
        return True

        # need to check if the asserted var is in the tenv or xenv

    # leave the check later
    def visitAll(self, ctx: Programmer.QXAll):
        """Visit a QXAll operation"""
        # We assume that the type of a QXAll operation is always valid
        return True

    def get_kenv(self):
        """Returns the kenv used by TypeCollector and TypeChecker"""
        return self.env
