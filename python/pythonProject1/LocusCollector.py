import Programmer
from ProgramVisitor import ProgramVisitor
from Programmer import *
from TypeChecker import *

def joinRange(q1:QXQRange, qs:[QXQRange]):
    tmp = []
    for i in len(qs):
        elem = qs[i]
        if q1.ID() == elem.ID() and compareAExp(elem.crange().right(), q1.crange().left()):
                tmp += [QXQRange(q1.ID(), QXCRange(elem.crange().right(), q1.crange().left()))] + qs[i+1:len(qs)]
                return tmp
        else:
            tmp += [elem]
    tmp += [q1]
    return tmp

def joinLocus(q1:[QXQRange], qs:[QXQRange]):
    for elem in q1:
        qs=joinLocus(elem,qs)
    return qs

def getVars(q: [QXQRange]):
    tmp = []
    for elem in q:
        tmp += [elem.ID()]
    return tmp

def findLocus(q: [str], qs:[[QXQRange]]):
    tmp = []
    for i in len(qs):
        elem = qs[i]
        if set(q) <= set(getVars(elem)):
            return elem,(tmp + qs[i+1:len(qs)])
    return None


class LocusCollector(ProgramVisitor):

    def __init__(self):
        self.renv = []

    def visitAssert(self, ctx: Programmer.QXAssert):
        if isinstance(ctx.spec(), QXQSpec):
            self.renv=joinLocus(ctx.spec().locus(),self.renv)
        return True

    def visitInit(self, ctx: Programmer.QXInit):
        return True

    def visitCast(self, ctx: Programmer.QXCast):
        self.renv=joinLocus(ctx.locus(), self.renv)
        return True

    def visitBind(self, ctx: Programmer.QXBind):
        return True

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        self.renv=joinLocus(ctx.locus(), self.renv)
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
        return True

    def visitCall(self, ctx: Programmer.QXCall):
        for elem in ctx.exps():
            elem.accept(self)
        return True

    def visitCon(self, ctx: Programmer.QXCon):
        self.renv=joinLocus([QXQRange(ctx.ID(),ctx.range())],self.renv)
        return True

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        self.renv=joinLocus([QXQRange(ctx.ID(), ctx.index(), QXBin("+", ctx.index(), QXNum(1)))], self.renv)
        return True

    def visitQNot(self, ctx: Programmer.QXQNot):
        return ctx.next().accept(self)

    def visitQRange(self, ctx: Programmer.QXQRange):
        self.renv = joinLocus([ctx],self.renv)
        return True

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
