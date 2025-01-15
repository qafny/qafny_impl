from TargetProgrammer import *
from AbstractTargetVisitor import AbstractTargetVisitor
from TypeChecker import *


class SubstDAExp(AbstractTargetVisitor):

    def __init__(self, id: str, e : DXAExp):
        # need st --> state we are deling with
        # kind map from fun vars to kind maps
        #self.kenv = kenv
        # the checked type env at index
        self.id = id
        self.exp = e


    def visitBin(self, ctx: DXBin):
        l=ctx.left().accept(self)
        r=ctx.right().accept(self)
        return DXBin(ctx.op(),l,r)

    def visitUni(self, ctx: DXUni):
        l = ctx.next().accept(self)
        return DXUni(ctx.op(), l)

    def visitBind(self, ctx: DXBind):
        if ctx.ID() == self.id:
            return self.exp
        else:
            return ctx

    def visitNum(self, ctx: DXNum):
        return ctx


    def visitIndex(self, ctx:DXIndex):
        l = ctx.index().accept(self)
        b = ctx.bind().accept(self)
        return DXBind(b,l)

    def visitCall(self, ctx: DXCall):
        tmp = []
        for elem in ctx.exps():
            tmp += [elem.accept(self)]
        return DXCall(ctx.ID(), tmp)