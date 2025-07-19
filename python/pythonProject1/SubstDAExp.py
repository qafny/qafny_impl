from TargetProgramVisitor import TargetProgramVisitor
from TargetProgrammer import *
from AbstractTargetVisitor import AbstractTargetVisitor
from TypeChecker import *


class SubstDAExp(TargetProgramVisitor):

    def __init__(self, id: str, e : DXAExp):
        # need st --> state we are deling with
        # kind map from fun vars to kind maps
        #self.kenv = kenv
        # the checked type env at index
        self.id = id
        self.exp = e

    def visit(self, ctx):
        match ctx:
            case DXBin():
                return self.visitBin(ctx)
            case DXUni():
                return self.visitUni(ctx)
            case DXAll():
                return self.visitAll(ctx)
            case DXAssert():
                return self.visitAssert(ctx)
            case DXAssign():
                return self.visitAssign(ctx)
            case DXComp():
                return self.visitComp(ctx)
            case DXEnsures():
                return self.visitEnsures(ctx)
            case FunType():
                return self.visitFunType(ctx)
            case DXIf():
                return self.visitIf(ctx)
            case DXInit():
                return self.visitInit(ctx)
            case DXInRange():
                return self.visitInRange(ctx)
            case DXLogic():
                return self.visitLogic(ctx)
            case DXMethod():
                return self.visitMethod(ctx)
            case DXNot():
                return self.visitNot(ctx)
            case DXProgram():
                return self.visitProgram(ctx)
            case DXRequires():
                return self.visitRequires(ctx)
            case SeqType():
                return self.visitSeqType(ctx)
            case SType():
                return self.visitSType(ctx)
            case DXWhile():
                return self.visitWhile(ctx)
            case DXCall():
                return self.visitCall(ctx)
            case DXBind():
                return self.visitBind(ctx)
            case DXIfExp():
                return self.visitIfExp(ctx)
            case DXCast():
                return self.visitCast(ctx)
            case DXReal():
                return self.visitReal(ctx)
            
            

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

    def visitAll(self, ctx: DXAll):
        return super().visitAll(ctx)
    
    def visitAssert(self, ctx: DXAssert):
        return super().visitAssert(ctx)
    
    def visitAssign(self, ctx: DXAssign):
        return super().visitAssign(ctx)
    
    def visitComp(self, ctx: DXComp):
        return super().visitComp(ctx)
    
    def visitEnsures(self, ctx: DXEnsures):
        return super().visitEnsures(ctx)
    
    def visitFunType(self, ctx: FunType):
        return super().visitFunType(ctx)
    
    def visitIf(self, ctx: DXIf):
        return super().visitIf(ctx)
    
    def visitInit(self, ctx: DXInit):
        return super().visitInit(ctx)
    
    def visitInRange(self, ctx: DXInRange):
        return super().visitInRange(ctx)
    
    def visitLogic(self, ctx: DXLogic):
        return super().visitLogic(ctx)
    
    def visitMethod(self, ctx: DXMethod):
        return super().visitMethod(ctx)
    
    def visitNot(self, ctx: DXNot):
        return super().visitNot(ctx)
    
    def visitProgram(self, ctx: DXProgram):
        return super().visitProgram(ctx)
    
    def visitRequires(self, ctx: DXRequires):
        return super().visitRequires(ctx)
    
    def visitSeqType(self, ctx: SeqType):
        return super().visitSeqType(ctx)
    
    def visitSType(self, ctx: SType):
        return super().visitSType(ctx)
    
    def visitWhile(self, ctx: DXWhile):
        return super().visitWhile(ctx)
    
    def visitIfExp(self, ctx):
        return super().visitIfExp(ctx)
    
    def visitCast(self, ctx: DXCast):
        return DXCast(ctx.type(), ctx.next().accept(self))
    
    def visitLength(self, ctx: DXLength):
        return DXLength(ctx.var().accept(self))
    
    def visitReal(self, ctx: DXReal):
        return super().visitReal(ctx)