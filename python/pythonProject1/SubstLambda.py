from TargetProgrammer import *
from AbstractTargetVisitor import AbstractTargetVisitor
from TypeChecker import *


class SubstLambda(AbstractTargetVisitor):

    def __init__(self, lamb):
        self.lamb = lamb
        self.outputs = []

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
            case DXIndex():
                return self.visitIndex(ctx)
            case DXNum():
                return self.visitNum(ctx)
            case DXReal():
                return self.visitReal(ctx)
            case DXLength():
                return self.visitLength(ctx)
            
            

    def visitBin(self, ctx: DXBin):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            l=ctx.left().accept(self)
            r=ctx.right().accept(self)
            return DXBin(ctx.op(),l,r)
        return l_res

    def visitUni(self, ctx: DXUni):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            l = ctx.next().accept(self)
            return DXUni(ctx.op(), l)
        return l_res

    def visitBind(self, ctx: DXBind):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            return DXBind(ctx.ID(), ctx.type().accept(self) if ctx.type() else None, ctx.num())
        return l_res

    def visitNum(self, ctx: DXNum):
        return ctx


    def visitIndex(self, ctx:DXIndex):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            l = ctx.bind().accept(self)
            b = ctx.index().accept(self)
            return DXIndex(l,b)
        return l_res
        
    def visitCall(self, ctx: DXCall):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            tmp = []
            for elem in ctx.exps():
                tmp += [elem.accept(self)]
            return DXCall(ctx.ID(), tmp)
        return l_res

    def visitAll(self, ctx: DXAll):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            bind = ctx.bind().accept(self)
            nxt = ctx.next().accept(self)
            return DXAll(bind, nxt)
        return l_res
    
    def visitAssert(self, ctx: DXAssert):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            spec = ctx.spec().accept(self)
            return DXAssert(spec)
        return l_res
    
    def visitAssign(self, ctx: DXAssign):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            ids = [i.accept(self) for i in ctx.ids()]
            exp = ctx.exp().accept(self)
            return DXAssign(ids, exp, ctx.init())
        return l_res
    
    def visitComp(self, ctx: DXComp):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            l = ctx.left().accept(self)
            r = ctx.right().accept(self)
            return DXComp(ctx.op(), l, r)
        return l_res
    
    def visitEnsures(self, ctx: DXEnsures):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            spec = ctx.spec().accept(self)
            return DXEnsures(spec)
        return l_res
    
    def visitFunType(self, ctx: FunType):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            l = ctx.left().accept(self)
            r = ctx.right().accept(self)
            return FunType(l, r)
        return l_res
    
    def visitIf(self, ctx: DXIf):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            cond = ctx.cond().accept(self)
            left = [stmt.accept(self) for stmt in ctx.left()]
            right = [stmt.accept(self) for stmt in ctx.right()]
            return DXIf(cond, left, right)
        return l_res
    
    def visitInit(self, ctx: DXInit):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            b = ctx.binding().accept(self)
            e = ctx.exp().accept(self) if ctx.exp() else None
            return DXInit(b, e)
        return l_res
    
    def visitInRange(self, ctx: DXInRange):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            x = ctx.bind().accept(self)
            l = ctx.left().accept(self)
            r = ctx.right().accept(self)
            return DXInRange(x, l, r)
        return l_res
    
    def visitLogic(self, ctx: DXLogic):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            l = ctx.left().accept(self)
            r = ctx.right().accept(self)
            return DXLogic(ctx.op(), l, r)
        return l_res
    
    def visitMethod(self, ctx: DXMethod):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            bindings = [b.accept(self) for b in ctx.bindings()]
            returns = [r.accept(self) for r in ctx.returns()]
            conds = [c.accept(self) for c in ctx.conds()]
            stmts = [s.accept(self) for s in ctx.stmts()]
            return DXMethod(ctx.ID(), ctx.axiom(), bindings, returns, conds, stmts)
        return l_res
    
    def visitNot(self, ctx: DXNot):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            nxt = ctx.next().accept(self)
            return DXNot(nxt)
        return l_res
    
    def visitProgram(self, ctx: DXProgram):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            methods = [m.accept(self) for m in ctx.method()]
            return DXProgram(methods)
        return l_res
    
    def visitRequires(self, ctx: DXRequires):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            spec = ctx.spec().accept(self)
            return DXRequires(spec)
        return l_res
    
    def visitSeqType(self, ctx: SeqType):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            ty = ctx.type().accept(self)
            return SeqType(ty)
        return l_res
    
    def visitSType(self, ctx: SType):
        l_res = self.lamb(self, ctx)
        return ctx if l_res is None else l_res
    
    def visitWhile(self, ctx: DXWhile):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            cond = ctx.cond().accept(self)
            stmts = [s.accept(self) for s in ctx.stmts()]
            inv = [i.accept(self) for i in ctx.inv()] if ctx.inv() else None
            return DXWhile(cond, stmts, inv)
        return l_res
    
    def visitIfExp(self, ctx: DXIfExp):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            bexp = ctx.bexp().accept(self)
            l = ctx.left().accept(self)
            r = ctx.right().accept(self)
            return DXIfExp(bexp, l, r)
        return l_res
    
    def visitCast(self, ctx: DXCast):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            typ = ctx.type().accept(self)
            nxt = ctx.next().accept(self)
            return DXCast(typ, nxt)
        return l_res
    
    def visitLength(self, ctx: DXLength):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            return DXLength(ctx.var().accept(self))
        return l_res
    
    def visitReal(self, ctx: DXReal):
        l_res = self.lamb(self, ctx)
        if l_res is None:
            return DXReal(ctx.real())
        return l_res