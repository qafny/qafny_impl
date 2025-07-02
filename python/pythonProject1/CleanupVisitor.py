import TargetProgrammer
from TargetProgrammer import *

from TargetProgramVisitor import TargetProgramVisitor


class CleanupVisitor(TargetProgramVisitor):

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
    
    def visitProgram(self, ctx: TargetProgrammer.DXProgram):
        methods = []
        for method in ctx.method():
            methods.append(method.accept(self))

        return DXProgram(methods, qafny_line_number=ctx.qafny_line_number())

    def visitMethod(self, ctx: TargetProgrammer.DXMethod):
        id = ctx.ID()
        axiom = ctx.axiom()
        bindings = ctx.bindings()
        returns = ctx.returns()

        conds = []
        for cond in ctx.conds():
            conds.append(cond.accept(self))

        stmts = []
        for stmt in ctx.stmts():
            stmts.append(stmt.accept(self))

        return DXMethod(id, axiom, bindings, returns, conds, stmts, qafny_line_number=ctx.qafny_line_number())

    def visitRequires(self, ctx: TargetProgrammer.DXRequires):
        return DXRequires(ctx.spec().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitEnsures(self, ctx: TargetProgrammer.DXEnsures):
        return DXEnsures(ctx.spec().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitAll(self, ctx: TargetProgrammer.DXAll):
        return DXAll(ctx.bind(), ctx.next().accept(self), qafny_line_number=ctx.qafny_line_number())
    
    def visitLogic(self, ctx: TargetProgrammer.DXLogic):
        return DXLogic(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), qafny_line_number=ctx.qafny_line_number())
    
    def visitComp(self, ctx: TargetProgrammer.DXComp):
        return DXComp(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), qafny_line_number=ctx.qafny_line_number())
    
    def visitNot(self, ctx: TargetProgrammer.DXNot):
        return DXNot(ctx.next().accept(self), qafny_line_number=ctx.qafny_line_number())
    
    def visitInRange(self, ctx):
        if isinstance(ctx.right(), DXNum) and ctx.right().num() == 2:
            return DXInRange(ctx.bind(), ctx.left().accept(self), DXCall('pow2', [DXNum(1)]), qafny_line_number=ctx.qafny_line_number()) 
        return DXInRange(ctx.bind(), ctx.left().accept(self), ctx.right().accept(self), qafny_line_number=ctx.qafny_line_number())
    
    def visitBin(self, ctx: TargetProgrammer.DXBin):
        if ctx.op() == '^':
            if isinstance(ctx.left(), DXNum) and ctx.left().num() == 2:
                return DXCall('pow2', [ctx.right().accept(self)], qafny_line_number=ctx.qafny_line_number())
            else:
                return DXCall('powN', [ctx.left().accept(self), ctx.right().accept(self)], qafny_line_number=ctx.qafny_line_number()) 
            
        elif ctx.op() == '/' and isinstance(ctx.left(), DXNum) and ctx.left().num() == 1 and isinstance(ctx.right(), DXCall) and ctx.right().ID() == 'pow2':
            return DXBin('/',DXNum(1.0), DXCast(SType('real'), ctx.right()), qafny_line_number=ctx.qafny_line_number())

        return DXBin(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitUni(self, ctx: TargetProgrammer.DXUni):
        return DXUni(ctx.op(), ctx.next().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitNum(self, ctx: TargetProgrammer.DXNum):
        return ctx

    def visitBind(self, ctx: TargetProgrammer.DXBind):
        return ctx

    def visitList(self, ctx: TargetProgrammer.DXList):
        exprs = []
        for i in ctx.exprs():
            exprs.append(i.accept(self))

        return DXList(exprs, qafny_line_number=ctx.qafny_line_number())

    def visitCall(self, ctx: TargetProgrammer.DXCall):
        #if ctx.ID() == 'omega' and isinstance(ctx.exps()[0], DXNum) and ctx.exps()[0].num() == 0:
            #return DXCast(SType('real'), DXNum(1))

        if ctx.ID() == 'ketIndex':
            return DXIndex(ctx.exps()[0], ctx.exps()[1], qafny_line_number=ctx.qafny_line_number())
        
        exps = []
        for exp in ctx.exps():
            exps.append(exp.accept(self))
        
        return DXCall(ctx.ID(), exps, ctx.end(), qafny_line_number=ctx.qafny_line_number())
    
    def visitIndex(self, ctx: TargetProgrammer.DXIndex):
        return DXIndex(ctx.bind().accept(self), ctx.index().accept(self), qafny_line_number=ctx.qafny_line_number())
        
    def visitCast(self, ctx: TargetProgrammer.DXCast):
        return DXCast(ctx.type(), ctx.next().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitIfExp(self, ctx: TargetProgrammer.DXIfExp):
        return DXIfExp(ctx.bexp().accept(self),
        ctx.left().accept(self),
        ctx.right().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitLength(self, ctx: TargetProgrammer.DXLength):
        return DXLength(ctx.var().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitVar(self, ctx: TargetProgrammer.DXVar):
        return ctx

    def visitWhile(self, ctx: TargetProgrammer.DXWhile):
        
        cond =ctx.cond().accept(self)

        stmts = []
        for s in ctx.stmts():
            stmts.append(s.accept(self))

        invs = []
        for i in ctx.inv():
            invs.append(i.accept(self))

        return DXWhile(cond, stmts, invs, qafny_line_number=ctx.qafny_line_number())

    def visitIf(self, ctx: TargetProgrammer.DXIf):
        cond = ctx.cond().accept(self)

        left = []
        for l in ctx.left():
            left.append(l.accept(self))

        right = []
        for r in ctx.right():
            right.append(r.accept(self))

        return DXIf(cond, left, right, qafny_line_number=ctx.qafny_line_number())

    def visitAssert(self, ctx:TargetProgrammer.DXAssert):
        return DXAssert(ctx.spec().accept(self), qafny_line_number=ctx.qafny_line_number())

    def visitAssign(self, ctx: TargetProgrammer.DXAssign):
        ids = []
        for i in ctx.ids():
            ids.append(i.accept(self))

        exp = None
        if isinstance(ctx.exp(), list):
            exp = []
            for i in ctx.exp():
                exp.append(i.accept(self))
        else:      
            exp = ctx.exp().accept(self)

        return DXAssign(ids, exp, ctx.init(), qafny_line_number=ctx.qafny_line_number())
    
    def visitInit(self, ctx: TargetProgrammer.DXInit):
        return ctx
    
    def visitSType(self, ctx: TargetProgrammer.SType):
        return ctx