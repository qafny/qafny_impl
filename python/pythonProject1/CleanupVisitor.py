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
#            print(f"Visiting method: {method}")
            methods.append(method.accept(self))

        return DXProgram(methods, line=ctx.line())

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
            print(f"\nVisiting stmt: {stmt}")
            stmts.append(stmt.accept(self))

        return DXMethod(id, axiom, bindings, returns, conds, stmts, line=ctx.line())

    def visitRequires(self, ctx: TargetProgrammer.DXRequires):
        return DXRequires(ctx.spec().accept(self), line=ctx.line())

    def visitEnsures(self, ctx: TargetProgrammer.DXEnsures):
        return DXEnsures(ctx.spec().accept(self), line=ctx.line())

    def visitAll(self, ctx: TargetProgrammer.DXAll):
        return DXAll(ctx.bind(), ctx.next().accept(self), line=ctx.line())
    
    def visitLogic(self, ctx: TargetProgrammer.DXLogic):
        return DXLogic(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), line=ctx.line())
    
    def visitComp(self, ctx: TargetProgrammer.DXComp):
        print('\n visitComp in Cleanup', ctx)
        return DXComp(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), line=ctx.line())
    
    def visitNot(self, ctx: TargetProgrammer.DXNot):
        return DXNot(ctx.next().accept(self), line=ctx.line())
    
    def visitInRange(self, ctx):
        if isinstance(ctx.right(), DXNum) and ctx.right().val() == 2:
            return DXInRange(ctx.bind(), ctx.left().accept(self), DXCall('pow2', [DXNum(1)]), line=ctx.line()) 
        return DXInRange(ctx.bind(), ctx.left().accept(self), ctx.right().accept(self), line=ctx.line())
    
    def visitBin(self, ctx: TargetProgrammer.DXBin):
        if ctx.op() == '^':
            if isinstance(ctx.left(), DXNum) and ctx.left().val() == 2:
                return DXCall('pow2', [ctx.right().accept(self)], line=ctx.line())
            else:
                return DXCall('powN', [ctx.left().accept(self), ctx.right().accept(self)], line=ctx.line()) 
            
        elif ctx.op() == '/' and isinstance(ctx.left(), DXNum) and ctx.left().val() == 1 and isinstance(ctx.right(), DXCall) and ctx.right().ID() == 'pow2':
            return DXBin('/',DXNum(1.0), DXCast(SType('real'), ctx.right()), line=ctx.line())

        print(f"Visiting bin in Cleanup: {ctx}")
        return DXBin(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), line=ctx.line())

    def visitUni(self, ctx: TargetProgrammer.DXUni):
        return DXUni(ctx.op(), ctx.next().accept(self), line=ctx.line())

    def visitNum(self, ctx: TargetProgrammer.DXNum):
        return ctx

    def visitBind(self, ctx: TargetProgrammer.DXBind):
        return ctx

    def visitList(self, ctx: TargetProgrammer.DXList):
        exprs = []
        for i in ctx.exprs():
            exprs.append(i.accept(self))

        return DXList(exprs, line=ctx.line())

    def visitCall(self, ctx: TargetProgrammer.DXCall):
        #if ctx.ID() == 'omega' and isinstance(ctx.exps()[0], DXNum) and ctx.exps()[0].num() == 0:
            #return DXCast(SType('real'), DXNum(1))

        if ctx.ID() == 'ketIndex':
            return DXIndex(ctx.exps()[0], ctx.exps()[1], line=ctx.line())
        
        exps = []
#        print(f"Visiting call: {ctx} with arguments:")
        for exp in ctx.exps():
#            print(f"Visiting call argument: {exp}")
            exps.append(exp.accept(self))
        
        return DXCall(ctx.ID(), exps, ctx.end(), line=ctx.line())
    
    def visitIndex(self, ctx: TargetProgrammer.DXIndex):
        return DXIndex(ctx.bind(), ctx.index().accept(self), line=ctx.line())
        
    def visitCast(self, ctx: TargetProgrammer.DXCast):
        return DXCast(ctx.type(), ctx.next().accept(self), line=ctx.line())

    def visitIfExp(self, ctx: TargetProgrammer.DXIfExp):
        return DXIfExp(ctx.bexp().accept(self),
        ctx.left().accept(self),
        ctx.right().accept(self), line=ctx.line())

    def visitLength(self, ctx: TargetProgrammer.DXLength):
        return DXLength(ctx.var().accept(self), line=ctx.line())

    def visitWhile(self, ctx: TargetProgrammer.DXWhile):
        
        cond =ctx.cond().accept(self)

        stmts = []
        print(f"\nVisiting while: {ctx}")
        for s in ctx.stmts():
            stmts.append(s.accept(self))

        invs = []
        for i in ctx.inv():
            invs.append(i.accept(self))

        return DXWhile(cond, stmts, invs, line=ctx.line())

    def visitIf(self, ctx: TargetProgrammer.DXIf):
        cond = ctx.cond().accept(self)

        left = []
        for l in ctx.left():
            left.append(l.accept(self))

        right = []
        for r in ctx.right():
            right.append(r.accept(self))

        return DXIf(cond, left, right, line=ctx.line())

    def visitAssert(self, ctx:TargetProgrammer.DXAssert):
        return DXAssert(ctx.spec().accept(self), line=ctx.line())

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

        return DXAssign(ids, exp, ctx.init(), line=ctx.line())
    
    def visitInit(self, ctx: TargetProgrammer.DXInit):
        return ctx
    
    def visitSType(self, ctx: TargetProgrammer.SType):
        return ctx