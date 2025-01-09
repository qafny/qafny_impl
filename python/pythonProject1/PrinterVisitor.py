import TargetProgrammer
from TargetProgrammer import *

from AbstractTargetVisitor import AbstractTargetVisitor


class PrinterVisitor(AbstractTargetVisitor):

    def visit(self, ctx):
        match ctx:
            case DXProgram():
                self.visitProgram(ctx)
            case DXMethod():
                self.visitMethod(ctx)
            case DXAssert():
                self.visitAssert(ctx)
            case DXRequires():
                self.visitRequires(ctx)
            case DXEnsures():
                self.visitEnsures(ctx)
            case DXInit():
                self.visitInit(ctx)
            case DXAssign():
                self.visitAssign(ctx)
            case DXBin():
                self.visitBin(ctx)
            case DXUni():
                self.visitUni(ctx)
            case DXBind():
                self.visitBind(ctx)
            case DXNum():
                self.visitNum(ctx)
            case DXCall():
                self.visitCall(ctx)
            case SType():
                self.visitSType(ctx)
            case DXLogic():
                self.visitLogic(ctx)
            case DXAll():
                self.visitAll(ctx)
            case DXWhile():
                self.visitWhile(ctx)
            case DXIf():
                self.visitIf(ctx)
            case SeqType():
                self.visitSeqType(ctx)
            case DXVar():
                self.visitVar(ctx)
            case DXIndex():
                self.visitIndex(ctx)
            case DXLength():
                self.visitLength(ctx)
            case DXComp():
                self.visitComp(ctx)
            case DXNot():
                self.visitNot(ctx)
            case DXInRange():
                self.visitInRange(ctx)
            
    def visitProgram(self, ctx: TargetProgrammer.DXProgram):
        #visit all the methods and append them to create a program 
        program = ''
        for method in ctx.method():
            program += method.accept(self) + "\n\n"
        return program
    
    def visitMethod(self, ctx: TargetProgrammer.DXMethod):
        #visit all attributes of method, append the resultant strings to create a method

        bindings = ''
        for binding in ctx.bindings():
            bindings += binding.ID() + ':' + binding.type().accept(self) + ', ' if binding.type() else '' 
        bindings = bindings[:-2]

        returns = 'returns ('
        for rbinding in ctx.returns():
            returns += rbinding.ID() + ':' + rbinding.type().accept(self) + ', ' if rbinding.type() else '' 

        returns = (returns[:-2] + ')\n') if len(ctx.returns()) > 0 else '\n'

        conds = ''
        for cond in ctx.conds():
            r = cond.accept(self)
            conds += r + '\n' if r else ''

        if ctx.axiom():
            method = 'method {{:axiom}}' + ctx.ID() + '(' + bindings + ') ' + returns + conds
            return method
        
        stmts = ''
        for stmt in ctx.stmts():
            stmts += stmt.accept(self) + '\n'

        method = 'method ' + ctx.ID() + '(' + bindings + ') ' + returns + conds + '{\n' + stmts + '}'

        return method
        
    def visitAssert(self, ctx: TargetProgrammer.DXAssert):
        return 'assert ' + ctx.spec().accept(self) + ';'

    def visitRequires(self, ctx: TargetProgrammer.DXRequires):
        return 'requires ' + ctx.spec().accept(self)

    def visitEnsures(self, ctx: TargetProgrammer.DXEnsures):
        return 'ensures ' + ctx.spec().accept(self)

    def visitInit(self, ctx: TargetProgrammer.DXInit):
        return 'var ' + ctx.binding().ID() + ' := ' + ctx.exp().accept(self) + ';' if ctx.exp() else  'var ' + ctx.binding().ID() + ';'

    def visitAssign(self, ctx: TargetProgrammer.DXAssign):
        ids = ''
        for id in ctx.ids():
            ids += id.accept(self) + ', '
        ids = ids[:-2]
        return ids + ' := ' + ctx.exp().accept(self) + ';'

    def visitBin(self, ctx: TargetProgrammer.DXBin):
        return ctx.left().accept(self) + ' ' + ctx.op() + ' ' + ctx.right().accept(self)

    def visitUni(self, ctx: TargetProgrammer.DXUni):
        return ctx.op + ctx.next().accept(self)

    def visitBind(self, ctx: TargetProgrammer.DXBind):
        return ctx.ID()

    def visitNum(self, ctx: TargetProgrammer.DXNum):
        return str(ctx.num())

    def visitCall(self, ctx: TargetProgrammer.DXCall):
        args = ''
        for arg in ctx.exps():
            args += arg.accept(self) + ", "
        args = args[:-2]
        return ctx.ID() + '(' + args + ')'

    def visitSType(self, ctx: TargetProgrammer.SType):
        return ctx.type()

    def visitLogic(self, ctx: TargetProgrammer.DXLogic):
        return ctx.left().accept(self) + ' ' + ctx.op() + ' ' + ctx.right().accept(self)
    
    def visitAll(self, ctx: TargetProgrammer.DXAll):
        return 'forall ' + ctx.bind().ID() + ' :: ' + ctx.next().accept(self)

    def visitWhile(self, ctx: TargetProgrammer.DXWhile):
        stmts = ''
        for stmt in ctx.stmts():
            stmts += stmt.accept(self) + '\n'
        inv = ''
        if ctx.inv():
            for i in ctx.inv():
                inv += 'invariant ' + i.accept(self) + '\n'
        return 'while(' + ctx.cond().accept(self) + ')\n' + inv + '{\n' + stmts + '}'
    
    def visitIf(self, ctx: TargetProgrammer.DXIf):
        stmts = ''
        for stmt in ctx.stmts():
            stmts += stmt.accept(self) + ';\n'

        return 'if ' + ctx.cond().accept(self) + '{\n' + stmts + '}'

    def visitVar(self, ctx: TargetProgrammer.DXVar):
        return ctx.ID()
    
    def visitIndex(self, ctx: TargetProgrammer.DXIndex):
        return ctx.bind().accept(self) + '[' + ctx.index().accept(self) + ']'
    
    def visitLength(self, ctx: TargetProgrammer.DXLength):
        return '|' + ctx.var().accept(self) + '|'
    
    def visitComp(self, ctx: TargetProgrammer.DXComp):
        return ctx.left().accept(self) + ' ' + ctx.op() + ' ' + ctx.right().accept(self)
    
    def visitNot(self, ctx: TargetProgrammer.DXNot):
        return '!' + ctx.next().accept(self)
    
    def visitInRange(self, ctx: TargetProgrammer.DXInRange):
        return ctx.left().accept(self) + " <= " + ctx.bind().accept(self) + " < " + ctx.right().accept(self)
    
    def visitSeqType(self, ctx: TargetProgrammer.SeqType):
        return 'seq<' + ctx.type().accept(self) + ">"

    
