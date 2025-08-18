import TargetProgrammer
from TargetProgrammer import *

from TargetProgramVisitor import TargetProgramVisitor


class PrinterVisitor(TargetProgramVisitor):

    def __init__(self, current_line: int = 1):
        self.line_mapping = {}
        self.current_line = current_line
    
    def visitProgram(self, ctx: TargetProgrammer.DXProgram):
        # visit all the methods and append them to create a program 
        program = ''
        for method in ctx.method():
            program += method.accept(self) + "\n\n"
            self.current_line += 2
        return program
    
    def visitMethod(self, ctx: TargetProgrammer.DXMethod):
        # visit all attributes of method, append the resultant strings to create a method
        print(f'\nVisiting method: {ctx}')
        bindings = ''
        for binding in ctx.bindings():
            bindings += binding.ID() + (str(binding.num()) if binding.num() else '') + ': ' + binding.type().accept(self) + ', ' if binding.type() else '' 
        bindings = bindings[:-2]

        returns = 'returns ('
        for rbinding in ctx.returns():
            returns += rbinding.ID() + (str(rbinding.num()) if rbinding.num() else '') + ': ' + rbinding.type().accept(self) + ', ' if rbinding.type() else '' 

        returns = (returns[:-2] + ')\n') if len(ctx.returns()) > 0 else '\n'
        self.line_mapping[self.current_line] = ctx
        self.current_line += 1
        conds = ''
        for cond in ctx.conds():
            r = cond.accept(self)
            conds += '  ' + r + '\n' if r else ''
            if r:
                self.line_mapping[self.current_line] = cond
                self.current_line += 1

        self.current_line += 1
        if ctx.axiom():
            method = 'method {:axiom} ' + ctx.ID() + '(' + bindings + ') ' + returns + conds
            return method
        
        stmts = ''
        for stmt in ctx.stmts():
            stmts += '  ' + stmt.accept(self) + '\n'
            self.line_mapping[self.current_line] = stmt
            self.current_line += 1

        method = 'method ' + ctx.ID() + '(' + bindings + ') ' + returns + conds + '{\n' + stmts + '}'

        return method
        
    def visitAssert(self, ctx: TargetProgrammer.DXAssert):
        return 'assert ' + ctx.spec().accept(self) + ';'

    def visitRequires(self, ctx: TargetProgrammer.DXRequires):
        return 'requires ' + ctx.spec().accept(self)

    def visitEnsures(self, ctx: TargetProgrammer.DXEnsures):
        return 'ensures ' + ctx.spec().accept(self)

    def visitInit(self, ctx: TargetProgrammer.DXInit):
#        print('\ninit', ctx)
        if ctx.exp() and isinstance(ctx.exp(), DXList) and len(ctx.exp().exprs()) == 0 and ctx.binding().type():
            return 'var ' + ctx.binding().ID() + (str(ctx.binding().num()) if ctx.binding().num() else '') + ':' + ctx.binding().type().accept(self) + ' := ' + ctx.exp().accept(self) + ';'

        return 'var ' + ctx.binding().ID() + (str(ctx.binding().num()) if ctx.binding().num() else '') + ' := ' + ctx.exp().accept(self) + ';' if ctx.exp() else  'var ' + ctx.binding().ID() + (str(ctx.binding().num()) if ctx.binding().num() else '') + (':' + ctx.binding().type().accept(self) if ctx.binding().type() else '') + ';'

    def visitAssign(self, ctx: TargetProgrammer.DXAssign):
        ids = ''
        res = ''
        for id in ctx.ids():
            ids += id.accept(self) + ', '
        ids = ids[:-2]
        if isinstance(ctx.exp(), list):
            exp = ''
            for ex in ctx.exp():
                exp += ex.accept(self) + ', '
            exp = exp[:-2]
            res = ids + ' := ' + exp + ';'
        else:
            res = ids + ' := ' + ctx.exp().accept(self) + ';'

        if ctx.init():
            res = 'var ' + res

        return res

    def visitBin(self, ctx: TargetProgrammer.DXBin):
        print('\nvisitBin', ctx)
        return '(' + ctx.left().accept(self) + ' ' + ctx.op() + ' ' + ctx.right().accept(self) + ')'

    def visitUni(self, ctx: TargetProgrammer.DXUni):
        return ctx.op() + '(' + ctx.next().accept(self) + ')'

    def visitBind(self, ctx: TargetProgrammer.DXBind):
        print('\nvisitBind', ctx)
        return ctx.ID() + (str(ctx.num()) if ctx.num() else '')

    def visitNum(self, ctx: TargetProgrammer.DXNum):
        return str(ctx.val())

    def visitCall(self, ctx: TargetProgrammer.DXCall):
        args = ''
        for arg in ctx.exps():
            args += arg.accept(self) + ", "
        args = args[:-2]
        return ctx.ID() + '(' + args + ');' if ctx.end() else ctx.ID() + '(' + args + ')'

    def visitSType(self, ctx: TargetProgrammer.SType):
        return ctx.type()

    def visitLogic(self, ctx: TargetProgrammer.DXLogic):
        return ctx.left().accept(self) + ' ' + ctx.op() + ' ' + ctx.right().accept(self)
    
    def visitAll(self, ctx: TargetProgrammer.DXAll):
        return 'forall ' + ctx.bind().accept(self) + ' :: ' + ctx.next().accept(self)

    def visitWhile(self, ctx: TargetProgrammer.DXWhile):
        stmts = ''
        for stmt in ctx.stmts():
            stmts += '  ' + stmt.accept(self) + '\n'
            self.line_mapping[self.current_line] = stmt
            self.current_line += 1
        inv = ''
        if ctx.inv():
            for i in ctx.inv():
                inv += '  invariant ' + i.accept(self) + '\n'
                self.line_mapping[self.current_line] = i
                self.current_line += 1
            
        self.current_line += 2
        return 'while(' + ctx.cond().accept(self) + ')\n' + inv + '{\n' + stmts + '}'
    
    def visitIf(self, ctx: TargetProgrammer.DXIf):
        stmts = ''
        for stmt in ctx.left():
            self.line_mapping[self.current_line] = stmt
            self.current_line += 1
            stmts += '  ' + stmt.accept(self) + '\n'

        elsestmts = ''
        for stmt in ctx.right():
            self.line_mapping[self.current_line] = stmt
            self.current_line += 1
            elsestmts += '  ' + stmt.accept(self) + '\n'

        elsepart = ''
        if len(elsestmts) > 0:
            elsepart = '\nelse {\n' + elsestmts + '}' 
            self.current_line += 2

        self.current_line += 1
        return 'if (' + ctx.cond().accept(self) + '){\n' + stmts + '}' + elsepart

    def visitIndex(self, ctx: TargetProgrammer.DXIndex):
        return ctx.bind().accept(self) + '[' + ctx.index().accept(self) + ']'
    
    def visitLength(self, ctx: TargetProgrammer.DXLength):
        return '|' + ctx.var().accept(self) + '|'
    
    def visitComp(self, ctx: TargetProgrammer.DXComp):
        return ctx.left().accept(self) + ' ' + ctx.op() + ' ' + ctx.right().accept(self)
    
    def visitNot(self, ctx: TargetProgrammer.DXNot):
        return '!' + '(' + ctx.next().accept(self) + ')'
    
    def visitInRange(self, ctx: TargetProgrammer.DXInRange):
        return ctx.left().accept(self) + " <= " + ctx.bind().accept(self) + " < " + ctx.right().accept(self)
    
    def visitSeqType(self, ctx: TargetProgrammer.SeqType):
        return 'seq<' + ctx.type().accept(self) + ">"

    def visitIfExp(self, ctx: TargetProgrammer.DXIfExp):
        return 'if ' + ctx.bexp().accept(self) + ' then ' + ctx.left().accept(self) + ' else ' + ctx.right().accept(self)
    
    def visitList(self, ctx: TargetProgrammer.DXList):
        exprs = ''
        for expr in ctx.exprs():
            exprs += expr.accept(self) + ", "
        exprs = exprs[:-2]
        return '[]' if len(ctx.exprs()) == 0 else '[' + exprs + ']'
    
    def visitCast(self, ctx: TargetProgrammer.DXCast):
        return '(' + ctx.next().accept(self) + ' as ' + ctx.type().type() + ')'