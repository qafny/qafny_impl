import Programmer
from ProgramVisitor import ProgramVisitor

class TargetToString(ProgramVisitor):

    def visitBin(self, ctx: Programmer.QXBin):
        return ctx.left().accept(self) + ctx.op() + ctx.right().accept(self)

    def visitBind(self, ctx: Programmer.QXBind):
        return ctx.ID()
    
    def visitNum(self, ctx: Programmer.QXNum):
        return str(ctx.num())