import Programmer
from ProgramVisitor import ProgramVisitor

class BindingCollector(ProgramVisitor):


    def __init__(self):
        pass

    def visitBin(self, ctx: Programmer.QXBin):
        return  ctx.left().accept(self) + ctx.right().accept(self)
    
    def visitBind(self, ctx: Programmer.QXBind):
        return [ctx]
    
    def visitNum(self, ctx: Programmer.QXNum):
        return []