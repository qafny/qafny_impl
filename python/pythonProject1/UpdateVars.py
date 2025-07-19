#import Programmer
#from ProgramVisitor import ProgramVisitor
from SubstDAExp import SubstDAExp
#from TargetProgramVisitor import TargetProgramVisitor
from TargetProgrammer import *


class UpdateVars(SubstDAExp):

    def __init__(self, counter, qs:dict):
        self._counter = counter
        self._vars = qs

    def counter(self):
        return self._counter

    def vars(self):
        return self._vars

    def visitBind(self, ctx: DXBind):
        v = self._vars(ctx.ID()).newBind(self._counter+1)
        self._counter +=1
        return v