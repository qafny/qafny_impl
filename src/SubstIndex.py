import ProgramTransfer
from SubstDAExp import SubstDAExp
#from TargetProgramVisitor import TargetProgramVisitor
from TargetProgrammer import *
#from AbstractTargetVisitor import AbstractTargetVisitor
#from TypeChecker import *


class SubstIndex(SubstDAExp):

    def __init__(self, id: str, e: [DXAExp]):
        # need st --> state we are deling with
        # kind map from fun vars to kind maps
        # self.kenv = kenv
        # the checked type env at index
        self._id = id
        self._exp = e

    def exp(self):
        return self._exp

    def visitBind(self, ctx: DXBind):
        if ctx == self._id:
            return ProgramTransfer.constructIndex(ctx, self.exp())
        else:
            return ctx