from abc import ABC, abstractmethod


class AbstractTargetVisitor(ABC):
    """
    Abstract Base Class for vistors to parsed qafny programs.
    """

    @abstractmethod
    def visit(self, ctx):
        pass

    @abstractmethod
    def visitMethod(self, ctx):
        pass

    @abstractmethod
    def visitProgram(self, ctx):
        pass

    @abstractmethod
    def visitAll(self, ctx):
        pass

    @abstractmethod
    def visitNot(self, ctx):
        pass

    @abstractmethod
    def visitComp(self, ctx):
        pass

    @abstractmethod
    def visitInRange(self, ctx):
        pass

    def visitLogic(self, ctx):
        pass

    @abstractmethod
    def visitAssert(self, ctx):
        pass

    @abstractmethod
    def visitRequires(self, ctx):
        pass

    @abstractmethod
    def visitEnsures(self, ctx):
        pass

    @abstractmethod
    def visitInit(self, ctx):
        pass

    @abstractmethod
    def visitAssign(self, ctx):
        pass

    @abstractmethod
    def visitIf(self, ctx):
        pass

    @abstractmethod
    def visitWhile(self, ctx):
        pass

    @abstractmethod
    def visitIndex(self, ctx):
        pass

    @abstractmethod
    def visitBin(self, ctx):
        pass

    @abstractmethod
    def visitIfExp(self, ctx):
        pass

    @abstractmethod
    def visitUni(self, ctx):
        pass

    @abstractmethod
    def visitBind(self, ctx):
        pass

    @abstractmethod
    def visitNum(self, ctx):
        pass

    @abstractmethod
    def visitReal(self, ctx):
        pass

    @abstractmethod
    def visitCast(self, ctx):
        pass

    @abstractmethod
    def visitCall(self, ctx):
        pass

    @abstractmethod
    def visitSType(self, ctx):
        pass

    @abstractmethod
    def visitSeqType(self, ctx):
        pass

    @abstractmethod
    def visitFunType(self, ctx):
        pass