from abc import ABC, abstractmethod


class AbstractProgramVisitor(ABC):
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
    def visitCast(self, ctx):
        pass

    @abstractmethod
    def visitCRange(self, ctx):
        pass

    @abstractmethod
    def visitBind(self, ctx):
        pass

    @abstractmethod
    def visitQAssign(self, ctx):
        pass

    @abstractmethod
    def visitCAssign(self, ctx):
        pass

    @abstractmethod
    def visitMeasure(self, ctx):
        pass

    @abstractmethod
    def visitIf(self, ctx):
        pass

    @abstractmethod
    def visitFor(self, ctx):
        pass

    @abstractmethod
    def visitCall(self, ctx):
        pass

    @abstractmethod
    def visitSingleT(self, ctx):
        pass

    @abstractmethod
    def visitCNot(self, ctx):
        pass

    @abstractmethod
    def visitQ(self, ctx):
        pass

    @abstractmethod
    def visitFun(self, ctx):
        pass

    @abstractmethod
    def visitNor(self, ctx):
        pass

    @abstractmethod
    def visitTyHad(self, ctx):
        pass

    @abstractmethod
    def visitEn(self, ctx):
        pass

    @abstractmethod
    def visitAA(self, ctx):
        pass

    @abstractmethod
    def visitQSpec(self, ctx):
        pass

    @abstractmethod
    def visitTensor(self, ctx):
        pass

    @abstractmethod
    def visitSKet(self, ctx):
        pass

    @abstractmethod
    def visitVKet(self, ctx):
        pass

    @abstractmethod
    def visitSum(self, ctx):
        pass

    @abstractmethod
    def visitVarState(self, ctx):
        pass

    @abstractmethod
    def visitPart(self, ctx):
        pass

    @abstractmethod
    def visitCon(self, ctx):
        pass

    @abstractmethod
    def visitBool(self, ctx):
        pass

    def visitLogic(self, ctx):
        pass

    @abstractmethod
    def visitQIndex(self, ctx):
        pass

    @abstractmethod
    def visitQComp(self, ctx):
        pass

    @abstractmethod
    def visitQNot(self, ctx):
        pass

    @abstractmethod
    def visitAll(self, ctx):
        pass

    @abstractmethod
    def visitBin(self, ctx):
        pass

    @abstractmethod
    def visitUni(self, ctx):
        pass

    @abstractmethod
    def visitSingle(self, ctx):
        pass

    @abstractmethod
    def visitOracle(self, ctx):
        pass

    @abstractmethod
    def visitNum(self, ctx):
        pass

    @abstractmethod
    def visitHad(self, ctx):
        pass

    @abstractmethod
    def visitQRange(self, ctx):
        pass