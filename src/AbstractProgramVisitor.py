from abc import ABC, abstractmethod


class AbstractProgramVisitor(ABC):
    """
    Abstract Base Class for vistors to parsed qafny programs.
    """

    @abstractmethod
    def visit(self, ctx: 'QXTop'):
        pass

    # ────────── Root Node ──────────
    @abstractmethod
    def visitProgram(self, ctx: 'QXProgram'):
        pass

    # ────────── Top level ──────────
    @abstractmethod
    def visitInclude(self, ctx: 'QXInclude'):
        pass

    @abstractmethod
    def visitMethod(self, ctx: 'QXMethod'):
        pass

    @abstractmethod
    def visitFunction(self, ctx: 'QXFunction'):
        pass

    @abstractmethod
    def visitLemma(self, ctx: 'QXLemma'):
        pass

    @abstractmethod
    def visitPredicate(self, ctx: 'QXPredicate'):
        pass
    
    # ────────── Conditions ──────────
    @abstractmethod
    def visitRequires(self, ctx: 'QXRequires'):
        pass

    @abstractmethod
    def visitEnsures(self, ctx: 'QXEnsures'):
        pass

    @abstractmethod
    def visitInvariant(self, ctx: 'QXInvariant'):
        pass

    @abstractmethod
    def visitDecreases(self, ctx: 'QXDecreases'):
        pass

    @abstractmethod
    def visitSeparates(self, ctx: 'QXSeparates'):
        pass

    # ────────── Statements ──────────
    @abstractmethod
    def visitAssert(self, ctx: 'QXAssert'):
        pass

    @abstractmethod
    def visitBreak(self, ctx: 'QXBreak'):
        pass

    @abstractmethod
    def visitCall(self, ctx: 'QXCall'):
        pass

    @abstractmethod
    def visitCAssign(self, ctx: 'QXCAssign'):
        pass

    @abstractmethod
    def visitCast(self, ctx: 'QXCast'):
        pass

    @abstractmethod
    def visitFor(self, ctx: 'QXFor'):
        pass

    @abstractmethod
    def visitIf(self, ctx: 'QXIf'):
        pass

    @abstractmethod
    def visitInit(self, ctx: 'QXInit'):
        pass

    @abstractmethod
    def visitMeasure(self, ctx: 'QXMeasure'):
        pass

    @abstractmethod
    def visitMeasureAbort(self, ctx: 'QXMeasureAbort'):
        pass

    @abstractmethod
    def visitQAssign(self, ctx: 'QXQAssign'):
        pass

    @abstractmethod
    def visitQCreate(self, ctx: 'QXQCreate'):
        pass

    @abstractmethod
    def visitReturn(self, ctx: 'QXReturn'):
        pass

    @abstractmethod
    def visitWhile(self, ctx: 'QXWhile'):
        pass

    # ────────── Partition ──────────
    @abstractmethod
    def visitPartPredicate(self, ctx: 'QXPartPredicate'):
        pass

    @abstractmethod
    def visitPartsection(self, ctx: 'QXPartsection'):
        pass

    @abstractmethod
    def visitPart(self, ctx: 'QXPart'):
        pass

    @abstractmethod
    def visitPartWithPredicates(self, ctx: 'QXPartWithPredicates'):
        pass

    @abstractmethod
    def visitPartGroup(self, ctx: 'QXPartGroup'):
        pass

    @abstractmethod
    def visitPartLambda(self, ctx: 'QXPartLambda'):
        pass

    @abstractmethod
    def visitPartWithSections(self, ctx: 'QXPartWithSections'):
        pass

    # ────────── Types ──────────
    @abstractmethod
    def visitSingleT(self, ctx: 'TySingle'):
        pass

    @abstractmethod
    def visitArrayT(self, ctx: 'TyArray'):
        pass

    @abstractmethod
    def visitTySet(self, ctx: 'TySet'):
        pass

    @abstractmethod
    def visitQ(self, ctx: 'TyQ'):
        pass

    @abstractmethod
    def visitFun(self, ctx: 'TyFun'):
        pass

    # ────────── Quantum types ──────────
    @abstractmethod
    def visitNor(self, ctx: 'TyNor'):
        pass

    @abstractmethod
    def visitTyHad(self, ctx: 'TyHad'):
        pass

    @abstractmethod
    def visitEn(self, ctx: 'TyEn'):
        pass

    @abstractmethod
    def visitAA(self, ctx: 'TyAA'):
        pass

    # ────────── Arithmetic expressions ──────────
    @abstractmethod
    def visitBin(self, ctx: 'QXBin'):
        pass

    @abstractmethod
    def visitBind(self, ctx: 'QXBind'):
        pass

    @abstractmethod
    def visitIfExp(self, ctx: 'QXIfExp'):
        pass

    @abstractmethod
    def visitMemberAccess(self, ctx: 'QXMemberAccess'):
        pass

    @abstractmethod
    def visitNegation(self, ctx: 'QXNegation'):
        pass

    @abstractmethod
    def visitQIndex(self, ctx: 'QXQIndex'):
        pass

    @abstractmethod
    def visitSumAExp(self, ctx: 'QXSumAExp'):
        pass

    @abstractmethod
    def visitUni(self, ctx: 'QXUni'):
        pass

    # ────────── Boolean expressions ──────────
    @abstractmethod
    def visitBool(self, ctx: 'QXComp'):
        pass

    @abstractmethod
    def visitCNot(self, ctx: 'QXCNot'):
        pass

    def visitLogic(self, ctx: 'QXLogic'):
        pass

    @abstractmethod
    def visitQComp(self, ctx: 'QXQComp'):
        pass

    @abstractmethod
    def visitQNot(self, ctx: 'QXQNot'):
        pass

    @abstractmethod
    def visitQSpec(self, ctx: 'QXQSpec'):
        pass

    # ────────── Quantum states ──────────
    @abstractmethod
    def visitAll(self, ctx: 'QXAll'):
        pass

    @abstractmethod
    def visitSum(self, ctx: 'QXSum'):
        pass

    @abstractmethod
    def visitTensor(self, ctx: 'QXTensor'):
        pass

    @abstractmethod
    def visitSKet(self, ctx: 'QXSKet'):
        pass

    @abstractmethod
    def visitVKet(self, ctx: 'QXVKet'):
        pass

    # ────────── Quantum gates/applications ──────────
    @abstractmethod
    def visitOracle(self, ctx: 'QXOracle'):
        pass

    @abstractmethod
    def visitSingle(self, ctx: 'QXSingle'):
        pass

    # ────────── Sums and ranges ──────────
    @abstractmethod
    def visitCon(self, ctx: 'QXCon'):
        pass

    @abstractmethod
    def visitCRange(self, ctx: 'QXCRange'):
        pass

    # ────────── Literals ──────────
    @abstractmethod
    def visitNum(self, ctx: 'QXNum'):
        pass

    @abstractmethod
    def visitBoolLiteral(self, ctx: 'QXBoolLiteral'):
        pass

    @abstractmethod
    def visitHad(self, ctx: 'QXHad'):
        pass

    @abstractmethod
    def visitSet(self, ctx: 'QXSet'):
        pass

    @abstractmethod
    def visitQRange(self, ctx: 'QXQRange'):
        pass