import Programmer
from Programmer import *

from AbstractProgramVisitor import AbstractProgramVisitor


class ProgramVisitor(AbstractProgramVisitor):

    def visit(self, ctx):
        match ctx:
            case QXProgram():
                return self.visitProgram(ctx)
            case QXInclude():
                return self.visitInclude(ctx)
            case QXMethod():
                return self.visitMethod(ctx)
            case QXFunction():
                return self.visitFunction(ctx)
            case QXLemma():
                return self.visitLemma(ctx)
            case QXPredicate():
                return self.visitPredicate(ctx)
            case QXRequires():
                return self.visitRequires(ctx)
            case QXEnsures():
                return self.visitEnsures(ctx)
            case QXInvariant():
                return self.visitInvariant(ctx)
            case QXDecreases():
                return self.visitDecreases(ctx)
            case QXSeparates():
                return self.visitSeparates(ctx)
            case QXAssert():
                return self.visitAssert(ctx)
            case QXBreak():
                return self.visitBreak(ctx)
            case QXCall():
                return self.visitCall(ctx)
            case QXCallStmt():
                return self.visitCallStmt(ctx)
            case QXCAssign():
                return self.visitCAssign(ctx)
            case QXCast():
                return self.visitCast(ctx)
            case QXFor():
                return self.visitFor(ctx)
            case QXIf():
                return self.visitIf(ctx)
            case QXInit():
                return self.visitInit(ctx)
            case QXMeasure():
                return self.visitMeasure(ctx)
            case QXMeasureAbort():
                return self.visitMeasureAbort(ctx)
            case QXQAssign():
                return self.visitQAssign(ctx)
            case QXQCreate():
                return self.visitQCreate(ctx)
            case QXReturn():
                return self.visitReturn(ctx)
            case QXWhile():
                return self.visitWhile(ctx)
            case QXPartPredicate():
                return self.visitPartPredicate(ctx)
            case QXPartsection():
                return self.visitPartsection(ctx)
            case QXPart():
                return self.visitPart(ctx)
            case QXPartWithPredicates():
                return self.visitPartWithPredicates(ctx)
            case QXPartGroup():
                return self.visitPartGroup(ctx)
            case QXPartLambda():
                return self.visitPartLambda(ctx)
            case QXPartWithSections():
                return self.visitPartWithSections(ctx)
            case TySingle():
                return self.visitSingleT(ctx)
            case TyArray():
                return self.visitArrayT(ctx)
            case TySet():
                return self.visitTySet(ctx)
            case TyQ():
                return self.visitQ(ctx)
            case TyFun():
                return self.visitFun(ctx)
            case TyNor():
                return self.visitNor(ctx)
            case TyHad():
                return self.visitTyHad(ctx)
            case TyEn():
                return self.visitEn(ctx)
            case TyAA():
                return self.visitAA(ctx)
            case QXBin():
                return self.visitBin(ctx)
            case QXBind():
                return self.visitBind(ctx)
            case QXIfExp():
                return self.visitIfExp(ctx)
            case QXMemberAccess():
                return self.visitMemberAccess(ctx)
            case QXNegation():
                return self.visitNegation(ctx)
            case QXQIndex():
                return self.visitQIndex(ctx)
            case QXSumAExp():
                return self.visitSumAExp(ctx)
            case QXUni():
                return self.visitUni(ctx)
            case QXComp():
                return self.visitBool(ctx)
            case QXCNot():
                return self.visitCNot(ctx)
            case QXLogic():
                return self.visitLogic(ctx)
            case QXQComp():
                return self.visitQComp(ctx)
            case QXQNot():
                return self.visitQNot(ctx)
            case QXQSpec():
                return self.visitQSpec(ctx)
            case QXAll():
                return self.visitAll(ctx)
            case QXSum():
                return self.visitSum(ctx)
            case QXTensor():
                return self.visitTensor(ctx)
            case QXSKet():
                return self.visitSKet(ctx)
            case QXVKet():
                return self.visitVKet(ctx)
            case QXOracle():
                return self.visitOracle(ctx)
            case QXSingle():
                return self.visitSingle(ctx)
            case QXCon():
                return self.visitCon(ctx)
            case QXCRange():
                return self.visitCRange(ctx)
            case QXNum():
                return self.visitNum(ctx)
            case QXBoolLiteral():
                return self.visitBoolLiteral(ctx)
            case QXHad():
                return self.visitHad(ctx)
            case QXSet():
                return self.visitSet(ctx)
            case QXQRange():
                return self.visitQRange(ctx)
            case _:
                raise NotImplementedError(f"No visit method defined for {type(ctx)}")

    # ────────── Root Node ──────────
    def visitProgram(self, ctx: Programmer.QXProgram):
        for elem in ctx.method():
            elem.accept(self)

    # ────────── Top level ──────────
    def visitInclude(self, ctx: Programmer.QXInclude):
        return ctx.path()

    def visitMethod(self, ctx: Programmer.QXMethod):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        for reelem in ctx.returns():
            reelem.accept(self)

        for condelem in ctx.conds():
            condelem.accept(self)

        for stmtelem in ctx.stmts():
            stmtelem.accept(self)

    def visitFunction(self, ctx: Programmer.QXFunction):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        ctx.return_type().accept(self)

        ctx.arith_expr().accept(self)

    def visitLemma(self, ctx: Programmer.QXLemma):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        for condelem in ctx.conds():
            condelem.accept(self)

    def visitPredicate(self, ctx: Programmer.QXPredicate):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        ctx.arith_expr().accept(self)

    # ────────── Conditions ──────────
    def visitRequires(self, ctx: Programmer.QXRequires):
        return ctx.spec().accept(self)

    def visitEnsures(self, ctx: Programmer.QXEnsures):
        return ctx.spec().accept(self)

    def visitInvariant(self, ctx: Programmer.QXInvariant):
        return ctx.spec().accept(self)

    def visitDecreases(self, ctx: Programmer.QXDecreases):
        return ctx.aexp().accept(self)

    def visitSeparates(self, ctx: Programmer.QXSeparates):
        return ctx.locus().accept(self)

    # ────────── Statements ──────────
    def visitAssert(self, ctx: Programmer.QXAssert):
        return ctx.spec().accept(self)

    def visitBreak(self, ctx: QXBreak):
        return 'break'

    def visitCall(self, ctx: Programmer.QXCall):
        for elem in ctx.exps():
            elem.accept(self)
        return ctx.ID()
    
    def visitCallStmt(self, ctx: Programmer.QXCallStmt):
        call_expr = ctx.call_expr()
        return call_expr.accept(self)

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        ctx.aexp().accept(self)
        return ctx.ids()

    def visitCast(self, ctx: Programmer.QXCast):
        ctx.qty().accept(self)
        for elem in ctx.locus():
            elem.accept(self)

    def visitFor(self, ctx: Programmer.QXFor):
        ctx.crange().accept(self)

        for ielem in ctx.conds():
            ielem.accept(self)

        for elem in ctx.stmts():
            elem.accept(self)

        return ctx.ID()

    def visitIf(self, ctx: Programmer.QXIf):
        ctx.bexp().accept(self)
        for elem in ctx.stmts():
            elem.accept(self)

        if ctx.else_stmts():
            for elem in ctx.else_stmts():
                elem.accept(self)

    def visitInit(self, ctx: Programmer.QXInit):
        return ctx.binding().accept(self)

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        for elem in ctx.locus():
            elem.accept(self)
        if ctx.res() is not None:
            ctx.res().accept(self)
        return ctx.ids()

    def visitMeasureAbort(self, ctx: Programmer.QXMeasureAbort):
        for elem in ctx.locus():
            elem.accept(self)
        if ctx.res() is not None:
            ctx.res().accept(self)
        return ctx.ids()

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        for elem in ctx.location():
            elem.accept(self)
        return ctx.exp().accept(self)

    def visitQCreate(self, ctx: Programmer.QXQCreate):
        ctx.qrange().accept(self)
        return ctx.size().accept(self)

    def visitReturn(self, ctx: Programmer.QXReturn):
        return ctx.ids()

    def visitWhile(self, ctx: Programmer.QXWhile):
        ctx.bexp().accept(self)
        for cond in ctx.conds():
            cond.accept(self)
        for stmt in ctx.stmts():
            stmt.accept(self)

    # ────────── Partition ──────────
    def visitPartPredicate(self, ctx: Programmer.QXPartPredicate):
        ctx.amplitude().accept(self)
        ctx.predicate().accept(self)

    def visitPartsection(self, ctx: Programmer.QXPartsection):
        ctx.amplitude().accept(self)
        ctx.ket().accept(self)
        ctx.predicate().accept(self)

    def visitPart(self, ctx: Programmer.QXPart):
        ctx.qnum().accept(self)
        ctx.fname().accept(self)
        ctx.trueAmp().accept(self)
        ctx.falseAmp().accept(self)

    def visitPartWithPredicates(self, ctx: Programmer.QXPartWithPredicates):
        ctx.qnum().accept(self)
        ctx.truePred().accept(self)
        ctx.falsePred().accept(self)

    def visitPartGroup(self, ctx: Programmer.QXPartGroup):
        ctx.fpred().accept(self) if not isinstance(ctx.fpred(), str) else ctx.fpred()
        ctx.bool().accept(self)
        ctx.amplitude().accept(self)

    def visitPartLambda(self, ctx: Programmer.QXPartLambda):
        ctx.fpred().accept(self)
        ctx.amplitude().accept(self)

    def visitPartWithSections(self, ctx: Programmer.QXPartWithSections):
        for section in ctx.sections():
            section.accept(self)

    # ────────── Types ──────────
    def visitSingleT(self, ctx: Programmer.TySingle):
        return ctx.type()

    def visitArrayT(self, ctx: Programmer.TyArray):
        ctx.num().accept(self)
        return ctx.type().accept(self)

    def visitTySet(self, ctx: Programmer.TySet):
        return ctx.type().accept(self)

    def visitQ(self, ctx: Programmer.TyQ):
        return ctx.flag().accept(self)

    def visitFun(self, ctx: Programmer.TyFun):
        for param in ctx.params():
            param.accept(self)
        ctx.return_type().accept(self)

    # ────────── Quantum types ──────────
    def visitNor(self, ctx: Programmer.TyNor):
        return "nor"

    def visitTyHad(self, ctx: Programmer.TyHad):
        return "had"

    def visitEn(self, ctx: Programmer.TyEn):
        return ctx.flag().accept(self)

    def visitAA(self, ctx: Programmer.TyAA):
        return "aa"

    # ────────── Arithmetic expressions ──────────
    def visitBin(self, ctx: Programmer.QXBin):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitBind(self, ctx: Programmer.QXBind):
        if ctx.type() is not None:
            ctx.type().accept(self)
        return ctx.ID()

    def visitIfExp(self, ctx: Programmer.QXIfExp):
        ctx.bexp().accept(self)
        ctx.left().accept(self)
        ctx.right().accept(self)
        return True

    def visitMemberAccess(self, ctx: Programmer.QXMemberAccess):
        return ctx.ids()

    def visitNegation(self, ctx: Programmer.QXNegation):
        return ctx.aexp().accept(self)

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        i = ctx.index().accept(self)
        return ctx.ID() + '[' + str(i)  + ']'

    def visitSumAExp(self, ctx: Programmer.QXSumAExp):
        ctx.sum().accept(self)
        return ctx.aexp().accept(self)

    def visitUni(self, ctx: Programmer.QXUni):
        ctx.next().accept(self)
        return ctx.op()

    # ────────── Boolean expressions ──────────
    def visitBool(self, ctx: Programmer.QXComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitCNot(self, ctx: Programmer.QXCNot):
        return ctx.next().accept(self)

    def visitLogic(self, ctx: Programmer.QXLogic):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitQComp(self, ctx: Programmer.QXQComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        ctx.index().accept(self)
        return ctx.op()

    def visitQNot(self, ctx: Programmer.QXQNot):
        return ctx.next().accept(self)

    def visitQSpec(self, ctx: Programmer.QXQSpec):
        for elem in ctx.locus():
            elem.accept(self)
        for state in ctx.states():
            state.accept(self)
        return ctx.qty().accept(self)

    # ────────── Quantum states ──────────
    def visitAll(self, ctx: Programmer.QXAll):
        ctx.bind().accept(self)
        ctx.bounds().accept(self)
        ctx.next().accept(self)

    def visitSum(self, ctx: Programmer.QXSum):
        # for elem in ctx.kets():
        #     if isinstance(elem, list):
        #         for e in elem:
        #             e.accept(self)
        #     else:
        ctx.kets().accept(self)
        ctx.amp().accept(self)
        for elem in ctx.sums():
            elem.accept(self)

    def visitTensor(self, ctx: Programmer.QXTensor):
        for elem in ctx.kets():
            if isinstance(elem, list):
                for e in elem:
                    e.accept(self)
            else:
                elem.accept(self)
        # if ctx.amp():
        #     ctx.amp().accept(self)
        if ctx.range():
            ctx.range().accept(self)
        return ctx.ID()

    def visitSKet(self, ctx: Programmer.QXSKet):
        return ctx.vector().accept(self)

    def visitVKet(self, ctx: Programmer.QXVKet):
        return ctx.vector().accept(self)

    # ────────── Quantum gates/applications ──────────
    def visitOracle(self, ctx: Programmer.QXOracle):
        for binding in ctx.bindings():
            binding.accept(self)

        ctx.phase().accept(self)
        for elem in ctx.vectors():
            elem.accept(self)

    def visitSingle(self, ctx: Programmer.QXSingle):
        return ctx.op()

    # ────────── Sums and ranges ──────────
    def visitCon(self, ctx: Programmer.QXCon):
        ctx.range().accept(self)
        return ctx.ID()

    def visitCRange(self, ctx: Programmer.QXCRange):
        if ctx.left() is not None:
            ctx.left().accept(self)
        if ctx.right() is not None:
            ctx.right().accept(self)

    # ────────── Literals ──────────
    def visitNum(self, ctx: Programmer.QXNum):
        return ctx.num()

    def visitBoolLiteral(self, ctx: Programmer.QXBoolLiteral):
        return ctx.value()

    def visitHad(self, ctx: Programmer.QXHad):
        return ctx.state()

    def visitSet(self, ctx: Programmer.QXSet):
        for member in ctx.members():
            member.accept(self)

    def visitQRange(self, ctx: Programmer.QXQRange):
        ctx.crange().accept(self)

        if isinstance(ctx.location(), Programmer.QXCall):
            return ctx.location().accept(self)
        else:
            return ctx.location()