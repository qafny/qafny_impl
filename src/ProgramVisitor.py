from src.Programmer import *

from src.AbstractProgramVisitor import AbstractProgramVisitor


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
    def visitProgram(self, ctx: QXProgram):
        for elem in ctx.method():
            elem.accept(self)

    # ────────── Top level ──────────
    def visitInclude(self, ctx: QXInclude):
        return ctx.path()

    def visitMethod(self, ctx: QXMethod):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        for reelem in ctx.returns():
            reelem.accept(self)

        for condelem in ctx.conds():
            condelem.accept(self)

        for stmtelem in ctx.stmts():
            stmtelem.accept(self)

    def visitFunction(self, ctx: QXFunction):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        ctx.return_type().accept(self)

        ctx.arith_expr().accept(self)

    def visitLemma(self, ctx: QXLemma):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        for condelem in ctx.conds():
            condelem.accept(self)

    def visitPredicate(self, ctx: QXPredicate):
        for bindelem in ctx.bindings():
            bindelem.accept(self)

        ctx.arith_expr().accept(self)

    # ────────── Conditions ──────────
    def visitRequires(self, ctx: QXRequires):
        return ctx.spec().accept(self)

    def visitEnsures(self, ctx: QXEnsures):
        return ctx.spec().accept(self)

    def visitInvariant(self, ctx: QXInvariant):
        return ctx.spec().accept(self)

    def visitDecreases(self, ctx: QXDecreases):
        return ctx.aexp().accept(self)

    def visitSeparates(self, ctx: QXSeparates):
        return ctx.locus().accept(self)

    # ────────── Statements ──────────
    def visitAssert(self, ctx: QXAssert):
        return ctx.spec().accept(self)

    def visitBreak(self, ctx: QXBreak):
        return 'break'

    def visitCall(self, ctx: QXCall):
        for elem in ctx.exps():
            elem.accept(self)
        return ctx.ID()
    
    def visitCallStmt(self, ctx: QXCallStmt):
        call_expr = ctx.call_expr()
        return call_expr.accept(self)

    def visitCAssign(self, ctx: QXCAssign):
        ctx.aexp().accept(self)
        return ctx.ids()

    def visitCast(self, ctx: QXCast):
        ctx.qty().accept(self)
        for elem in ctx.locus():
            elem.accept(self)

    def visitFor(self, ctx: QXFor):
        ctx.crange().accept(self)

        for ielem in ctx.conds():
            ielem.accept(self)

        for elem in ctx.stmts():
            elem.accept(self)

        return ctx.ID()

    def visitIf(self, ctx: QXIf):
        ctx.bexp().accept(self)
        for elem in ctx.stmts():
            elem.accept(self)

        if ctx.else_stmts():
            for elem in ctx.else_stmts():
                elem.accept(self)

    def visitInit(self, ctx: QXInit):
        return ctx.binding().accept(self)

    def visitMeasure(self, ctx: QXMeasure):
        for elem in ctx.locus():
            elem.accept(self)
        if ctx.res() is not None:
            ctx.res().accept(self)
        return ctx.ids()

    def visitMeasureAbort(self, ctx: QXMeasureAbort):
        for elem in ctx.locus():
            elem.accept(self)
        if ctx.res() is not None:
            ctx.res().accept(self)
        return ctx.ids()

    def visitQAssign(self, ctx: QXQAssign):
        for elem in ctx.location():
            elem.accept(self)
        return ctx.exp().accept(self)

    def visitQCreate(self, ctx: QXQCreate):
        ctx.qrange().accept(self)
        return ctx.size().accept(self)

    def visitReturn(self, ctx: QXReturn):
        return ctx.ids()

    def visitWhile(self, ctx: QXWhile):
        ctx.bexp().accept(self)
        for cond in ctx.conds():
            cond.accept(self)
        for stmt in ctx.stmts():
            stmt.accept(self)

    # ────────── Partition ──────────
    def visitPartPredicate(self, ctx: QXPartPredicate):
        ctx.amplitude().accept(self)
        ctx.predicate().accept(self)

    def visitPartsection(self, ctx: QXPartsection):
        ctx.amplitude().accept(self)
        ctx.ket().accept(self)
        ctx.predicate().accept(self)

    def visitPart(self, ctx: QXPart):
        ctx.qnum().accept(self)
        ctx.fname().accept(self)
        ctx.trueAmp().accept(self)
        ctx.falseAmp().accept(self)

    def visitPartWithPredicates(self, ctx: QXPartWithPredicates):
        ctx.qnum().accept(self)
        ctx.truePred().accept(self)
        ctx.falsePred().accept(self)

    def visitPartGroup(self, ctx: QXPartGroup):
        ctx.fpred().accept(self) if not isinstance(ctx.fpred(), str) else ctx.fpred()
        ctx.bool().accept(self)
        ctx.amplitude().accept(self)

    def visitPartLambda(self, ctx: QXPartLambda):
        ctx.fpred().accept(self)
        ctx.amplitude().accept(self)

    def visitPartWithSections(self, ctx: QXPartWithSections):
        for section in ctx.sections():
            section.accept(self)

    # ────────── Types ──────────
    def visitSingleT(self, ctx: TySingle):
        return ctx.type()

    def visitArrayT(self, ctx: TyArray):
        ctx.num().accept(self)
        return ctx.type().accept(self)

    def visitTySet(self, ctx: TySet):
        return ctx.type().accept(self)

    def visitQ(self, ctx: TyQ):
        return ctx.flag().accept(self)

    def visitFun(self, ctx: TyFun):
        for param in ctx.params():
            param.accept(self)
        ctx.return_type().accept(self)

    # ────────── Quantum types ──────────
    def visitNor(self, ctx: TyNor):
        return "nor"

    def visitTyHad(self, ctx: TyHad):
        return "had"

    def visitEn(self, ctx: TyEn):
        return ctx.flag().accept(self)

    def visitAA(self, ctx: TyAA):
        return "aa"

    # ────────── Arithmetic expressions ──────────
    def visitBin(self, ctx: QXBin):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitBind(self, ctx: QXBind):
        if ctx.type() is not None:
            ctx.type().accept(self)
        return ctx.ID()

    def visitIfExp(self, ctx: QXIfExp):
        ctx.bexp().accept(self)
        ctx.left().accept(self)
        ctx.right().accept(self)
        return True

    def visitMemberAccess(self, ctx: QXMemberAccess):
        return ctx.ids()

    def visitNegation(self, ctx: QXNegation):
        return ctx.aexp().accept(self)

    def visitQIndex(self, ctx: QXQIndex):
        i = ctx.index().accept(self)
        return ctx.ID() + '[' + str(i)  + ']'

    def visitSumAExp(self, ctx: QXSumAExp):
        ctx.sum().accept(self)
        return ctx.aexp().accept(self)

    def visitUni(self, ctx: QXUni):
        ctx.next().accept(self)
        return ctx.op()

    # ────────── Boolean expressions ──────────
    def visitBool(self, ctx: QXComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitCNot(self, ctx: QXCNot):
        return ctx.next().accept(self)

    def visitLogic(self, ctx: QXLogic):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitQComp(self, ctx: QXQComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        ctx.index().accept(self)
        return ctx.op()

    def visitQNot(self, ctx: QXQNot):
        return ctx.next().accept(self)

    def visitQSpec(self, ctx: QXQSpec):
        for elem in ctx.locus():
            elem.accept(self)
        for state in ctx.states():
            state.accept(self)
        return ctx.qty().accept(self)

    # ────────── Quantum states ──────────
    def visitAll(self, ctx: QXAll):
        ctx.bind().accept(self)
        ctx.bounds().accept(self)
        ctx.next().accept(self)

    def visitSum(self, ctx: QXSum):
        # for elem in ctx.kets():
        #     if isinstance(elem, list):
        #         for e in elem:
        #             e.accept(self)
        #     else:
        ctx.kets().accept(self)
        ctx.amp().accept(self)
        for elem in ctx.sums():
            elem.accept(self)

    def visitTensor(self, ctx: QXTensor):
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

    def visitSKet(self, ctx: QXSKet):
        return ctx.vector().accept(self)

    def visitVKet(self, ctx: QXVKet):
        return ctx.vector().accept(self)

    # ────────── Quantum gates/applications ──────────
    def visitOracle(self, ctx: QXOracle):
        for binding in ctx.bindings():
            binding.accept(self)

        ctx.phase().accept(self)
        for elem in ctx.vectors():
            elem.accept(self)

    def visitSingle(self, ctx: QXSingle):
        return ctx.op()

    # ────────── Sums and ranges ──────────
    def visitCon(self, ctx: QXCon):
        ctx.range().accept(self)
        return ctx.ID()

    def visitCRange(self, ctx: QXCRange):
        if ctx.left() is not None:
            ctx.left().accept(self)
        if ctx.right() is not None:
            ctx.right().accept(self)

    # ────────── Literals ──────────
    def visitNum(self, ctx: QXNum):
        return ctx.num()

    def visitBoolLiteral(self, ctx: QXBoolLiteral):
        return ctx.value()

    def visitHad(self, ctx: QXHad):
        return ctx.state()

    def visitSet(self, ctx: QXSet):
        for member in ctx.members():
            member.accept(self)

    def visitQRange(self, ctx: QXQRange):
        ctx.crange().accept(self)

        if isinstance(ctx.location(), QXCall):
            return ctx.location().accept(self)
        else:
            return ctx.location()