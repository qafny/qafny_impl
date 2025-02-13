# type checker
import copy
from enum import Enum
from collections import ChainMap
from operator import truediv

#from types import NoneType

from antlr4 import ParserRuleContext

from ExpLexer import *
from ExpVisitor import *
from Programmer import *
from ExpParser import *

""" Transforms an ANTLR AST into a Qafny one.
"""
class ProgramTransformer(ExpVisitor):

    def visitProgram(self, ctx:ExpParser.ProgramContext):
        i = 0
        tmp = []
        while(ctx.method(i) is not None):
            tmp.append(self.visitMethod(ctx.method(i)))
            i = i + 1
        return QXProgram(tmp)


    # Visit a parse tree produced by ExpParser#method.
    def visitMethod(self, ctx:ExpParser.MethodContext):
        bindings = self.visitBindings(ctx.bindings())
        if ctx.Axiom() is not None:
            axiom = True
        else:
            axiom = False
        returns = self.visitReturna(ctx.returna())
        conds = self.visitConds(ctx.conds())
        if not axiom:
            stmts = self.visitStmts(ctx.stmts())
        else:
            stmts = []
        return QXMethod(ctx.ID(), axiom, bindings, returns, conds, stmts)

    # Visit a parse tree produced by ExpParser#returna.
    def visitReturna(self, ctx:ExpParser.ReturnaContext):
        if ctx is None:
            return []
        return self.visitBindings(ctx.bindings())

    def dealWithList(self, op: str, specs: [QXSpec]):
        tmp = []
        if op == "requires":
            for elem in specs:
                tmp.append(QXRequires(elem))
            return tmp
        if op == "ensures":
            for elem in specs:
                tmp.append(QXEnsures(elem))
            return tmp

    # Visit a parse tree produced by ExpParser#conds.
    def visitConds(self, ctx:ExpParser.CondsContext):
        if ctx is None:
            return None
        i = 0
        tmp = []
        while(ctx.spec(i) is not None):
            top = self.visitReen(ctx.reen(i))
            tmp = tmp + self.dealWithList(top, self.visitSpec(ctx.spec(i)))
            i = i + 1
        return tmp

            # Visit a parse tree produced by ExpParser#reen.
    def visitReen(self, ctx: ExpParser.ReenContext):
        if ctx.Requires() is not None:
            return "requires"
        if ctx.Ensures() is not None:
            return "ensures"

    # Visit a parse tree produced by ExpParser#invariants.
    def visitInvariants(self, ctx:ExpParser.InvariantsContext):
        i = 0
        tmp = []
        while(ctx.spec(i) is not None):
            vs = self.visitSpec(ctx.spec(i))
            tmp = tmp + vs
            i = i + 1
        return tmp

    # Visit a parse tree produced by ExpParser#stmts.
    def visitStmts(self, ctx:ExpParser.StmtsContext):
        if ctx is None:
            return None
        i = 0
        tmp = []
        while(ctx.stmt(i) is not None):
            tmp = tmp + (self.visitStmt(ctx.stmt(i)))
            i = i + 1
        return tmp

    # Visit a parse tree produced by ExpParser#stmt.
    def visitStmt(self, ctx:ExpParser.StmtContext):
        if ctx.fcall() is not None:
            return [self.visitFcall(ctx.fcall())]
        if ctx.ifexp() is not None:
            return [self.visitIfexp(ctx.ifexp())]
        if ctx.forexp() is not None:
            return [self.visitForexp(ctx.forexp())]
        if ctx.measure() is not None:
            return [self.visitMeasure(ctx.measure())]
        if ctx.qassign() is not None:
            return [self.visitQassign(ctx.qassign())]
        if ctx.assigning() is not None:
            return [self.visitAssigning(ctx.assigning())]
        if ctx.varcreate() is not None:
            return self.visitVarcreate(ctx.varcreate())
        if ctx.casting() is not None:
            return [self.visitCasting(ctx.casting())]
        if ctx.asserting() is not None:
            return self.visitAsserting(ctx.asserting())


    # Visit a parse tree produced by ExpParser#spec.
    def visitSpec(self, ctx:ExpParser.SpecContext):
        if ctx.qunspec() is not None:
            return [self.visitQunspec(ctx.qunspec())]
        if ctx.allspec() is not None:
            return [self.visitAllspec(ctx.allspec())]
        if ctx.logicImply() is not None:
            return [self.visitLogicImply(ctx.logicImply())]
        if ctx.chainBExp() is not None:
            return self.visitChainBExp(ctx.chainBExp())


    # Visit a parse tree produced by ExpParser#allspec.
    def visitAllspec(self, ctx:ExpParser.AllspecContext):
        bind = self.visitBinding(ctx.binding())
        imply = self.visitLogicImply(ctx.logicImply())
        return QXAll(bind, imply)


    # Visit a parse tree produced by ExpParser#bexp.
    def visitBexp(self, ctx:ExpParser.BexpContext):
        if ctx.qbool() is not None:
            return self.visitQbool(ctx.qbool())
        if ctx.logicOrExp() is not None:
            return self.visitLogicOrExp(ctx.logicOrExp())


    # Visit a parse tree produced by ExpParser#qbool.
    def visitQbool(self, ctx:ExpParser.QboolContext):
        if ctx.qbool() is not None:
            v = self.visitQbool(ctx.qbool())
            return QXQNot(v)
        if ctx.comOp() is not None:
            left = self.visitArithExpr(ctx.arithExpr(0))
            right = self.visitArithExpr(ctx.arithExpr(1))
            op = self.visitComOp(ctx.comOp())
            index = self.visitQindex(ctx.qindex())
            return QXQComp(op, left, right, index)
        return self.visitQindex(ctx.qindex())


    # Visit a parse tree produced by ExpParser#logicImply.
    def visitLogicImply(self, ctx:ExpParser.LogicImplyContext):
        if ctx.logicImply() is not None:
            v2 = self.visitLogicImply(ctx.logicImply())
            v1 = self.visitLogicOrExp(ctx.logicOrExp())
            return QXLogic("==>", v1, v2)
        return self.visitLogicOrExp(ctx.logicOrExp())


    # Visit a parse tree produced by ExpParser#logicOrExp.
    def visitLogicOrExp(self, ctx:ExpParser.LogicOrExpContext):
        if not ctx:
            return None
        if ctx.logicOrExp() is not None:
            v1 = self.visitLogicAndExp(ctx.logicAndExp())
            v2 = self.visitLogicOrExp(ctx.logicOrExp())
            return QXLogic("||", v1, v2)
        return self.visitLogicAndExp(ctx.logicAndExp())


    # Visit a parse tree produced by ExpParser#logicAndExp.
    def visitLogicAndExp(self, ctx:ExpParser.LogicAndExpContext):
        if ctx.logicAndExp() is not None:
            v1 = self.visitLogicNotExp(ctx.logicNotExp())
            v2 = self.visitLogicAndExp(ctx.logicAndExp())
            return QXLogic("&&", v1, v2)
        return self.visitLogicNotExp(ctx.logicNotExp())


    # Visit a parse tree produced by ExpParser#logicNotExp.
    def visitLogicNotExp(self, ctx:ExpParser.LogicNotExpContext):
        if ctx.logicNotExp() is not None:
            return QXCNot(self.visitLogicNotExp(ctx.logicNotExp()))
        if ctx.fcall() is not None:
            return self.visitFcall(ctx.fcall())
        if ctx.chainBExp() is not None:
            vs = self.visitChainBExp(ctx.chainBExp())
            return vs

    # Visit a parse tree produced by ExpParser#chainBExp.
    def visitChainBExp(self, ctx:ExpParser.ChainBExpContext):
        i = 0
        va = []
        op = []
        while ctx.arithExpr(i):
            va.append(self.visitArithExpr(ctx.arithExpr(i)))
            i += 1
        i = 0
        while ctx.comOp(i):
            op.append(self.visitComOp(ctx.comOp(i)))
            i += 1
        i = len(op) - 1
        while i >= 0:
            va[i] = QXComp(op[i], va[i], va[i+1])
            i -= 1
        return va[0]


    # Visit a parse tree produced by ExpParser#comOp.
    def visitComOp(self, ctx:ExpParser.ComOpContext):
        if ctx.EQ() is not None:
            return "=="
        if ctx.GE() is not None:
            return ">="
        if ctx.LE() is not None:
            return "<="
        if ctx.LT() is not None:
            return "<"
        if ctx.GT() is not None:
            return ">"


    # Visit a parse tree produced by ExpParser#qunspec.
    def visitQunspec(self, ctx:ExpParser.QunspecContext):
        locus = self.visitLocus(ctx.locus())
        qty = self.visitQty(ctx.qty())
        state = self.visitQspec(ctx.qspec())
        return QXQSpec(locus, qty, state)


    # Visit a parse tree produced by ExpParser#qspec.
    def visitQspec(self, ctx:ExpParser.QspecContext):
        if ctx.tensorall() is not None:
            return self.visitTensorall(ctx.tensorall())
        if ctx.manyket() is not None:
            if ctx.arithExpr() is not None:
                return QXTensor(self.visitManyket(ctx.manyket()), None, None, self.visitArithExpr(ctx.arithExpr()))
            else:
                return QXTensor(self.visitManyket(ctx.manyket()))
        if ctx.sumspec() is not None:
            return self.visitSumspec(ctx.sumspec())

    # Visit a parse tree produced by ExpParser#tensorall.
    def visitTensorall(self, ctx:ExpParser.TensorallContext):
        v = None
        if ctx.crange() is not None:
            v = self.visitCrange(ctx.crange())
        return QXTensor(self.visitManyket(ctx.manyket()), ctx.ID(), v)


    # Visit a parse tree produced by ExpParser#sumspec.
    def visitSumspec(self, ctx:ExpParser.SumspecContext):
        sums = self.visitMaySum(ctx.maySum())
        amp = self.visitArithExpr(ctx.arithExpr())
        kets = self.visitManyket(ctx.manyket())
        return QXSum(sums, amp, kets)

    def visitPartspec(self, ctx:ExpParser.PartspecContext):
        num = ctx.arithExpr(0).accept(self)
        fname = ctx.arithExpr(1).accept(self)
        true = ctx.arithExpr(2).accept(self)
        false = ctx.arithExpr(3).accept(self)
        return QXPart(num, fname, true, false)


    # Visit a parse tree produced by ExpParser#maySum.
    def visitMaySum(self, ctx:ExpParser.MaySumContext):
        tmp = []
        i = 0
        while ctx.ID(i) is not None:
            tmp.append(QXCon(ctx.ID(i), self.visitCrange(ctx.crange(i))))
            i = i + 1
        return tmp


    # Visit a parse tree produced by ExpParser#asserting.
    def visitAsserting(self, ctx:ExpParser.AssertingContext):
        tmp = self.visitSpec(ctx.spec())
        value = []
        for elem in tmp:
            value.append(QXAssert(elem))
        return value


    # Visit a parse tree produced by ExpParser#casting.
    def visitCasting(self, ctx:ExpParser.CastingContext):
        qty = self.visitQty(ctx.qty())
        locus = self.visitLocus(ctx.locus())
        return QXCast(qty, locus)


    # Visit a parse tree produced by ExpParser#varcreate.
    def visitVarcreate(self, ctx:ExpParser.VarcreateContext):
        bind = self.visitBinding(ctx.binding())
        value = self.visitArithExpr(ctx.arithExpr())
        return [QXInit(bind), QXCAssign(bind.ID(),value)]


    # Visit a parse tree produced by ExpParser#assigning.
    def visitAssigning(self, ctx:ExpParser.AssigningContext):
        return QXCAssign(ctx.ID(), self.visitArithExpr(ctx.arithExpr()))


    # Visit a parse tree produced by ExpParser#ids.
    def visitIds(self, ctx:ExpParser.IdsContext):
        i = 0
        tmp = []
        while ctx.ID(i) is not None:
            tmp.append(ctx.ID(i))
            i = i + 1
        return tmp

    # Visit a parse tree produced by ExpParser#qassign.
    def visitQassign(self, ctx:ExpParser.QassignContext):
        locus = self.visitLocus(ctx.locus())
        exp = self.visitExpr(ctx.expr())
        return QXQAssign(locus, exp)


    # Visit a parse tree produced by ExpParser#measure.
    def visitMeasure(self, ctx:ExpParser.MeasureContext):
        locus = self.visitLocus(ctx.locus())
        ids = self.visitIds(ctx.ids())
        if ctx.arithExpr() is not None:
            restrict = self.visitArithExpr(ctx.arithExpr())
        return QXMeasure(ids, locus, restrict)


    # Visit a parse tree produced by ExpParser#ifexp.
    def visitIfexp(self, ctx:ExpParser.IfexpContext):
        bexp = self.visitBexp(ctx.bexp())
        stmts = self.visitStmts(ctx.stmts())
        return QXIf(bexp, stmts)


    # Visit a parse tree produced by ExpParser#forexp.
    def visitForexp(self, ctx:ExpParser.ForexpContext):
        id = ctx.ID()
        crange = self.visitCrange(ctx.crange())
        inv = self.visitInvariants(ctx.invariants())
        stmts = self.visitStmts(ctx.stmts())
        return QXFor(id, crange, inv, stmts)


    # Visit a parse tree produced by ExpParser#fcall.
    def visitFcall(self, ctx:ExpParser.FcallContext):
        return QXCall(ctx.ID(), self.visitArithExprs(ctx.arithExprs()))


    # Visit a parse tree produced by ExpParser#arithExprs.
    def visitArithExprs(self, ctx:ExpParser.ArithExprsContext):
        tmp = []
        i = 0
        while ctx.arithExpr(i) is not None:
            tmp.append(self.visitArithExpr(ctx.arithExpr(i)))
            i = i + 1
        return tmp

    # Visit a parse tree produced by ExpParser#arithExpr.
    def visitArithExpr(self, ctx:ExpParser.ArithExprContext):
        if ctx.arithExpr() is not None:
            op = self.visitOp(ctx.op())
            v2 = self.visitArithExpr(ctx.arithExpr())
            v1 = self.visitArithAtomic(ctx.arithAtomic())
            return QXBin(op, v1, v2)
        return self.visitArithAtomic(ctx.arithAtomic())


    # Visit a parse tree produced by ExpParser#arithAtomic.
    def visitArithAtomic(self, ctx:ExpParser.ArithAtomicContext):
        if ctx.ID() is not None:
            return QXBind(ctx.ID())
        if ctx.numexp() is not None:
            return self.visitNumexp(ctx.numexp())
        if ctx.arithExpr() is not None:
            return self.visitArithExpr(ctx.arithExpr())
        if ctx.fcall() is not None:
            return self.visitFcall(ctx.fcall())
        if ctx.absExpr() is not None:
            return self.visitAbsExpr(ctx.absExpr())
        if ctx.sinExpr() is not None:
            return self.visitSinExpr(ctx.sinExpr())
        if ctx.cosExpr() is not None:
            return self.visitCosExpr(ctx.cosExpr())
        if ctx.sqrtExpr() is not None:
            return self.visitSqrtExpr(ctx.sqrtExpr())
        if ctx.omegaExpr() is not None:
            return self.visitOmegaExpr(ctx.omegaExpr())
        if ctx.qindex() is not None:
            return self.visitQindex(ctx.qindex())

    # Visit a parse tree produced by ExpParser#sinExpr.
    def visitSinExpr(self, ctx:ExpParser.SinExprContext):
        return QXUni("sin", self.visitArithExpr(ctx.arithExpr()))


    # Visit a parse tree produced by ExpParser#cosExpr.
    def visitCosExpr(self, ctx:ExpParser.CosExprContext):
        return QXUni("cos", self.visitArithExpr(ctx.arithExpr()))


    # Visit a parse tree produced by ExpParser#sqrtExpr.
    def visitSqrtExpr(self, ctx:ExpParser.SqrtExprContext):
        return QXUni("sqrt", self.visitArithExpr(ctx.arithExpr()))


    # Visit a parse tree produced by ExpParser#absExpr.
    def visitAbsExpr(self, ctx:ExpParser.AbsExprContext):
        return QXUni("abs", self.visitArithExpr(ctx))


    # Visit a parse tree produced by ExpParser#omegaExpr.
    def visitOmegaExpr(self, ctx:ExpParser.OmegaExprContext):
        return QXCall("omega", [self.visitArithExpr(ctx.arithExpr(0)), self.visitAbsExpr(ctx.arithExpr(1))])


    # Visit a parse tree produced by ExpParser#expr.
    def visitExpr(self, ctx:ExpParser.ExprContext):
        if ctx.SHad() is not None:
            return QXSingle("H")
        if ctx.SQFT() is not None:
            return QXSingle("QFT")
        if ctx.RQFT() is not None:
            return QXSingle("RQFT")
        if ctx.lambdaT() is not None:
            return self.visitLambdaT(ctx.lambdaT())

    def genKet(self, ids: [str]):
        tmp = []
        for elem in ids:
            tmp.append(QXKet(QXBind(elem)))
        return tmp


    # Visit a parse tree produced by ExpParser#lambdaT.
    def visitLambdaT(self, ctx:ExpParser.LambdaTContext):
        ids = self.visitIds(ctx.ids())
        if ctx.omegaExpr() is None:
            omega = QXCall('omega', [QXNum(0), QXNum(1)])
        else:
            omega = self.visitOmegaExpr(ctx.omegaExpr())
        if ctx.manyket() is None:
            kets = self.genKet(ids)
        else:
            kets = self.visitManyket(ctx.manyket())
        return QXOracle(ids, omega, kets)


    # Visit a parse tree produced by ExpParser#manyket.
    def visitManyket(self, ctx:ExpParser.ManyketContext):
        kets = []
        i = 0
        while ctx.ket(i) is not None:
            kets.append(self.visitKet(ctx.ket(i)))
            i = i + 1
        return kets


    # Visit a parse tree produced by ExpParser#ket.
    def visitKet(self, ctx:ExpParser.KetContext):
        if ctx.qstate() is not None:
            return QXSKet(self.visitQstate(ctx.qstate()))
        if ctx.arithExpr() is not None:
            return QXVKet(self.visitArithExpr(ctx.arithExpr()))

    # Visit a parse tree produced by ExpParser#qstate.
    def visitQstate(self, ctx:ExpParser.QstateContext):
        if ctx.arithExpr() is not None:
            return self.visitArithExpr(ctx.arithExpr())
        else:
            return QXHad(self.visitAddOp(ctx.addOp()))


    # Visit a parse tree produced by ExpParser#bindings.
    def visitBindings(self, ctx:ExpParser.BindingsContext):
        i = 0
        tmp = []
        while ctx.binding(i) is not None:
            tmp.append(self.visitBinding(ctx.binding(i)))
            i = i + 1
        return tmp


    # Visit a parse tree produced by ExpParser#binding.
    def visitBinding(self, ctx:ExpParser.BindingContext):
        id = ctx.ID()
        t = self.visitTypeT(ctx.typeT())
        return QXBind(id, t)


    # Visit a parse tree produced by ExpParser#locus.
    def visitLocus(self, ctx:ExpParser.LocusContext):
        i = 0
        tmp = []
        while ctx.qrange(i) is not None:
            x = self.visitQrange(ctx.qrange(i))
            if isinstance(x, list):
                tmp.extend(x)
            else:    
                tmp.append(x)
            i = i + 1
        return tmp

    # Visit a parse tree produced by ExpParser#crange.
    def visitCrange(self, ctx:ExpParser.CrangeContext):
        return QXCRange(self.visitArithExpr(ctx.arithExpr(0)), self.visitArithExpr(ctx.arithExpr(1)))


    # Visit a parse tree produced by ExpParser#index.
    def visitIndex(self, ctx:ExpParser.IndexContext):
        return self.visitArithExpr(ctx.arithExpr())


    # Visit a parse tree produced by ExpParser#qindex.
    def visitQindex(self, ctx:ExpParser.QindexContext):
        return QXQIndex((ctx.ID()), self.visitIndex(ctx.index()))


    # Visit a parse tree produced by ExpParser#rangeT.
    def visitRangeT(self, ctx:ExpParser.RangeTContext):
        if len(ctx.ID()) > 1:
           tmp = []
           for i in range(len(ctx.ID())):
                tmp.append(QXQRange(str(ctx.ID()[i]), self.visitCrange(ctx.crange()[i])))
           return tmp
        return QXQRange(str(ctx.ID()[0]), self.visitCrange(ctx.crange()[0]))


    # Visit a parse tree produced by ExpParser#qrange.
    def visitQrange(self, ctx:ExpParser.QrangeContext):
        if ctx.qindex() is not None:
            v = self.visitQindex(ctx.qindex())
            return QXQRange(v.ID(), QXCRange(v.index(), QXBin("+",v.index(),QXNum(1))))
        else:
            return self.visitRangeT(ctx.rangeT())


    def visitCifexp(self, ctx:ExpParser.CifexpContext):
        b = self.visitLogicOrExp(ctx.logicOrExp())
        l = self.visitArithExpr(ctx.arithExpr(0))
        r = self.visitArithExpr(ctx.arithExpr(1))
        return QXIfExp(b,l,r)


    # Visit a parse tree produced by ExpParser#numexp.
    def visitNumexp(self, ctx:ExpParser.NumexpContext):
        return QXNum(int(ctx.getText()))


    # Visit a parse tree produced by ExpParser#typeT.
    def visitTypeT(self, ctx:ExpParser.TypeTContext):
        if not ctx:
            return None
        if ctx.typeT() is not None:
            return TyFun(self.visitBaseTy(ctx.baseTy()), self.visitTypeT(ctx.typeT()))
        return self.visitBaseTy(ctx.baseTy())

    # Visit a parse tree produced by ExpParser#baseTy.
    def visitBaseTy(self, ctx:ExpParser.BaseTyContext):
        if ctx.TNat() is not None:
            return TySingle("nat")
        if ctx.TReal() is not None:
            return TySingle("real")
        if ctx.TBool() is not None:
            return TySingle("bool")
        if ctx.TInt() is not None:
            return TySingle("int")
        if ctx.baseTy() is not None:
            ty = ctx.baseTy().accept(self)
            v = ctx.arithExpr().accept(self)
            return TyArray(ty, v)
        if ctx.arithExpr() is not None:
            return TyQ(self.visitArithExpr(ctx.arithExpr()))


    # Visit a parse tree produced by ExpParser#qty.
    def visitQty(self, ctx:ExpParser.QtyContext):
        if ctx.Nor() is not None:
            return TyNor()
        if ctx.Had() is not None:
            return TyHad()
        if ctx.En() is not None:
            if ctx.arithExpr() is not None:
                return TyEn(self.visitArithExpr(ctx.arithExpr()))
            return TyEn(QXNum(1))
        if ctx.AA() is not None:
            return TyAA()


    # Visit a parse tree produced by ExpParser#addOp.
    def visitAddOp(self, ctx:ExpParser.AddOpContext):
        return ctx.getText()


    # Visit a parse tree produced by ExpParser#op.
    def visitOp(self, ctx:ExpParser.OpContext):
        if ctx.addOp() is not None:
            return self.visitAddOp(ctx.addOp())
        return ctx.getText()