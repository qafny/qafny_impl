# Generated from Exp.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .ExpParser import ExpParser
else:
    from ExpParser import ExpParser

# This class defines a complete listener for a parse tree produced by ExpParser.
class ExpListener(ParseTreeListener):

    # Enter a parse tree produced by ExpParser#program.
    def enterProgram(self, ctx:ExpParser.ProgramContext):
        pass

    # Exit a parse tree produced by ExpParser#program.
    def exitProgram(self, ctx:ExpParser.ProgramContext):
        pass


    # Enter a parse tree produced by ExpParser#method.
    def enterMethod(self, ctx:ExpParser.MethodContext):
        pass

    # Exit a parse tree produced by ExpParser#method.
    def exitMethod(self, ctx:ExpParser.MethodContext):
        pass


    # Enter a parse tree produced by ExpParser#returna.
    def enterReturna(self, ctx:ExpParser.ReturnaContext):
        pass

    # Exit a parse tree produced by ExpParser#returna.
    def exitReturna(self, ctx:ExpParser.ReturnaContext):
        pass


    # Enter a parse tree produced by ExpParser#conds.
    def enterConds(self, ctx:ExpParser.CondsContext):
        pass

    # Exit a parse tree produced by ExpParser#conds.
    def exitConds(self, ctx:ExpParser.CondsContext):
        pass


    # Enter a parse tree produced by ExpParser#invariants.
    def enterInvariants(self, ctx:ExpParser.InvariantsContext):
        pass

    # Exit a parse tree produced by ExpParser#invariants.
    def exitInvariants(self, ctx:ExpParser.InvariantsContext):
        pass


    # Enter a parse tree produced by ExpParser#stmts.
    def enterStmts(self, ctx:ExpParser.StmtsContext):
        pass

    # Exit a parse tree produced by ExpParser#stmts.
    def exitStmts(self, ctx:ExpParser.StmtsContext):
        pass


    # Enter a parse tree produced by ExpParser#stmt.
    def enterStmt(self, ctx:ExpParser.StmtContext):
        pass

    # Exit a parse tree produced by ExpParser#stmt.
    def exitStmt(self, ctx:ExpParser.StmtContext):
        pass


    # Enter a parse tree produced by ExpParser#spec.
    def enterSpec(self, ctx:ExpParser.SpecContext):
        pass

    # Exit a parse tree produced by ExpParser#spec.
    def exitSpec(self, ctx:ExpParser.SpecContext):
        pass


    # Enter a parse tree produced by ExpParser#allspec.
    def enterAllspec(self, ctx:ExpParser.AllspecContext):
        pass

    # Exit a parse tree produced by ExpParser#allspec.
    def exitAllspec(self, ctx:ExpParser.AllspecContext):
        pass


    # Enter a parse tree produced by ExpParser#bexp.
    def enterBexp(self, ctx:ExpParser.BexpContext):
        pass

    # Exit a parse tree produced by ExpParser#bexp.
    def exitBexp(self, ctx:ExpParser.BexpContext):
        pass


    # Enter a parse tree produced by ExpParser#qbool.
    def enterQbool(self, ctx:ExpParser.QboolContext):
        pass

    # Exit a parse tree produced by ExpParser#qbool.
    def exitQbool(self, ctx:ExpParser.QboolContext):
        pass


    # Enter a parse tree produced by ExpParser#logicImply.
    def enterLogicImply(self, ctx:ExpParser.LogicImplyContext):
        pass

    # Exit a parse tree produced by ExpParser#logicImply.
    def exitLogicImply(self, ctx:ExpParser.LogicImplyContext):
        pass


    # Enter a parse tree produced by ExpParser#logicOrExp.
    def enterLogicOrExp(self, ctx:ExpParser.LogicOrExpContext):
        pass

    # Exit a parse tree produced by ExpParser#logicOrExp.
    def exitLogicOrExp(self, ctx:ExpParser.LogicOrExpContext):
        pass


    # Enter a parse tree produced by ExpParser#logicAndExp.
    def enterLogicAndExp(self, ctx:ExpParser.LogicAndExpContext):
        pass

    # Exit a parse tree produced by ExpParser#logicAndExp.
    def exitLogicAndExp(self, ctx:ExpParser.LogicAndExpContext):
        pass


    # Enter a parse tree produced by ExpParser#logicNotExp.
    def enterLogicNotExp(self, ctx:ExpParser.LogicNotExpContext):
        pass

    # Exit a parse tree produced by ExpParser#logicNotExp.
    def exitLogicNotExp(self, ctx:ExpParser.LogicNotExpContext):
        pass


    # Enter a parse tree produced by ExpParser#chainBExp.
    def enterChainBExp(self, ctx:ExpParser.ChainBExpContext):
        pass

    # Exit a parse tree produced by ExpParser#chainBExp.
    def exitChainBExp(self, ctx:ExpParser.ChainBExpContext):
        pass


    # Enter a parse tree produced by ExpParser#comOp.
    def enterComOp(self, ctx:ExpParser.ComOpContext):
        pass

    # Exit a parse tree produced by ExpParser#comOp.
    def exitComOp(self, ctx:ExpParser.ComOpContext):
        pass


    # Enter a parse tree produced by ExpParser#qunspec.
    def enterQunspec(self, ctx:ExpParser.QunspecContext):
        pass

    # Exit a parse tree produced by ExpParser#qunspec.
    def exitQunspec(self, ctx:ExpParser.QunspecContext):
        pass


    # Enter a parse tree produced by ExpParser#qspec.
    def enterQspec(self, ctx:ExpParser.QspecContext):
        pass

    # Exit a parse tree produced by ExpParser#qspec.
    def exitQspec(self, ctx:ExpParser.QspecContext):
        pass


    # Enter a parse tree produced by ExpParser#partspec.
    def enterPartspec(self, ctx:ExpParser.PartspecContext):
        pass

    # Exit a parse tree produced by ExpParser#partspec.
    def exitPartspec(self, ctx:ExpParser.PartspecContext):
        pass


    # Enter a parse tree produced by ExpParser#tensorall.
    def enterTensorall(self, ctx:ExpParser.TensorallContext):
        pass

    # Exit a parse tree produced by ExpParser#tensorall.
    def exitTensorall(self, ctx:ExpParser.TensorallContext):
        pass


    # Enter a parse tree produced by ExpParser#sumspec.
    def enterSumspec(self, ctx:ExpParser.SumspecContext):
        pass

    # Exit a parse tree produced by ExpParser#sumspec.
    def exitSumspec(self, ctx:ExpParser.SumspecContext):
        pass


    # Enter a parse tree produced by ExpParser#maySum.
    def enterMaySum(self, ctx:ExpParser.MaySumContext):
        pass

    # Exit a parse tree produced by ExpParser#maySum.
    def exitMaySum(self, ctx:ExpParser.MaySumContext):
        pass


    # Enter a parse tree produced by ExpParser#asserting.
    def enterAsserting(self, ctx:ExpParser.AssertingContext):
        pass

    # Exit a parse tree produced by ExpParser#asserting.
    def exitAsserting(self, ctx:ExpParser.AssertingContext):
        pass


    # Enter a parse tree produced by ExpParser#casting.
    def enterCasting(self, ctx:ExpParser.CastingContext):
        pass

    # Exit a parse tree produced by ExpParser#casting.
    def exitCasting(self, ctx:ExpParser.CastingContext):
        pass


    # Enter a parse tree produced by ExpParser#varcreate.
    def enterVarcreate(self, ctx:ExpParser.VarcreateContext):
        pass

    # Exit a parse tree produced by ExpParser#varcreate.
    def exitVarcreate(self, ctx:ExpParser.VarcreateContext):
        pass


    # Enter a parse tree produced by ExpParser#assigning.
    def enterAssigning(self, ctx:ExpParser.AssigningContext):
        pass

    # Exit a parse tree produced by ExpParser#assigning.
    def exitAssigning(self, ctx:ExpParser.AssigningContext):
        pass


    # Enter a parse tree produced by ExpParser#ids.
    def enterIds(self, ctx:ExpParser.IdsContext):
        pass

    # Exit a parse tree produced by ExpParser#ids.
    def exitIds(self, ctx:ExpParser.IdsContext):
        pass


    # Enter a parse tree produced by ExpParser#qassign.
    def enterQassign(self, ctx:ExpParser.QassignContext):
        pass

    # Exit a parse tree produced by ExpParser#qassign.
    def exitQassign(self, ctx:ExpParser.QassignContext):
        pass


    # Enter a parse tree produced by ExpParser#measure.
    def enterMeasure(self, ctx:ExpParser.MeasureContext):
        pass

    # Exit a parse tree produced by ExpParser#measure.
    def exitMeasure(self, ctx:ExpParser.MeasureContext):
        pass


    # Enter a parse tree produced by ExpParser#ifexp.
    def enterIfexp(self, ctx:ExpParser.IfexpContext):
        pass

    # Exit a parse tree produced by ExpParser#ifexp.
    def exitIfexp(self, ctx:ExpParser.IfexpContext):
        pass


    # Enter a parse tree produced by ExpParser#forexp.
    def enterForexp(self, ctx:ExpParser.ForexpContext):
        pass

    # Exit a parse tree produced by ExpParser#forexp.
    def exitForexp(self, ctx:ExpParser.ForexpContext):
        pass


    # Enter a parse tree produced by ExpParser#fcall.
    def enterFcall(self, ctx:ExpParser.FcallContext):
        pass

    # Exit a parse tree produced by ExpParser#fcall.
    def exitFcall(self, ctx:ExpParser.FcallContext):
        pass


    # Enter a parse tree produced by ExpParser#arithExprs.
    def enterArithExprs(self, ctx:ExpParser.ArithExprsContext):
        pass

    # Exit a parse tree produced by ExpParser#arithExprs.
    def exitArithExprs(self, ctx:ExpParser.ArithExprsContext):
        pass


    # Enter a parse tree produced by ExpParser#arithExpr.
    def enterArithExpr(self, ctx:ExpParser.ArithExprContext):
        pass

    # Exit a parse tree produced by ExpParser#arithExpr.
    def exitArithExpr(self, ctx:ExpParser.ArithExprContext):
        pass


    # Enter a parse tree produced by ExpParser#arithAtomic.
    def enterArithAtomic(self, ctx:ExpParser.ArithAtomicContext):
        pass

    # Exit a parse tree produced by ExpParser#arithAtomic.
    def exitArithAtomic(self, ctx:ExpParser.ArithAtomicContext):
        pass


    # Enter a parse tree produced by ExpParser#sinExpr.
    def enterSinExpr(self, ctx:ExpParser.SinExprContext):
        pass

    # Exit a parse tree produced by ExpParser#sinExpr.
    def exitSinExpr(self, ctx:ExpParser.SinExprContext):
        pass


    # Enter a parse tree produced by ExpParser#cosExpr.
    def enterCosExpr(self, ctx:ExpParser.CosExprContext):
        pass

    # Exit a parse tree produced by ExpParser#cosExpr.
    def exitCosExpr(self, ctx:ExpParser.CosExprContext):
        pass


    # Enter a parse tree produced by ExpParser#sqrtExpr.
    def enterSqrtExpr(self, ctx:ExpParser.SqrtExprContext):
        pass

    # Exit a parse tree produced by ExpParser#sqrtExpr.
    def exitSqrtExpr(self, ctx:ExpParser.SqrtExprContext):
        pass


    # Enter a parse tree produced by ExpParser#absExpr.
    def enterAbsExpr(self, ctx:ExpParser.AbsExprContext):
        pass

    # Exit a parse tree produced by ExpParser#absExpr.
    def exitAbsExpr(self, ctx:ExpParser.AbsExprContext):
        pass


    # Enter a parse tree produced by ExpParser#omegaExpr.
    def enterOmegaExpr(self, ctx:ExpParser.OmegaExprContext):
        pass

    # Exit a parse tree produced by ExpParser#omegaExpr.
    def exitOmegaExpr(self, ctx:ExpParser.OmegaExprContext):
        pass


    # Enter a parse tree produced by ExpParser#expr.
    def enterExpr(self, ctx:ExpParser.ExprContext):
        pass

    # Exit a parse tree produced by ExpParser#expr.
    def exitExpr(self, ctx:ExpParser.ExprContext):
        pass


    # Enter a parse tree produced by ExpParser#lambdaT.
    def enterLambdaT(self, ctx:ExpParser.LambdaTContext):
        pass

    # Exit a parse tree produced by ExpParser#lambdaT.
    def exitLambdaT(self, ctx:ExpParser.LambdaTContext):
        pass


    # Enter a parse tree produced by ExpParser#manyket.
    def enterManyket(self, ctx:ExpParser.ManyketContext):
        pass

    # Exit a parse tree produced by ExpParser#manyket.
    def exitManyket(self, ctx:ExpParser.ManyketContext):
        pass


    # Enter a parse tree produced by ExpParser#ket.
    def enterKet(self, ctx:ExpParser.KetContext):
        pass

    # Exit a parse tree produced by ExpParser#ket.
    def exitKet(self, ctx:ExpParser.KetContext):
        pass


    # Enter a parse tree produced by ExpParser#qstate.
    def enterQstate(self, ctx:ExpParser.QstateContext):
        pass

    # Exit a parse tree produced by ExpParser#qstate.
    def exitQstate(self, ctx:ExpParser.QstateContext):
        pass


    # Enter a parse tree produced by ExpParser#bindings.
    def enterBindings(self, ctx:ExpParser.BindingsContext):
        pass

    # Exit a parse tree produced by ExpParser#bindings.
    def exitBindings(self, ctx:ExpParser.BindingsContext):
        pass


    # Enter a parse tree produced by ExpParser#binding.
    def enterBinding(self, ctx:ExpParser.BindingContext):
        pass

    # Exit a parse tree produced by ExpParser#binding.
    def exitBinding(self, ctx:ExpParser.BindingContext):
        pass


    # Enter a parse tree produced by ExpParser#locus.
    def enterLocus(self, ctx:ExpParser.LocusContext):
        pass

    # Exit a parse tree produced by ExpParser#locus.
    def exitLocus(self, ctx:ExpParser.LocusContext):
        pass


    # Enter a parse tree produced by ExpParser#crange.
    def enterCrange(self, ctx:ExpParser.CrangeContext):
        pass

    # Exit a parse tree produced by ExpParser#crange.
    def exitCrange(self, ctx:ExpParser.CrangeContext):
        pass


    # Enter a parse tree produced by ExpParser#index.
    def enterIndex(self, ctx:ExpParser.IndexContext):
        pass

    # Exit a parse tree produced by ExpParser#index.
    def exitIndex(self, ctx:ExpParser.IndexContext):
        pass


    # Enter a parse tree produced by ExpParser#qindex.
    def enterQindex(self, ctx:ExpParser.QindexContext):
        pass

    # Exit a parse tree produced by ExpParser#qindex.
    def exitQindex(self, ctx:ExpParser.QindexContext):
        pass


    # Enter a parse tree produced by ExpParser#rangeT.
    def enterRangeT(self, ctx:ExpParser.RangeTContext):
        pass

    # Exit a parse tree produced by ExpParser#rangeT.
    def exitRangeT(self, ctx:ExpParser.RangeTContext):
        pass


    # Enter a parse tree produced by ExpParser#qrange.
    def enterQrange(self, ctx:ExpParser.QrangeContext):
        pass

    # Exit a parse tree produced by ExpParser#qrange.
    def exitQrange(self, ctx:ExpParser.QrangeContext):
        pass


    # Enter a parse tree produced by ExpParser#numexp.
    def enterNumexp(self, ctx:ExpParser.NumexpContext):
        pass

    # Exit a parse tree produced by ExpParser#numexp.
    def exitNumexp(self, ctx:ExpParser.NumexpContext):
        pass


    # Enter a parse tree produced by ExpParser#typeT.
    def enterTypeT(self, ctx:ExpParser.TypeTContext):
        pass

    # Exit a parse tree produced by ExpParser#typeT.
    def exitTypeT(self, ctx:ExpParser.TypeTContext):
        pass


    # Enter a parse tree produced by ExpParser#baseTy.
    def enterBaseTy(self, ctx:ExpParser.BaseTyContext):
        pass

    # Exit a parse tree produced by ExpParser#baseTy.
    def exitBaseTy(self, ctx:ExpParser.BaseTyContext):
        pass


    # Enter a parse tree produced by ExpParser#qty.
    def enterQty(self, ctx:ExpParser.QtyContext):
        pass

    # Exit a parse tree produced by ExpParser#qty.
    def exitQty(self, ctx:ExpParser.QtyContext):
        pass


    # Enter a parse tree produced by ExpParser#addOp.
    def enterAddOp(self, ctx:ExpParser.AddOpContext):
        pass

    # Exit a parse tree produced by ExpParser#addOp.
    def exitAddOp(self, ctx:ExpParser.AddOpContext):
        pass


    # Enter a parse tree produced by ExpParser#op.
    def enterOp(self, ctx:ExpParser.OpContext):
        pass

    # Exit a parse tree produced by ExpParser#op.
    def exitOp(self, ctx:ExpParser.OpContext):
        pass


    # Enter a parse tree produced by ExpParser#reen.
    def enterReen(self, ctx:ExpParser.ReenContext):
        pass

    # Exit a parse tree produced by ExpParser#reen.
    def exitReen(self, ctx:ExpParser.ReenContext):
        pass



del ExpParser