# Generated from Exp.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .ExpParser import ExpParser
else:
    from ExpParser import ExpParser

# This class defines a complete generic visitor for a parse tree produced by ExpParser.

class ExpVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by ExpParser#program.
    def visitProgram(self, ctx:ExpParser.ProgramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#method.
    def visitMethod(self, ctx:ExpParser.MethodContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#returna.
    def visitReturna(self, ctx:ExpParser.ReturnaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#conds.
    def visitConds(self, ctx:ExpParser.CondsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#invariants.
    def visitInvariants(self, ctx:ExpParser.InvariantsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#stmts.
    def visitStmts(self, ctx:ExpParser.StmtsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#stmt.
    def visitStmt(self, ctx:ExpParser.StmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#spec.
    def visitSpec(self, ctx:ExpParser.SpecContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#allspec.
    def visitAllspec(self, ctx:ExpParser.AllspecContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#bexp.
    def visitBexp(self, ctx:ExpParser.BexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qbool.
    def visitQbool(self, ctx:ExpParser.QboolContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#logicImply.
    def visitLogicImply(self, ctx:ExpParser.LogicImplyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#logicOrExp.
    def visitLogicOrExp(self, ctx:ExpParser.LogicOrExpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#logicAndExp.
    def visitLogicAndExp(self, ctx:ExpParser.LogicAndExpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#logicNotExp.
    def visitLogicNotExp(self, ctx:ExpParser.LogicNotExpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#chainBExp.
    def visitChainBExp(self, ctx:ExpParser.ChainBExpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#comOp.
    def visitComOp(self, ctx:ExpParser.ComOpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qunspec.
    def visitQunspec(self, ctx:ExpParser.QunspecContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qspec.
    def visitQspec(self, ctx:ExpParser.QspecContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#partspec.
    def visitPartspec(self, ctx:ExpParser.PartspecContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#tensorall.
    def visitTensorall(self, ctx:ExpParser.TensorallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#sumspec.
    def visitSumspec(self, ctx:ExpParser.SumspecContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#maySum.
    def visitMaySum(self, ctx:ExpParser.MaySumContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#asserting.
    def visitAsserting(self, ctx:ExpParser.AssertingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#casting.
    def visitCasting(self, ctx:ExpParser.CastingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#varcreate.
    def visitVarcreate(self, ctx:ExpParser.VarcreateContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#assigning.
    def visitAssigning(self, ctx:ExpParser.AssigningContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ids.
    def visitIds(self, ctx:ExpParser.IdsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qassign.
    def visitQassign(self, ctx:ExpParser.QassignContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#measure.
    def visitMeasure(self, ctx:ExpParser.MeasureContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ifexp.
    def visitIfexp(self, ctx:ExpParser.IfexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#forexp.
    def visitForexp(self, ctx:ExpParser.ForexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#fcall.
    def visitFcall(self, ctx:ExpParser.FcallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#arithExprs.
    def visitArithExprs(self, ctx:ExpParser.ArithExprsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#arithExpr.
    def visitArithExpr(self, ctx:ExpParser.ArithExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#arithAtomic.
    def visitArithAtomic(self, ctx:ExpParser.ArithAtomicContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#sinExpr.
    def visitSinExpr(self, ctx:ExpParser.SinExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#cosExpr.
    def visitCosExpr(self, ctx:ExpParser.CosExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#sqrtExpr.
    def visitSqrtExpr(self, ctx:ExpParser.SqrtExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#absExpr.
    def visitAbsExpr(self, ctx:ExpParser.AbsExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#omegaExpr.
    def visitOmegaExpr(self, ctx:ExpParser.OmegaExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#expr.
    def visitExpr(self, ctx:ExpParser.ExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#lambdaT.
    def visitLambdaT(self, ctx:ExpParser.LambdaTContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#manyket.
    def visitManyket(self, ctx:ExpParser.ManyketContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ket.
    def visitKet(self, ctx:ExpParser.KetContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qstate.
    def visitQstate(self, ctx:ExpParser.QstateContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#bindings.
    def visitBindings(self, ctx:ExpParser.BindingsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#binding.
    def visitBinding(self, ctx:ExpParser.BindingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#locus.
    def visitLocus(self, ctx:ExpParser.LocusContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#crange.
    def visitCrange(self, ctx:ExpParser.CrangeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#index.
    def visitIndex(self, ctx:ExpParser.IndexContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qindex.
    def visitQindex(self, ctx:ExpParser.QindexContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#rangeT.
    def visitRangeT(self, ctx:ExpParser.RangeTContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qrange.
    def visitQrange(self, ctx:ExpParser.QrangeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#numexp.
    def visitNumexp(self, ctx:ExpParser.NumexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#typeT.
    def visitTypeT(self, ctx:ExpParser.TypeTContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#baseTy.
    def visitBaseTy(self, ctx:ExpParser.BaseTyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qty.
    def visitQty(self, ctx:ExpParser.QtyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#addOp.
    def visitAddOp(self, ctx:ExpParser.AddOpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#op.
    def visitOp(self, ctx:ExpParser.OpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#reen.
    def visitReen(self, ctx:ExpParser.ReenContext):
        return self.visitChildren(ctx)



del ExpParser