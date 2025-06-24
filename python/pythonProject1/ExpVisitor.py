# Generated from Exp.g4 by ANTLR 4.9.2
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .ExpParser import ExpParser
else:
    from ExpParser import ExpParser

# This class defines a complete generic visitor for a parse tree produced by ExpParser.

class ExpVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by ExpParser#program.
    def visitProgram(self, ctx:ExpParser.ProgramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#topLevel.
    def visitTopLevel(self, ctx:ExpParser.TopLevelContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#method.
    def visitMethod(self, ctx:ExpParser.MethodContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#function.
    def visitFunction(self, ctx:ExpParser.FunctionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#lemma.
    def visitLemma(self, ctx:ExpParser.LemmaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#predicate.
    def visitPredicate(self, ctx:ExpParser.PredicateContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#returna.
    def visitReturna(self, ctx:ExpParser.ReturnaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#conds.
    def visitConds(self, ctx:ExpParser.CondsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#reen.
    def visitReen(self, ctx:ExpParser.ReenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#loopConds.
    def visitLoopConds(self, ctx:ExpParser.LoopCondsContext):
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


    # Visit a parse tree produced by ExpParser#bexp.
    def visitBexp(self, ctx:ExpParser.BexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qbool.
    def visitQbool(self, ctx:ExpParser.QboolContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#logicImply.
    def visitLogicImply(self, ctx:ExpParser.LogicImplyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#allspec.
    def visitAllspec(self, ctx:ExpParser.AllspecContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#logicExpr.
    def visitLogicExpr(self, ctx:ExpParser.LogicExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#logicInExpr.
    def visitLogicInExpr(self, ctx:ExpParser.LogicInExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#chainBExp.
    def visitChainBExp(self, ctx:ExpParser.ChainBExpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#comOp.
    def visitComOp(self, ctx:ExpParser.ComOpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qtypeCreate.
    def visitQtypeCreate(self, ctx:ExpParser.QtypeCreateContext):
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


    # Visit a parse tree produced by ExpParser#partpred.
    def visitPartpred(self, ctx:ExpParser.PartpredContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#partsection.
    def visitPartsection(self, ctx:ExpParser.PartsectionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#partsections.
    def visitPartsections(self, ctx:ExpParser.PartsectionsContext):
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


    # Visit a parse tree produced by ExpParser#idindices.
    def visitIdindices(self, ctx:ExpParser.IdindicesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qassign.
    def visitQassign(self, ctx:ExpParser.QassignContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qcreate.
    def visitQcreate(self, ctx:ExpParser.QcreateContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#measure.
    def visitMeasure(self, ctx:ExpParser.MeasureContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#measureAbort.
    def visitMeasureAbort(self, ctx:ExpParser.MeasureAbortContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#returnStmt.
    def visitReturnStmt(self, ctx:ExpParser.ReturnStmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#breakStmt.
    def visitBreakStmt(self, ctx:ExpParser.BreakStmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ifexp.
    def visitIfexp(self, ctx:ExpParser.IfexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#cifexp.
    def visitCifexp(self, ctx:ExpParser.CifexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#manyketpart.
    def visitManyketpart(self, ctx:ExpParser.ManyketpartContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#forexp.
    def visitForexp(self, ctx:ExpParser.ForexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#whileexp.
    def visitWhileexp(self, ctx:ExpParser.WhileexpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#fcall.
    def visitFcall(self, ctx:ExpParser.FcallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#arithExprsOrKets.
    def visitArithExprsOrKets(self, ctx:ExpParser.ArithExprsOrKetsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#arithExprWithSum.
    def visitArithExprWithSum(self, ctx:ExpParser.ArithExprWithSumContext):
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


    # Visit a parse tree produced by ExpParser#notExpr.
    def visitNotExpr(self, ctx:ExpParser.NotExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#absExpr.
    def visitAbsExpr(self, ctx:ExpParser.AbsExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#omegaExpr.
    def visitOmegaExpr(self, ctx:ExpParser.OmegaExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#rotExpr.
    def visitRotExpr(self, ctx:ExpParser.RotExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ketCallExpr.
    def visitKetCallExpr(self, ctx:ExpParser.KetCallExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#setInstance.
    def visitSetInstance(self, ctx:ExpParser.SetInstanceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#memberAccess.
    def visitMemberAccess(self, ctx:ExpParser.MemberAccessContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#expr.
    def visitExpr(self, ctx:ExpParser.ExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#lambdaT.
    def visitLambdaT(self, ctx:ExpParser.LambdaTContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#dis.
    def visitDis(self, ctx:ExpParser.DisContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#manyket.
    def visitManyket(self, ctx:ExpParser.ManyketContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ket.
    def visitKet(self, ctx:ExpParser.KetContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ketsum.
    def visitKetsum(self, ctx:ExpParser.KetsumContext):
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


    # Visit a parse tree produced by ExpParser#typeOptionalBindings.
    def visitTypeOptionalBindings(self, ctx:ExpParser.TypeOptionalBindingsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#typeOptionalBinding.
    def visitTypeOptionalBinding(self, ctx:ExpParser.TypeOptionalBindingContext):
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


    # Visit a parse tree produced by ExpParser#idindex.
    def visitIdindex(self, ctx:ExpParser.IdindexContext):
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


    # Visit a parse tree produced by ExpParser#ArrayType.
    def visitArrayType(self, ctx:ExpParser.ArrayTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#ArrayWithSizeType.
    def visitArrayWithSizeType(self, ctx:ExpParser.ArrayWithSizeTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#BoolType.
    def visitBoolType(self, ctx:ExpParser.BoolTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#DynamicArrayType.
    def visitDynamicArrayType(self, ctx:ExpParser.DynamicArrayTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#SetType.
    def visitSetType(self, ctx:ExpParser.SetTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#QBitStringType.
    def visitQBitStringType(self, ctx:ExpParser.QBitStringTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#NaturalType.
    def visitNaturalType(self, ctx:ExpParser.NaturalTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#BitVectorType.
    def visitBitVectorType(self, ctx:ExpParser.BitVectorTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#RealType.
    def visitRealType(self, ctx:ExpParser.RealTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#IntType.
    def visitIntType(self, ctx:ExpParser.IntTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#qty.
    def visitQty(self, ctx:ExpParser.QtyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#aaType.
    def visitAaType(self, ctx:ExpParser.AaTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#additiveOp.
    def visitAdditiveOp(self, ctx:ExpParser.AdditiveOpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#multiplicativeOp.
    def visitMultiplicativeOp(self, ctx:ExpParser.MultiplicativeOpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#exponentialOp.
    def visitExponentialOp(self, ctx:ExpParser.ExponentialOpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpParser#boolLiteral.
    def visitBoolLiteral(self, ctx:ExpParser.BoolLiteralContext):
        return self.visitChildren(ctx)



del ExpParser