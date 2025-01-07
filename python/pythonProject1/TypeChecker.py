import Programmer
from ProgramVisitor import ProgramVisitor
from CollectKind import *

def compareQRange(q1: QXQRange, q2: QXQRange):
    return (q1.ID().ID() == q2.ID().ID()
            and compareAExp(q1.crange().left(),q2.crange().left())
            and compareAExp(q1.crange().right(),q2.crange().right()))

def compareRangeLocus(q1: QXQRange, qs: [QXQRange]):
    vs = []
    for i in range(len(qs)):
        if compareQRange(q1,qs[i]):
            return (vs + (qs[i+1:len(qs)]))
        vs = vs + [qs[i]]
    return None

def compareLocus(q1: [QXQRange], q2: [QXQRange]):
    vs = q2
    for elem in q1:
        vs = compareRangeLocus(elem, vs)
        if vs is None:
            return None

    return vs

def subLocus(qs: [([QXQRange], TyQ)], q2: [QXQRange]):
    vs = q2
    qsf = []
    for i in range(len(qs)):
        elem,ty = qs[i]
        vsa = vs
        vs = compareLocus(elem, q2)
        if vs is None:
            vs = vsa
            qsf = qsf + [elem]
        if not vs:
            qsf = qsf + qs[i+1:len(qs)]
            break

    if not vs:
        return qsf
    else:
        return None


# check the types of the quantum array (nor, Had, EN types)
class TypeChecker(ProgramVisitor):

    def __init__(self, kenv: dict, tenv:dict, f: str, ind: int):
        # need st --> state we are deling with
        self.kenv = kenv
        self.tenv = tenv
        self.name = f
        self.ind = ind
        self.kinds = dict()
        self.renv = []

    def visitMethod(self, ctx: Programmer.QXMethod):
        x = ctx.ID()
        if x == self.name:
            return True

        self.kinds = self.kenv.get(self.name)
        self.renv = self.tenv.get(self.name)[0]

        v = True
        for i in range(self.ind):
            v = v and ctx.stmts()[i].accept(self)
        return v

    def visitProgram(self, ctx: Programmer.QXProgram):
        for elem in ctx.method():
            v = elem.accept(self)
            if not v:
                return False
        return True


    def visitAssert(self, ctx: Programmer.QXAssert):
        return ctx.spec().accept(self)

    def visitInit(self, ctx: Programmer.QXInit):
        y = ctx.binding().ID()
        tv = ctx.binding().type()
        self.tenv.update({y: tv})
        return True

    def visitCast(self, ctx: Programmer.QXCast):
        ty = ctx.qty()
        self.renv = subLocus(self.renv,ctx.locus())
        if self.renv is None:
            return False
        self.renv = self.renv + [(ctx.locus(), ty)]
        return True

    def visitBind(self, ctx: Programmer.QXBind):
        if ctx.type() is not None:
            ctx.type().accept(self)
        return ctx.ID()

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        return True

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        for elem in ctx.locus():
            elem.accept(self)
        return ctx.ids()

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        return True

    def visitIf(self, ctx: Programmer.QXIf):
        ctx.bexp().accept(self)
        for elem in ctx.stmts():
            elem.accept(self)

    def visitFor(self, ctx: Programmer.QXFor):
        ctx.crange().accept(self)

        for ielem in ctx.inv():
            ielem.accept(self)

        for elem in ctx.stmts():
            elem.accept(self)
        return ctx.ID()

    def visitCall(self, ctx: Programmer.QXCall):
        for elem in ctx.exps():
            elem.accept(self)
        return ctx.ID()

    def visitCNot(self, ctx: Programmer.QXCNot):
        return ctx.next().accept(self)

    def visitEn(self, ctx: Programmer.TyEn):
        return ctx.flag().accept(self)

    def visitQSpec(self, ctx: Programmer.QXQSpec):
        ctx.qty().accept(self)
        for elem in ctx.locus():
            elem.accept(self)
        return ctx.state().accept(self)

    def visitTensor(self, ctx: Programmer.QXTensor):
        for elem in ctx.kets():
            elem.accept(self)

    def visitKet(self, ctx: Programmer.QXKet):
        return ctx.vector().accept(self)

    def visitSum(self, ctx: Programmer.QXSum):
        for elem in ctx.kets():
            elem.accept(self)
        ctx.amp().accept(self)
        for elem in ctx.sums():
            elem.accept(self)

    def visitLogic(self, ctx: Programmer.QXLogic):
        ctx.left().accept(self)
        ctx.right().accept(self)

    def visitBool(self, ctx: Programmer.QXComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitCon(self, ctx: Programmer.QXCon):
        ctx.range().accept(self)
        return ctx.ID()

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        ctx.index().accept(self)
        return ctx.ID()

    def visitQNot(self, ctx: Programmer.QXQNot):
        return ctx.next().accept(self)

    def visitQComp(self, ctx: Programmer.QXQComp):
        ctx.left().accept(self)
        ctx.right().accept(self)
        ctx.index().accept(self)

    def visitAll(self, ctx: Programmer.QXAll):
        ctx.bind().accept(self)
        ctx.next().accept(self)

    def visitBin(self, ctx: Programmer.QXBin):
        ctx.left().accept(self)
        ctx.right().accept(self)
        return ctx.op()

    def visitUni(self, ctx: Programmer.QXUni):
        ctx.next().accept(self)
        return ctx.op()

    def visitSingle(self, ctx: Programmer.QXSingle):
        return ctx.op()

    def visitOracle(self, ctx: Programmer.QXOracle):
        ctx.phase().accept(self)
        for elem in ctx.vectors():
            elem.accept(self)
