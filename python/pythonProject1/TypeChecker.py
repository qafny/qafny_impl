import Programmer
from LocusCollector import LocusCollector
from ProgramVisitor import ProgramVisitor
from CollectKind import *

def compareQRange(q1: QXQRange, q2: QXQRange):
    return (q1.ID() == q2.ID()
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

def compareType(ty: QXQTy, ty1: QXQTy = None):
    if ty1 is None:
        return ty

    if isinstance(ty, TyEn) and isinstance(ty1, TyEn):
        if ty.flag().num() < ty1.flag().num():
            return ty1
        else:
            return ty

    if isinstance(ty, TyEn) and isinstance(ty1, TyNor):
        return ty

    if isinstance(ty, TyNor) and isinstance(ty1, TyEn):
        return ty1

    if isinstance(ty, TyHad) and isinstance(ty1, TyEn):
        return ty1

    if isinstance(ty, TyEn) and isinstance(ty1, TyHad):
        return ty

    if isinstance(ty, TyNor) and isinstance(ty1, TyHad):
        return TyEn(QXNum(1))

    if isinstance(ty, TyHad) and isinstance(ty1, TyNor):
        return TyEn(QXNum(1))

    if isinstance(ty, TyHad) and isinstance(ty1, TyHad):
        return TyEn(QXNum(1))

    if isinstance(ty, TyNor) and isinstance(ty1, TyNor):
        return ty


def compareSingle(qs: [QXQRange], qv: [QXQRange]):
    if len(qv) != 1:
        return None

    elem = qv[0]
    vs = []
    for i in len(qs):
        v = qs[i]
        if elem.ID() == v.ID():
            if compareAExp(elem.crange().left(), v.crange().left()):
                if compareAExp(elem.crange().right(), v.crange().right()):
                    qv = []
                    vs += (qs[i+1:len(qs)])
                    return (QXQRange(elem.ID(), QXCRange(v.crange().left(), v.crange().right())), vs, qv)
                else:
                    qv = [QXQRange(elem.ID(), QXCRange(v.crange().right(), elem.crange().right()))]
                    vs += (qs[i+1:len(qs)])
                    return (QXQRange(elem.ID(), QXCRange(v.crange().left(), v.crange().right())), vs, qv)
        vs += [v]

    return None


def subLocusGen(q: [QXQRange], qs: [([QXQRange], TyQ)]):
    rev = []
    floc = []
    type = None
    for i in len(qs):
        elem,qty = qs[i]
        if isinstance(qty, TyEn):
            vs = compareLocus(elem, q)
            if vs is None:
                rev += [(elem, qty)]
            elif vs == []:
                floc += elem
                type = compareType(qty, type)
                rev += (qs[i+1:len(qs)])
                return (floc, type, rev)
            else:
                q = vs
                floc += elem
                type = compareType(qty, type)

        if isinstance(qty, TyNor) or isinstance(qty, TyHad):
            re = compareSingle(q, elem)
            if re is not None:
                qxv, vs, qv = re
                if vs == []:
                    floc += [qxv]
                    type = compareType(qty, type)
                    rev += qv + (qs[i+1:len(qs)])
                    return (floc, type, rev)
                else:
                    floc += [qxv]
                    type = compareType(qty, type)
                    rev += qv
            else:
                rev += [qs[i]]
    return None

def sameLocus(q: [QXQRange], qs : [([QXQRange], TyQ)]):
    for i in len(qs):
        elem, qty = qs[i]
        vs = compareLocus(q, elem)
        if vs == []:
            vs += (qs[i+1:len(qs)])
            return vs
        elif vs is None:
            vs += [qs[i]]
        else:
            return None

def subRangeLocus(elem: QXQRange, qs: [QXQRange]):
    vs = []
    for i in range(len(qs)):
        v = qs[i]
        if elem.ID() == v.ID():
            if compareAExp(elem.crange().left(), v.crange().left()):
                if compareAExp(elem.crange().right(), v.crange().right()):
                    vs += (qs[i + 1:len(qs)])
                    return vs
                else:
                    vs += [QXQRange(elem.ID(), QXCRange(v.crange().right(), elem.crange().right()))] + (qs[i + 1:len(qs)])
                    return vs
        vs = vs + [qs[i]]
    return None

def subRangeLoci(q: [QXQRange], qs: [QXQRange]):
    for elem in q:
        qs = subRangeLocus(elem, qs)
        if qs is None:
            return None
    return qs

def subLocus(q: [QXQRange] , qs: [([QXQRange], TyQ)]):
    qsf = []
    rty = None
    for i in range(len(qs)):
        elem,ty = qs[i]
        vs = subRangeLoci(q, elem)
        if vs is None:
            qsf = qsf + [qs[i]]
        elif vs == []:
            qsf = qsf + qs[i+1:len(qs)]
            if isinstance(ty, TyHad):
                rty = TyEn(QXNum(1))
            else:
                rty = ty
            return (rty, qsf)
        else:
            qsf += [(vs,ty)] + qs[i+1:len(qs)]
            if isinstance(ty, TyHad):
                rty = TyEn(QXNum(1))
            else:
                rty = ty
            return (rty, qsf)

    return None

def addOneType(ty : QXQTy):
    if isinstance(ty, TyEn):
        return TyEn(QXNum(ty.flag().num()+1))
    if isinstance(ty, TyNor):
        return TyHad
    return None

# check the types of the quantum array (nor, Had, EN types)
class TypeChecker(ProgramVisitor):

    def __init__(self, kenv: dict, tenv:dict, f: str, ind: int):
        # need st --> state we are deling with
        #kind map from fun vars to kind maps
        self.kenv = kenv
        #type env
        self.tenv = tenv
        #current fun name
        self.name = f
        #the index for a function name to check
        self.ind = ind
        #kind env
        self.kinds = dict()
        #the checked type env at index
        self.renv = []

    def visitMethod(self, ctx: Programmer.QXMethod):
        x = ctx.ID()
        if x != self.name:
            return True

        self.kinds = self.kenv.get(self.name)[0]
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
        kv = ctx.binding().type()
        self.kinds.update({y: kv})
        return True

    def visitCast(self, ctx: Programmer.QXCast):
        ty = ctx.qty()
        if isinstance(ty, TyAA):
            vs = sameLocus(ctx.locus(), self.renv)
            if vs is None:
                return False
            else:
                self.renv = [(ctx.locus(), TyAA())] + vs
                return True

        re = subLocusGen(ctx.locus(), self.renv)
        if re is None:
            return False
        newLoc, newTy, vs = re
        self.renv = vs + [(newLoc, ty)]
        return True

    def visitBind(self, ctx: Programmer.QXBind):
        if ctx.type() is not None:
            ctx.type().accept(self)
        return ctx.ID()

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        loc, ty, nenv = subLocusGen(ctx.locus(), self.renv)
        if isinstance(ctx.exp(), QXSingle):
            ty = addOneType(ty)
        if ty is None:
            return False

        self.renv = nenv
        self.renv += [(loc, ty)]
        return True

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        re = subLocus(ctx.locus(), self.renv)
        if re is None:
            return False
        nty, nenv = re

        for id in ctx.ids():
            self.kinds.update({id:TySingle("nat")})

        self.renv = [(ctx.locus(), nty)]+nenv
        return True

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        return True

    def visitIf(self, ctx: Programmer.QXIf):
        if isinstance(ctx.bexp(), QXBool):
            oldenv = self.renv
            for elem in ctx.stmts():
                elem.accept(self)
            if all(x == y for x, y in zip(oldenv, self.renv)):
                return True
            else:
                return False


        if isinstance(ctx.bexp(), QXQBool):
            findLocus = LocusCollector(self.renv)
            tmpv = self.renv
            self.renv = findLocus.renv
            for elem in ctx.stmts():
                elem.accept()
            self.renv = tmpv
            return True


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
