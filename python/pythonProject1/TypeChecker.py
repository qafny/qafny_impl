import LocusCollector
import Programmer
from ProgramVisitor import ProgramVisitor
from CollectKind import *
from SimpAExp import SimpAExp
from SubstAExp import SubstAExp


def compareQRange(q1: QXQRange, q2: QXQRange):
    return (q1.ID() == q2.ID()
            and compareAExp(q1.cranges()[0].left(),q2.cranges()[0].left())
            and compareAExp(q1.cranges()[0].right(),q2.cranges()[0].right()))

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

def compareSubLocus(q1: [QXQRange], q2: [([QXQRange], QXQTy, int)]):
    vs = []
    for i in q1:
        for loc, qty, num in q2:
            tmpres = compareLocus([i], loc)
            if tmpres == [] or tmpres:
                vs.append((loc, qty, num))
                break
    return vs


def equalLocusEnv(q1: [QXQRange], qs: [([QXQRange], QXQTy, int)]):
    vs = None
    for elem, ty, num in qs:
        vs = compareLocus(q1, elem)
        if vs is not None:
            return vs
    return vs

def equalEnv(qs1: [([QXQRange], QXQTy,int)], qs2: [([QXQRange], QXQTy, int)]):
    vs = None
    for elem, ty, num in qs1:
        vs = equalLocusEnv(elem, qs2)
        if vs is not None:
            return vs
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
    for i in range(len(qs)):
        v = qs[i]
        if elem.ID() == v.ID():
            if compareAExp(elem.cranges()[0].left(), v.cranges()[0].left()):
                if compareAExp(elem.cranges()[0].right(), v.cranges()[0].right()):
                    qv = []
                    vs += (qs[i+1:len(qs)])
                    return (QXQRange(elem.ID(), [QXCRange(v.cranges()[0].left(), v.cranges()[0].right())]), vs, qv)
                else:
                    qv = [QXQRange(elem.ID(), [QXCRange(v.cranges()[0].right(), elem.cranges()[0].right())])]
                    vs += (qs[i+1:len(qs)])
                    return (QXQRange(elem.ID(), [QXCRange(v.cranges()[0].left(), v.cranges()[0].right())]), vs, qv)
        vs += [v]

    return None


def subLocusGen(q: [QXQRange], qs: [([QXQRange], QXQTy, int)]):
    rev = []
    floc = []
    type = None
    for i in range(len(qs)):
        elem, qty, num = qs[i]
        if isinstance(qty, TyEn):
            vs = compareLocus(elem, q)
            if vs is None:
                rev += [(elem, qty, num)]
            elif vs == []:
                floc += elem
                type = compareType(qty, type)
                rev += (qs[i+1:len(qs)])
                return (floc, type, rev, num)
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
                    return (floc, type, rev, num)
                else:
                    floc += [qxv]
                    type = compareType(qty, type)
                    rev += qv
            else:
                rev += [qs[i]]

    

    sl = compareSubLocus(q, qs)
    
    if len(sl)>0:
        floc = []
        for loc,ty,num in sl:
            type = compareType(ty, type)
            floc.extend(loc)

        
        slrev = []
        for loc,qty,num in qs:
            if loc not in floc:
                slrev.append((loc,qty,num))
        
        return (floc, type, slrev, -1)
    
    if floc is not None:
        return floc, type, rev, -1
    
    return None

def sameLocus(q: [QXQRange], qs : [([QXQRange], QXQTy, int)]):
    for i in range(len(qs)):
        elem, qty, num = qs[i]
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
            if compareAExp(elem.cranges()[0].left(), v.cranges()[0].left()):
                if compareAExp(elem.cranges()[0].right(), v.cranges()[0].right()):
                    vs += (qs[i + 1:len(qs)])
                    return vs
                else:
                    vs += [QXQRange(elem.ID(), [QXCRange(v.cranges()[0].right(), elem.cranges()[0].right())])] + (qs[i + 1:len(qs)])
                    return vs
        vs = vs + [qs[i]]
    return None

def subRangeLoci(q: [QXQRange], qs: [QXQRange]):
    for elem in q:
        qs = subRangeLocus(elem, qs)
        if qs is None:
            return None
    return qs

def subLocus(q: [QXQRange] , qs: [([QXQRange], QXQTy, int)]):
    qsf = []
    rty = None
    for i in range(len(qs)):
        elem, ty, num = qs[i]
        vs = subRangeLoci(q, elem)
        if vs is None:
            qsf = qsf + [qs[i]]
        elif vs == []:
            qsf = qsf + qs[i+1:len(qs)]
            if isinstance(ty, TyHad):
                rty = TyEn(QXNum(1))
            else:
                rty = ty
            return (rty, num, qsf)
        else:
            qsf += [(vs,ty, num)] + qs[i+1:len(qs)]
            if isinstance(ty, TyHad):
                rty = TyEn(QXNum(1))
            else:
                rty = ty
            return (rty, num, qsf)

    return None

def addOneType(ty : QXQTy):
    if isinstance(ty, TyEn):
        return TyEn(QXNum(ty.flag().num()+1))
    if isinstance(ty, TyNor):
        return TyHad()
    return None

def replaceLocus(t: [QXQRange], r1: QXQRange, r2: QXQRange):
    tmp = []
    for elem in t:
        if compareQRange(elem, r1):
            tmp += [r2]
        else:
            tmp += [elem]
    return tmp

def replaceLoci(t: [QXQRange], r1: [QXQRange], r2: [QXQRange]):
    vs = zip(r1, r2)
    for v1,v2 in vs:
        t = replaceLocus(t, v1, v2)
    return t

def replaceEnvLoci(tenv: [([QXQRange],QXQTy, int)], l1: [QXQRange], l2: [QXQRange]):
    tmp = []
    for elem,ty,num in tenv:
        vs = compareLocus(elem, l1)
        if vs is None:
            tmp += [(elem,ty, num)]
        else:
            tmp += [replaceLoci(elem, l1, l2)]
    return tmp

def substQVar(l:[(str,QXAExp)], v:str):

    for name, elem in l:
        if name == v and isinstance(elem, QXBind):
            return elem.ID()
    return v

def substAllVars(l:[SubstAExp], v:QXAExp):

    for st in l:
        v = st.visit(v)

    return v

# check the types of the quantum array (nor, Had, EN types)
# for a given index in a function name f, we check the type information at location index in the function
# the type env, is the initial type env getting from TypeCollector, no ending type env is needed.
class TypeChecker(ProgramVisitor):

    def __init__(self, kenv: dict, tenv: dict, renv:[([QXQRange], QXQTy, int)], counter : int):
        # need st --> state we are deling with
        #kind map from fun vars to kind maps
        #generated from CollectKind
        #self.kenv = kenv
        #type env
        #this is the type env mapping from function names to input and output envs,
        #as well as predicates. These predicates deal with the relations among loci.
        #for example, if we have a locus x[i,j), y[m,n), Obviously, we should have
        #predicates to show that i < j, and m < n. Otherwise, the locus will not make sense.
        #recall that type env is not a map, but a list of pairs
        # first in a pair is a locus (a list of ranges), the second is a quantum type
        #we assume that this is also an input, because the input/output type envs can be generated
        #in TypeCollector
        #self.tenv = tenv
        #current fun name, where we want
        #self.name = f
        #the index for a function name to check
        #self.ind = ind
        #kind env, this is the step kind env inside a function f
        self.kinds = kenv
        # this is the type env generated from TypeCollector
        self.tenv = tenv
        #QXCall(f,...)
        #the checked type env at index
        #the generated type environment.
        self.renv = renv
        self.counter = counter

    def kenv(self):
        return self.kinds

    def renv(self):
        return self.renv

    #Need to deal with assertion
    #assertion might modify locus types
    def visitAssert(self, ctx: Programmer.QXAssert):
        return ctx.spec().accept(self)

    #It is correct here
    #since KindCollections might only collect
    #variables in the beginning, and in the return clause
    #it will not recall variables inside a function.
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
                self.renv = [(ctx.locus(), TyAA(), self.counter)] + vs
                self.counter += 1
                return True

        re = subLocusGen(ctx.locus(), self.renv)
        if re is None:
            return False
        newLoc, newTy, vs , num, = re
        self.renv = vs + [(newLoc, ty, num)]
        return True

    def visitBind(self, ctx: Programmer.QXBind):
        if ctx.type() is not None:
            ctx.type().accept(self)
        return ctx.ID()

    def visitQAssign(self, ctx: Programmer.QXQAssign):
        loc, ty, nenv, num = subLocusGen(ctx.locus(), self.renv)
        
        if isinstance(ctx.exp(), QXSingle):
            ty = addOneType(ty)
        if ty is None:
            return False

        self.renv = nenv
        self.renv += [(loc, ty, num)]
        return True

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        re = subLocus(ctx.locus(), self.renv)
        if re is None:
            return False
        nty, num, nenv = re

        for id in ctx.ids():
            self.kinds.update({id:TySingle("nat")})

        self.renv = [(ctx.locus(), nty, num)]+nenv
        return True

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        return True

    def visitIf(self, ctx: Programmer.QXIf):
        if isinstance(ctx.bexp(), QXBool):
            oldenv = self.renv
            for elem in ctx.stmts():
                elem.accept(self)
            if equalEnv(oldenv, self.renv):
                return True
            else:
                return False


        if isinstance(ctx.bexp(), QXQBool):
            findLocus = LocusCollector.LocusCollector()
            findLocus.visit(ctx.bexp())

            floc, ty, nenv, num = subLocusGen(findLocus.renv, self.renv)

            if isinstance(ty, TyNor):
                for elem in ctx.stmts():
                    elem.accept(self)
                return True

            for elem in ctx.stmts():
                findLocus.visit(elem)
            floc, ty, nenv, num = subLocusGen(findLocus.renv, self.renv)
            self.renv = [(floc,ty, num)] + nenv
            for elem in ctx.stmts():
                elem.accept(self)
            return True

    def visitFor(self, ctx: Programmer.QXFor):
        lbound = ctx.cranges()[0].left()
        rbound = ctx.cranges()[0].right()

        tmpv = self.renv
        self.kinds.update({ctx.ID():TySingle("nat")})
        self.renv = []
        for ielem in ctx.inv():
            if isinstance(ielem, QXQSpec):
                self.renv += [(ielem.locus(),ielem.qty(), self.counter)]
                self.counter += 1

        for elem in ctx.stmts():
            re = elem.accept(self)
            if not re:
                return re

        tmp1 = deepcopy(self.renv)
        simpler = SimpAExp()
        subst1 = SubstAExp(ctx.ID(), lbound)
        subst2 = SubstAExp(ctx.ID(), rbound)
        for elem, ty in tmp1:
            elem1 = subst1.visit(elem)
            elem1a = simpler.visit(elem1)
            elem2 = subst2.visit(elem)
            elem2a = simpler.visit(elem2)
            replaceEnvLoci(tmpv, elem1a, elem2a)

        self.renv = tmpv

        return True

    def visitCall(self, ctx: Programmer.QXCall):
        x = ctx.ID()
        kenv = self.kinds.get(x)
        tmpQVars = []
        tmpVars = []
        for i in range(len(kenv.items())):
            var, ty = kenv.items()[i]
            elem = ctx.exps()[i]
            if isinstance(ty, TyQ):
                tmpQVars += [(var,elem)]
            else:
                tmpVars += [(var,elem)]

        substs = []
        for var,elem in tmpVars:
            st = SubstAExp(var, elem)
            substs += [st]

        endEnv = self.tenv.get(x)[1]

        tmpNewEnv = []

        for loc, ty in endEnv:
            for ran in loc:
                id = substQVar(tmpQVars, ran.ID())
                left = substAllVars(substs, ran.cranges()[0].left())
                right = substAllVars(substs, ran.cranges()[0].right())
                v = QXQRange(id, [QXCRange(left, right)])
                tmpNewEnv += [(v,ty)]

        modEnv = self.renv

        for loc, ty in tmpNewEnv:
            vs = subLocusGen(loc, modEnv)
            if vs is None:
                return False

            floc, qty, ret, num = vs
            modEnv = [(floc, ty, self.counter)] + ret
            self.counter += 1

        self.renv = modEnv
        return True

    def visitCNot(self, ctx: Programmer.QXCNot):
        return ctx.next().accept(self)

    def visitEn(self, ctx: Programmer.TyEn):
        return ctx.flag().accept(self)

    def visitQSpec(self, ctx: Programmer.QXQSpec):
        ctx.qty().accept(self)
        for elem in ctx.locus():
            elem.accept(self)

        if isinstance(ctx.states(), list):
            return ctx.states()[0].accept(self)
        return ctx.states().accept(self)

    def visitTensor(self, ctx: Programmer.QXTensor):
        for elem in ctx.kets():
            if isinstance(elem, list):
                elem[0].accept(self)
            else:
                elem.accept(self)

    def visitSKet(self, ctx: Programmer.QXSKet):
        return ctx.vector().accept(self)

    def visitVKet(self, ctx: Programmer.QXVKet):
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
