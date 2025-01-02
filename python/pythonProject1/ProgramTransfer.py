import Programmer
from Programmer import *
from ProgramVisitor import ProgramVisitor
from TargetProgrammer import *
from CollectKind import *

def compareQRange(q1: QXQRange, q2: QXQRange):
    return q1.ID().ID() == q2.ID().ID() and compareAExp(q1.crange().left(),q2.crange().left()) and compareAExp(q1.crange().right(),q2.crange().right())

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

def subLocus(q2: [QXQRange], qs: [([QXQRange], QXQTy, int)]):
    vs = q2
    qsf = []
    for locus,qty,num in qs:
        vs = compareLocus(q2, locus)
        if vs is not None:
            return locus,qty, num
    return None

def genType(n:int, t:DXType):
    for i in range(n):
        t = SeqType(t)
    return t

def makeVars(locus:[QXQRange], t:QXQTy, n:int):
    tmp = []
    if isinstance(t, TyNor):
        for elem in locus:
            tmp += [DXBind(elem.ID(), SeqType(SType("bv1")),n)]

    elif isinstance(t, TyEn):
        num = t.flag().num()
        for elem in locus:
            tmp += [DXBind("amp", genType(num, SType("real")), n)]
            tmp += [DXBind("phase", genType(num, SType("real")), n)]
            tmp += [DXBind(elem.ID(), genType(num, SeqType(SType("bv1"))),n)]
    return tmp

def makeMap(ids: [str], locus: [QXQRange]):
    tmp = dict()
    for i in range(len(ids)):
        tmp.update({ids[i]:locus[i]})
    return tmp

def makeIndex(id : DXBind, sums: [QXCon]):
    tmp = id
    for elem in sums:
        tmp = DXIndex(tmp, DXBind(elem.ID(),SType("nat")))
    return tmp

class ProgramTransfer(ProgramVisitor):

    def __init__(self, kenv: dict, tenv: dict):
        # need st --> state we are deling with
        self.kenv = kenv
        self.tenv = tenv
        self.fkenv = None
        self.ftenvp = None
        self.ftenvr = None
        self.varnums = None
        self.sizemap = dict()
        self.counter = 0
        self.fvar = ""
        self.qvars = []
        self.locus = []

    def genVarNumMap(self, tenv: [([QXQRange], QXQTy)]):
        tmp = []
        for locus, qty in tenv:
            tmp = tmp + [(locus, qty, self.counter)]
            self.counter += 1
        return tmp

    def upvar(self, var:str):
        tmp = self.varnums.get(var)
        self.varnums.update({var:self.varnums.get(var)+1})
        return tmp

    def calRange(self, v:QXCRange):
        if isinstance(v.left(), QXNum) and v.left().num() == 0:
            return v.right()
        return QXBin("-", v.right(), v.left())

    def genSizeMap(self):
        self.sizemap = dict()
        for locus, qty, num in self.varnums:
            for elem in locus:
                v = self.calRange(elem.crange())
                self.sizemap.update({(elem.ID(), num):v})


    def genArgs(self, binds):
        for locus, qty, num in self.varnums:
            if isinstance(qty, TyNor):
                for elem in locus:
                    binds += [DXBind(elem.ID(), SeqType(SType("bv1")), num)]
            if isinstance(qty, TyHad):
                for elem in locus:
                    binds += [DXBind(elem.ID(), SeqType(SType("real")), num)]
            if isinstance(qty, TyEn):
                if isinstance(qty.flag(), QXNum):
                    tyv = qty.flag().num()
                    binds += [DXBind("amp", genType(tyv, SType("real")), num)]
                    binds += [DXBind("phase", genType(tyv, SType("real")), num)]
                    for elem in locus:
                        binds += [DXBind(elem.ID(), genType(tyv, SeqType(SType("bv1"))), num)]
                else:
                    return None

        return binds

    def removeLocus(self, n:int):
        vs = []
        for i in range(len(self.varnums)):
            locus,qty,num = self.varnums[i]
            if n == num:
                vs += self.varnums[i+1:len(self.varnums)]
                break
            else:
                vs += [(locus,qty,num)]

        self.varnums = vs


    def replaceType(self, n:int, t:QXQTy):
        vs = []
        for i in range(len(self.varnums)):
            locus,qty,num = self.varnums[i]
            if n == num:
                vs += [(locus,t,num)]+self.varnums[i+1:len(self.varnums)]
                break
            else:
                vs += [(locus,qty,num)]

        self.varnums = vs


    #argument generation 1) for classical variables, just directly gen.
    #2) for quantum variable, do not gen argument in qafny,
    # but take in the loci in requires, and gen variables according to loci with correct types.
    def visitMethod(self, ctx: Programmer.QXMethod):
        self.fvar = ctx.ID()
        self.fkenv = self.kenv.get(self.fvar)
        self.ftenvp = self.tenv.get(self.fvar)[0]
        self.ftenvr = self.tenv.get(self.fvar)[1]
        self.varnums = self.genVarNumMap(self.ftenvp)
        self.genSizeMap()

        tmpbind = []
        for bindelem in ctx.bindings():
            tmpv = bindelem.accept(self)
            if tmpv is not None:
                tmpbind.append(tmpv)

        tmpbind = self.genArgs(tmpbind)

        if tmpbind is None:
            return None

        tmpcond = []
        for condelem in ctx.conds():
            tmpcond.append(condelem.accept(self))

        axiom = ctx.axiom()
        tmpstmt = []
        if axiom:
            for stmtelem in ctx.stmts():
                tmpstmt.append(stmtelem.accept(self))

        tmpreturn = []
        for reelem in ctx.returns():
            tmpv = reelem.accept(self)
            if tmpv is not None:
                tmpreturn.append(tmpv)

        return DXMethod(self.fvar, axiom, tmpbind, tmpreturn, tmpcond, tmpstmt)


    def visitProgram(self, ctx: Programmer.QXProgram):
        tmp = []
        for elem in ctx.method():
            tmp.append(elem.accept(self))
        return DXProgram(tmp)


    def visitBind(self, ctx: Programmer.QXBind):
        if isinstance(ctx.type(), TySingle):
            ty = ctx.type().accept(self)
            return DXBind(ctx.ID(), ty, None)
        return None


    def visitAssert(self, ctx: Programmer.QXAssert):
        v = ctx.spec().accept(self)
        return DXAssert(v)


    def visitRequires(self, ctx: Programmer.QXRequires):
        v = ctx.spec().accept(self)
        return DXRequires(v)


    def visitEnsures(self, ctx: Programmer.QXEnsures):
        v = ctx.spec().accept(self)
        return DXEnsures(v)


    def visitCRange(self, ctx: Programmer.QXCRange):
        super().visitCRange(ctx)


    def visitCast(self, ctx: Programmer.QXCast):
        v = subLocus(ctx.locus(), self.varnums)
        if v is not None:
            loc,qty,num = v
            vs = compareLocus(ctx.locus(), loc)
            if not vs and isinstance(qty, TyHad) and isinstance(ctx.qty(), TyEn):
                result = [DXAssign(makeVars(ctx.locus(),ctx.qty(),self.counter),DXCall("hadEn", makeVars(ctx.locus(),TyHad(),num)))]
                self.removeLocus(num)
                self.varnums = [(loc,ctx.qty(),self.counter)] + self.varnums
                self.counter += 1
                return result


    def visitInit(self, ctx: Programmer.QXInit):
        return super().visitInit(ctx)


    def genKetList(self, varmap: dict, flag: int, num:int, ids: [str], kets: [QXKet]):
        tmp = []
        for i in range(len(ids)):
            if isinstance(kets[i].vector(), QXBind) and ids[i] == kets[i].vector().ID():
                var = varmap.get(ids[i]).ID()
                tmp += [DXAssign([DXBind(var,genType(flag,SeqType(SType("bv1"))),self.counter)],DXBind(var,genType(flag,SeqType(SType("bv1"))),num))]
            elif isinstance(kets[i].vector(), QXBin):
                var = varmap.get(kets[i].vector().left().ID())
                val = DXNum(kets[i].vector().right().num())
                tmp += [DXAssign([DXBind(var,genType(flag,SeqType(SType("bv1"))),self.counter)],DXCall("lambdaBaseEn",[val,DXBind(var,genType(flag,SeqType(SType("bv1"))),num)]))]
        return tmp


    def visitQAssign(self, ctx: Programmer.QXQAssign):
        v = subLocus(ctx.locus(), self.varnums)
        if v is not None:
            loc,qty,num = v

            if isinstance(qty, TyNor) and isinstance(ctx.exp(),QXSingle) and ctx.exp().op() == "H":
                vs = compareLocus(ctx.locus(), loc)
                if not vs:
                    self.replaceType(num,TyHad())
                else:
                    self.removeLocus(num)
                    self.varnums = [(ctx.locus(),TyHad,self.counter),(vs,TyNor(),num)] + self.varnums
                    self.counter += 1
                return [DXAssign(makeVars(ctx.locus(),TyNor(),self.counter),DXCall("hadNorHad", makeVars(ctx.locus(),TyNor(),num)))]

            if isinstance(qty, TyEn) and isinstance(ctx.exp(),QXSingle) and ctx.exp().op() == "H":
                flagNum = qty.flag().num()
                vs = compareLocus(ctx.locus(), loc)
                self.replaceType(num, TyEn(QXNum(flagNum + 1)))

                tmr = []
                for rem in vs:
                    tmr += [DXAssign(makeVars([rem], TyEn(QXNum(flagNum + 1)), self.counter), DXCall("castBaseEn", makeVars([rem], qty, num)))]
                result = tmr + [DXAssign(makeVars(ctx.locus(),TyEn(QXNum(flagNum + 1)),self.counter),DXCall("hadEn", makeVars(ctx.locus(),qty,num)))]
                self.removeLocus(num)
                self.varnums = [(loc,TyEn(QXNum(flagNum + 1)),self.counter)] + self.varnums
                self.counter += 1
                return result

            if isinstance(qty, TyEn) and isinstance(ctx.exp(),QXOracle):
                flagNum = qty.flag().num()
                varmap = makeMap(ctx.exp().ids(), ctx.locus())
                result = self.genKetList(varmap, flagNum, num, ctx.exp().ids(), ctx.exp().vectors)
                vs = compareLocus(ctx.locus(), loc)
                for rem in vs:
                    result += [DXAssign([DXBind(rem.ID(),genType(flagNum,SeqType(SType("bv1"))),self.counter)],DXBind(rem.ID(),genType(flagNum,SeqType(SType("bv1"))),num))]
                self.removeLocus(num)
                self.varnums = [(loc,qty,self.counter)] + self.varnums
                self.counter += 1
                return result

        return None


    def visitMeasure(self, ctx: Programmer.QXMeasure):
        return super().visitMeasure(ctx)

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        return super().visitCAssign(ctx)

    def visitIf(self, ctx: Programmer.QXIf):
        super().visitIf(ctx)

    def visitFor(self, ctx: Programmer.QXFor):
        return super().visitFor(ctx)

    def visitCall(self, ctx: Programmer.QXCall):
        return super().visitCall(ctx)

    def visitSingleT(self, ctx: Programmer.TySingle):
        return super().visitSingleT(ctx)

    def visitArrayT(self, ctx: Programmer.TyArray):
        ty = ctx.type().accept(self)
        return SeqType(ty)

    def visitFun(self, ctx: Programmer.TyFun):
        super().visitFun(ctx)

    def visitQ(self, ctx: Programmer.TyQ):
        super().visitQ(ctx)

    def visitCNot(self, ctx: Programmer.QXCNot):
        v = ctx.next().accept(self)
        return DXCNot(v)

    def visitNor(self, ctx: Programmer.TyNor):
        return super().visitNor(ctx)

    def visitTyHad(self, ctx: Programmer.TyHad):
        return super().visitTyHad(ctx)

    def visitEn(self, ctx: Programmer.TyEn):
        super().visitEn(ctx)

    def visitQSpec(self, ctx: Programmer.QXQSpec):
        loc,qty,num = subLocus(ctx.locus(), self.varnums)
        self.qvars = makeVars(ctx.locus(), ctx.qty(), num)
        self.locus = ctx.locus()
        return ctx.state().accept(self)


    def visitAA(self, ctx: Programmer.TyAA):
        return super().visitAA(ctx)

    def visitKet(self, ctx: Programmer.QXKet):
        return ctx.vector().accept(self)

    def visitTensor(self, ctx: Programmer.QXTensor):
        if ctx.ID() is None:
            x = DXBind("tmp", SType("nat"), self.counter)
            self.counter += 1
        else:
            x = ctx.ID()

        tmp = []

        for i in range(len(self.locus)):
            left = self.locus[i].crange().left().accept(self)
            right = self.locus[i].crange().right().accept(self)
            v = ctx.kets()[i].accept(self)
            tmp += [DXAll(x, DXLogic("==>", DXInRange(x, left, right), DXComp("==", DXIndex(self.qvars[i], x), v)))]

        return tmp


    def visitSum(self, ctx: Programmer.QXSum):
        tmp = []
        for i in range(len(self.qvars)):
            v = ctx.kets()[i].accept(self)
            eq = DXComp("==",makeIndex(self.qvars[i], ctx.sums()),v)
            for con in ctx.sums():
                x = DXBind(con.ID(), SType("nat"))
                range = DXInRange(x,con.crange().left().accept(self), con.crange().right().accept(self))
                eq = DXAll(x, DXLogic("==>",range,eq))
            tmp += [eq]

        num = self.qvars[0].num()
        ampvar = makeIndex(DXBind("amp", SType("real"), num),ctx.sums())
        v = ctx.amp().accept(self)
        eq = DXComp("==", ampvar, v)
        for con in ctx.sums():
            x = DXBind(con.ID(), SType("nat"))
            range = DXInRange(x, con.crange().left().accept(self), con.crange().right().accept(self))
            eq = DXAll(x, DXLogic("==>", range, eq))
        return ([eq]+tmp)

    def visitLogic(self, ctx: Programmer.QXLogic):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXLogic(ctx.op(), left, right)


    def visitBool(self, ctx: Programmer.QXComp):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXComp(ctx.op(), left, right)


    def visitQIndex(self, ctx: Programmer.QXQIndex):
        return super().visitQIndex(ctx)


    def visitCon(self, ctx: Programmer.QXCon):
        return super().visitCon(ctx)


    def visitQComp(self, ctx: Programmer.QXQComp):
        super().visitQComp(ctx)

    def visitQNot(self, ctx: Programmer.QXQNot):
        super().visitQNot(ctx)

    def visitAll(self, ctx: Programmer.QXAll):
        x = ctx.bind().accept(self)
        p = ctx.next().accept(self)
        DXAll(x, p)

    def visitBin(self, ctx: Programmer.QXBin):
        return DXBin(ctx.op(), ctx.left().accept(self), ctx.right().accept(self))

    def visitUni(self, ctx: Programmer.QXUni):
        return DXUni(ctx.op(), ctx.next().accept(self))

    def visitSingle(self, ctx: Programmer.QXSingle):
        return super().visitSingle(ctx)

    def visitOracle(self, ctx: Programmer.QXOracle):
        super().visitOracle(ctx)

    def visitNum(self, ctx: Programmer.QXNum):
        return DXNum(ctx.num())

    def visitHad(self, ctx: Programmer.QXHad):
        if ctx.state() == "+":
            return DXCall("omega", [DXNum(0), DXNum(2)])
        else:
            return DXCall("omega", [DXNum(1), DXNum(2)])

    def visitQRange(self, ctx: Programmer.QXQRange):
        return super().visitQRange(ctx)