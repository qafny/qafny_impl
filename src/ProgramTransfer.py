import Programmer
from LocusCollector import LocusCollector
from BindingCollector import BindingCollector
from Programmer import *
from ProgramVisitor import ProgramVisitor
from SubstAExp import SubstAExp
from SubstDAExp import SubstDAExp
from SubstIndex import SubstIndex
from SubstLambda import SubstLambda
from TargetProgrammer import *
from CollectKind import *
from TargetToString import TargetToString
from TypeChecker import TypeChecker, subLocusGen, compareType
from EqualityVisitor import EqualityVisitor
from UpdateVars import UpdateVars



def eqQRange(q1: QXQRange, q2: QXQRange):
    return (str(q1.location()) == str(q2.location()) and compareAExp(q1.crange().left(),q2.crange().left())
            and compareAExp(q1.crange().right(),q2.crange().right()))


def compareQRange(q1: QXQRange, q2: QXQRange):
    return str(q1.location()) == str(q2.location()) and compareAExp(q1.crange().left(),q2.crange().left())

def compareRangeLocus(q1: QXQRange, qs: [QXQRange]):
    vs = []
    for i in range(len(qs)):
        if compareQRange(q1,qs[i]):
            if compareAExp(q1.crange().right(), qs[i].crange().right()):
                return (vs + (qs[i+1:len(qs)]))
            else:
                return (vs + [QXQRange(q1.location(), qs[i].index(),
                                       QXCRange(qs[i].crange().left(),
                                                QXBin("+",q1.crange().right(), QXNum(1)),
                                                parser_context=qs[i].crange()), parser_context=qs[i])]
                        + (qs[i+1:len(qs)]))
        vs = vs + [qs[i]]
    return None

# compareLocus and return the reminder locus
def compareLocus(q1: [QXQRange], q2: [QXQRange]):
    vs = q2
    for elem in q1:
        vs = compareRangeLocus(elem, vs)
        if vs is None:
            return None
    return vs

# check if q2 is in the database, and then return locus,qty,num, where q2 is part of locus
def subLocus(q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):
    vs = q2
    qsf = []
    for locus, qty, num in qs:
        vs = compareLocus(q2, locus)
        if vs is not None:
            return locus, qty, num
    return None


def genType(n:int, t:DXType):
    for i in range(n):
        t = SeqType(t, t)
    return t

def makeVars(locus: [QXQRange], t: QXQTy, n: int):
    tmp = dict()
    if isinstance(t, TyNor):
        for elem in locus:
            tmp.update({elem.location(): DXBind(elem.location(), SeqType(SType("bv1")),n)})

    elif isinstance(t, TyEn):
        num = t.flag().num()
        for elem in locus:
            tmp.update({elem.location(): DXBind(elem.location(), genType(num, SeqType(SType("bv1"))),n)})
        # amplitude comes after the value in hadEn, that should be the precedent
        tmp += [DXBind("amp", genType(num, SType("real", transformed_from=num)), n, transformed_from=elem)]
    
    elif isinstance(t, TyHad):
        for elem in locus:
            tmp.update({elem.location(): DXBind(elem.location(), SeqType(SType("real")), n)})

    return tmp

def meetType(t1: QXQTy, t2: QXQTy):
    if isinstance(t1, TyNor) and not isinstance(t2, TyHad):
        return t2
    if isinstance(t2, TyNor) and not isinstance(t1, TyHad):
        return t1
    if isinstance(t1, TyHad) and not isinstance(t2, TyNor):
        return t2
    if isinstance(t2, TyHad) and not isinstance(t1, TyNor):
        return t1
    if isinstance(t1, TyEn) and isinstance(t2, TyEn):
        if t1.flag() < t2.flag():
            return t2
        else:
            return t1
    if isinstance(t1, TyEn) and isinstance(t2, TyAA):
        if t1.flag() < t2.flag():
            return t2
        else:
            return TyAA(t1.flag(), t2.qrange(), parser_context=t2)
    if isinstance(t1, TyAA) and isinstance(t2, TyEn):
        if t1.flag() < t2.flag():
            return TyAA(t2.flag(), t1.qrange(), parser_context=t1)
        else:
            return t1
    else:
        if t1.flag() < t2.flag():
            return t2
        else:
            return t1

def makeMap(ids: [str], locus: [QXQRange]):
    tmp = dict()
    for i in range(len(ids)):
        tmp.update({str(ids[i]):locus[i]})
    return tmp

def makeIndex(id : DXBind, sums: [QXCon]):
    tmp = id
    for elem in sums:
        tmp = DXIndex(tmp, DXBind(elem.ID(), SType("nat", transformed_from=elem), transformed_from=elem), transformed_from=elem)
    return tmp

def locateAExp(locus: [QXQRange], r:QXQRange, lexp:[DXAExp]):
    for i in range(len(locus)):
        if compareQRange(locus[i], r):
            return lexp[i]
    return None

def replaceAExp(locus: [QXQRange], r:QXQRange, lexp:[DXAExp], rep: DXAExp):
    re = []
    for i in range(len(locus)):
        if compareQRange(locus[i], r):
            re += [rep] + lexp[i+1:]
            return re
        else:
            re += [lexp[i]]
    return lexp

def updateInd(lexp:[DXAExp], ind: DXAExp):
    tmp = []
    for elem in lexp:
        tmp += [DXIndex(elem, ind, transformed_from=elem)]
    return tmp

def constructIndex(v:DXAExp, vs: [DXBind]):
    for elem in vs:
        v = DXIndex(v, elem)
    return v

def findHadRange(q2 : QXQRange, qs: [([QXQRange], QXQTy, dict)]):

    vs = []
    for i in len(qs):
        loc, qty, vars = qs[i]
        if isinstance(qty, TyHad):
            v = compareRangeLocus(q2, loc)
            if not v:
                vs += [(v, qty, vars)]+qs[i+1:]
                return q2, vars, vs
            else:
                return None
        vs += [qs[i]]
    return None

def findHadLocus(self, q2 : [QXQRange], qs: [([QXQRange], QXQTy, dict)]):

    for i in len(q2):
        v = findHadRange(q2[i], qs)
        if v is not None:
            return v


class StackFactor:
    pass

class EnFactor(StackFactor):
    def __init__(self, condition: (str, DXAExp, DXAExp)):
        self._condition = condition

    def condition(self):
        return self._condition

class AAFactor(StackFactor):
    def __init__(self, left: DXComp, right: DXComp):
        self._left = left
        self._right = right

    def left(self):
        return self._left

    def right(self):
        return self._right



# In the transfer below. transferring a stmt/exp results in a list of resulting stmts in Dafny
# but transferring logic specification results in one specification
class ProgramTransfer(ProgramVisitor):

    def __init__(self, kenv: dict, tenv: dict):
        # need st --> state we are deling with
        # kind env, can be generated by CollectKind.
        # mapping from function names to mappings.
        # each mapping is a map from variables to kinds in a method.
        # different methods have different mapping
        self.kenv = kenv
        # type env, can be generated by TypeCollector
        # mapping from function names to two lists of pairs.
        # each list contains pairs of a locus and a type
        # meaning that the locus has a certain type
        # different loci in a list are disjoint
        # there are two lists for each function name
        # The first list contains the type information for the loci at the input
        # The second list contains the type information for the loci at the output location
        self.tenv = tenv
        # fkenv a kind env in a function.
        # in visitMethod, we find out the kind-env for the fun-name.
        self.fkenv = None
        #ftenvp gets the locus-list at the input position of a function
        self.ftenvp = None
        # ftenvr gets the locus-list at the output position of a function
        self.ftenvr = None
        #varnums is the locus-list at the input position
        #There is an additional field for each pair (like now it is not a pair, but triple)
        #The third field iin a triple is a generated identifier (a number) to identify the specific locus
        self.varnums = None
        self.currLocus = None
        self.originLocus = None
        self.conStack = []
        #outvarnums is the locus-list at the ouput position
        self.outvarnums = None
        #The sizemap is for used inside a function
        #This sizemap maps a range in a locus to its size
        #like x[i,j) has the size j - i
        self.sizemap = dict()
        #The counter generates a new identifier for a locus,
        #When generating a locus ID, we increment the counter
        self.counter = 0
        #The function name when visiting a method
        self.fvar = ""
        #For a locus in a spec, we generate Dafny variables for each range in a locus,
        #depending on the types
        #For example, if x[i,j) , y[0,n) |-> en(1).
        #In Qafny, the variables x and y have no extra number identifier, because it is unnecessary
        #In Dafny, each instruction/function call will generate new seq variables
        #So, we need to always generate different new instances for x and y
        #So, for a spec, we will call the counter to generate the new identifier
        #and the Dafny variable for the locus is DVar(x,counter), DVar(y,counter),
        # DVar(amp,counter), and DVar(phase,counter)
        # The variables are also associated with types, and depend on types
        #Now, the DVars above have type seq<real> or seq<seq<bv1>>
        # if we have a en(2) type above, we will have seq<seq<real>> and seq<seq<seq<bv1>>>
        self.qvars = []
        #When analyzing a locus in a specific plance
        #We assign the locus to the global field in the visitor class
        #This will reserves the locus and allow the sub-visitor-call to assign the current locus
        self.locus = []

        # additional Dafny methods, as template libs generated from the method
        # in printing, one needs to print out the additional methods first
        self.addFuns = []

        # additional Dafny methods that need to be pulled from DafnyLibrary
        self.libFuns = set()

        # flag variable to handle transfering of ensures differently to map the correct output variable number to the ensures
        self.t_ensures = False

        #dictionary to store all initial locus data like length and values
        self.initial_locus_data = {}

        # qafny line number to input into Dafny AST
        self.current_qafny_line_number = None

    #add DX functions to cast types
    def joinIfLocus(self, q: QXQRange, aLocus: [QXQRange], aTy: QXQTy, aVars: dict,
                       bLocus: [QXQRange], bTy: QXQTy, bVars: dict):

        #case when bTy is TyEn
        if isinstance(bTy, TyEn):
            if isinstance(aTy, TyNor):
                if eqQRange(q, aLocus[0]):
                    return [], [((bLocus + [q]), bTy,
                           bVars.update({q.location(): DXBind(q.location(), bVars.values()[0].type(), aVars(q.location()).num())}))]
                elif compareLocus(q, aLocus[0]):
                    vs = [((bLocus + [q]), bTy, bVars.update({q.location(): DXBind(q.location(),
                                                                                   bVars.values()[0].type(), self.counter)})),
                          ([QXQRange(q.location(),
                                     QXCRange(QXBin(q.crange().right(), QXNum(1)), aLocus[0].crange().right()),
                                     parser_context=q)], TyNor, aVars(q.location()).num())]
                    self.counter += 1
                    return vs
            # we only allow one qubit had in bexp
            if isinstance(aTy, TyHad):
                if eqQRange(q, aLocus[0]):
                    return [((bLocus + [q]), TyAA(bTy.flag(), q.crange()),
                      bVars.update({q.location(): DXBind(q.location(), bVars.values()[0].type(), aVars(q.location()).num())}))]
                elif compareLocus(q, aLocus[0]):
                    vs = [((bLocus + [q]), TyAA(bTy.flag(), q.crange()), bVars.update({q.location(): DXBind(q.location(),
                                                                                   bVars.values()[0].type(), self.counter)})),
                          ([QXQRange(q.location(),
                                     QXCRange(QXBin(q.crange().right(), QXNum(1)), aLocus[0].crange().right()),
                                     parser_context=q)], TyHad, aVars(q.location()).num())]
                    self.counter += 1
                    return vs
        return None

    def genHadEnCastPred(self, vars: dict, qty: QXQTy, transformed_from: Union[QXTop, DXTop]):
        newvars = self.upVarsType(vars, qty)
        result = [DXInit(x, transformed_from=transformed_from) for x in newvars.values()]
        newampvar = [x for x in newvars.values() if x.ID() == 'amp']
        othervars = [x for x in newvars.values() if x.ID() != 'amp']
        result += [DXAssign(newampvar + othervars, DXCall("hadEn", vars.values(), transformed_from=transformed_from),
                            transformed_from=transformed_from)]
        self.libFuns.add('hadEn')
        return result

    def genEnNorExtendPred(self, x: DXBind, y: DXBind, qty: QXQTy, transformed_from: Union[QXTop, DXTop]):
        self.libFuns.add('mergeBitEn')
        v = DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXCall('mergeBitEn',
                                [DXLength(x), y]),True, transformed_from=transformed_from)
        self.counter += 1
        return v

    #TODO: need to modify the meaning of duplicateMergeBitEn to be a AA type choice,
    #the number of elements in seq will not change, but we will have case for y == 0 and y == 1
    def genEnHadAASeqPred(self, vars: dict, y: DXBind, qty: QXQTy, transformed_from: Union[QXTop, DXTop]):
        x = vars.values()[0]
        res = []

        yleft = y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)
        res += DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXNum(0),True, transformed_from=transformed_from)
        self.counter += 1

        yright = y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)
        res += DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXNum(1),True, transformed_from=transformed_from)
        self.counter += 1

        self.libFuns.add('duplicateMergeBitEn')
        self.libFuns.add('mergeAmpEn')
        self.libFuns.add('omega')
        half = DXBin('/', QXNum(1), DXUni('sqrt', 2))
        omega = DXCall('omega', [DXNum(1),DXNum(2)], True, transformed_from=transformed_from)
        for elem in vars.values():
            if elem.ID() != 'amp':
                #the duplicateMergeBitEn will simply copy the same result from the old elem to be a new one
                #and perform y == 0 ==> duplicate as well as y == 1 ==> duplicate
                res += [DXAssign([DXBind(elem.ID(), genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                     DXCall('duplicateMergeBitEn',  [yleft, DXNum(0), DXLength(x), elem]),
                     True, transformed_from=transformed_from)]
                self.counter += 1
                res += [DXAssign([DXBind(elem.ID(), genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                     DXCall('duplicateMergeBitEn',  [yright, DXNum(1), DXLength(x), elem]),
                     True, transformed_from=transformed_from)]
                self.counter += 1
            else:
                res += [DXAssign([DXBind('amp', genType(qty.flag(), (SType("real"))), self.counter)],
                     DXCall('mergeAmpEn', [yleft, DXNum(0), DXLength(x), elem, half]),
                     True, transformed_from=transformed_from)]
                self.counter += 1
                res += [DXAssign([DXBind('amp', genType(qty.flag(), (SType("real"))), self.counter)],
                     DXCall('mergeAmpEn', [yleft, DXNum(0), DXLength(x), elem,
                                           DXBin('*', half, DXBin('*', omega, y))]),
                     True, transformed_from=transformed_from)]
                self.counter += 1


    def superLocus(self, q2: [QXQRange], ty:QXQTy):
        vs = []
        for i in len(self.varnums):
            loc, qty, vars = self.varnums[i]
            if ty == qty:
                remind = compareLocus(loc, q2)
                if remind is not None:
                    return remind, loc, qty, vars, (vs + self.varnums[i+1:])
            vs += [self.varnums[i]]
        return None

    def computeType(self, q2: [QXQRange]):
        tmp = None
        for elem in q2:
            loc, qty, vars = subLocus([elem], self.varnums)
            if tmp is None:
                tmp = qty
            else:
                tmp = meetType(tmp, qty)
        return tmp

    def includeIfLocus(self, q2: [QXQRange]):
        v = subLocus(q2, self.varnums)
        if v is not None:
            return [], v

        ty = self.computeType(q2)
        rt = self.superLocus(q2, ty)

        if rt is not None:
            remind, loc, qty, vars, tmpVarNums = rt
            if isinstance(ty, TyHad):
                pred = self.genHadEnCastPred(vars, qty, loc[0])
                qty = TyEn(QXNum(1), transformed_from=qty)
                norRe, norVars, tmpVarNumsa = self.collectNorLocus(remind, tmpVarNums)
                loc = loc + norRe
                for v in norVars.values():
                    pred += [self.genEnNorExtendPred(vars.values()[0], v, qty, loc[0])]
                vars.update({k:v for k,v in norVars.items()})
                self.varnums = ([(loc, qty, vars)]) + tmpVarNumsa
                return pred, loc, qty, vars

            if isinstance(ty, TyEn):
                norRe, norVars, tmpVarNumsa = self.collectNorLocus(remind, tmpVarNums)
                loc = loc + norRe
                pred = []
                for v in norVars.values():
                    pred += [self.genEnNorExtendPred(vars.values()[0], v, qty, loc[0])]
                vars.update({k:v for k,v in norVars.items()})
                vb = findHadLocus(remind, tmpVarNumsa)
                if vb is not None:
                    qa, oldVars, tmpVarNumsb = vb
                    pred += self.genEnHadAASeqPred(vars, oldVars.values()[0], ty, loc[0])
                    qty = TyAA(qty.flag(), qa, loc[0])
                self.varnums = ([(loc, qty, vars)]) + tmpVarNumsa
                return pred, loc, qty, vars
        return None


    def collectNorLocus(self, q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):
        qv = []
        vs = []
        for elem in q2:
            for i in len(qs):
                loc, qty, vars = qs[i]
                if isinstance(qty, TyNor):
                    if eqQRange(elem, loc[0]):
                        vs += [(loc, qty, vars)]
                    else:
                        reLoc = compareRangeLocus(elem, loc)
                        qv += [(reLoc, qty, vars)]

                        vs += [(elem, TyNor, self.upVars(vars))]
                else:
                    qv += [(loc, qty, vars)]

        newVars = dict()
        va = []
        for loc, qty, vars in vs:
            va += loc
            newVars.update({k:v for k,v in vars.items()})

        return va, newVars, qv
                #vs = compareLocus(loc, [elem]):



    def upVars(self, v: dict):
        tmp = dict()
        for key in v.keys():
            tmp.update({key: v.get(key).newBind(self.counter)})
            self.counter += 1

        return tmp

    def upVarsSub(self, v: dict, qs: set):
        tmp = dict()
        for key in v.keys():
            if key in qs:
                tmp.update({key: v.get(key).newBind(self.counter)})
                self.counter += 1
            else:
                tmp.update({key: v.get(key)})
        return tmp

    #create new DXBind with TyNor and TyHad type Only
    #and for TyEnv, we increment the type to 1
    def upVarsType(self, v: dict, t: QXQTy):
        tmp = dict()
        for key in v.keys():
            if isinstance(t, TyNor):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("bv1")), self.counter)})
            elif isinstance(t, TyHad):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("real")), self.counter)})
            elif isinstance(t, TyEn):
                tmp.update({key: v.get(key).newBindType(SeqType(v.get(key).type()), self.counter)})
            self.counter += 1

        return tmp

    def genVarNumMap(self, tenv: [([QXQRange], QXQTy)]):
        tmp = []
        for locus, qty in tenv:
            tdis = dict()
            for r in locus:
                tdis.update({r.location(): self.counter})
                self.counter += 1
            tmp = tmp + [(locus, qty, tdis)]
            self.counter += 1
        return tmp

    def upvar(self, var: str):
        tmp = self.varnums.get(var)
        self.varnums.update({var:self.varnums.get(var)+1})
        return tmp

    def calRange(self, v: QXCRange):
        '''Converts a QXCRange to a QXAExpr that represents the length of the q-bit array.'''
        if isinstance(v.left(), QXNum) and v.left().num() == 0:
            return v.right()
        return QXBin("-", v.right(), v.left(), v)

    def genSizeMap(self):
        self.sizemap = dict()
        for locus, qty, num in self.varnums:
            for elem in locus:
                v = self.calRange(elem.crange())
                self.sizemap.update({(elem.location(), num[elem.location()]): v})


    def genArgs(self, binds):
        for locus, qty, num in self.varnums:
            if isinstance(qty, TyNor):
                for elem in locus:
                    binds += [DXBind(elem.location(), SeqType(SType("bv1", transformed_from=qty), transformed_from=qty), num, transformed_from=qty)]
            if isinstance(qty, TyHad):
                for elem in locus:
                    binds += [DXBind(elem.location(), SeqType(SType("real", transformed_from=qty), transformed_from=qty), num, transformed_from=qty)]
            if isinstance(qty, TyEn):
                if isinstance(qty.flag(), QXNum):
                    tyv = qty.flag().num()
                    binds += [DXBind("amp", genType(tyv, SType("real", transformed_from=qty)), num, transformed_from=qty)]
                    for elem in locus:
                        binds += [DXBind(elem.location(), genType(tyv, SeqType(SType("bv1", transformed_from=qty), transformed_from=qty)), num, transformed_from=qty)]
                else:
                    return None

        return binds

    def genBindRequires(self):
        '''Generates pre-conditions (i.e. requires) based off of the binds present in this method.'''

        def generate_pow2_expr(node: Union[QXBind, QXNum]) -> DXCall:
            if isinstance(node, QXBind):
                return DXCall('pow2', [DXBind(node.ID(), SType("nat"))])
            return DXCall('pow2', [DXNum(node.num())])

        def generateENRequiresForLocus(locus: [QXQRange], qty: TyEn, curr_bind: DXBind, lcounter: int) -> [DXRequires]:
            res = []
            for i in range(qty.flag().num()+1):
                if i == 0:
                    tr = DXCall('pow2', [DXBind(locus[i].crange().right().ID(),SType("nat"))
                                         if isinstance(locus[i].crange().right(), QXBind)
                                         else DXNum(locus[i].crange().right().num())], transformed_from=locus[i].crange().right())
                    tmp = DXComp('==', DXLength(curr_bind), tr, transformed_from=locus[i].crange().right())
                    res.append(DXRequires(tmp, transformed_from=locus[i].crange().right()))
                else:
                    allvar = DXBind('tmp', SType('nat', transformed_from=locus[i-1]), lcounter, transformed_from=locus[i-1])
                    lcounter += 1
                    pow2_in_var = DXCall('pow2',[DXBind(locus[i-1].crange().right().ID(),SType("nat"))],
                                         transformed_from=locus[i-1].crange()) \
                        if isinstance(locus[i-1].crange().right(), QXBind) \
                        else DXCall('pow2',[DXNum(locus[i-1].crange().right().num())],
                                    transformed_from=locus[i-1].crange())
                    pow2_var = DXCall('pow2',[DXBind(locus[i].crange().right().ID(), SType("nat"))
                                              if isinstance(locus[i].crange().right(), QXBind)
                                              else DXNum(locus[i].crange().right().num())])\
                        if i < len(locus) else (DXBind(locus[i-1].crange().right().ID(), SType("nat"))
                                                if isinstance(locus[i-1].crange().right(), QXBind) else DXNum(locus[i-1].crange().right().num()))
                    pow2_in_var = generate_pow2_expr(locus[i-1].crange().right())
                    pow2_var = (generate_pow2_expr(locus[i].crange().right()) 
                       if i < len(locus) else 
                       DXBind(locus[i-1].crange().right().ID(), SType("nat")))
                    #if isinstance(pow2_var.exps()[0], DXNum) or isinstance(pow2_var.exps()[0], DXVar):
                        #pow2_var = DXNum(2**int(pow2_var.exps()[0].ID()))
                    if i == qty.flag().num() and curr_bind.ID() == 'amp':
                        continue
                    elif i == qty.flag().num() and curr_bind.ID() != 'amp':
                        currloc = [x for x in locus if x.location() == curr_bind.ID()]
                        pow2_var = DXBind(currloc[0].crange().right().ID(), SType("nat")) \
                            if isinstance(currloc[0].crange().right(), QXBind) else DXNum(currloc[0].crange().right().num())
                        left = DXLength(DXIndex(curr_bind, allvar), transformed_from=curr_bind)
                        comp = DXComp('==', left, pow2_var, transformed_from=curr_bind)
                        if i == 1:
                            tmp = DXAll(allvar, DXLogic('==>', DXInRange(allvar, DXNum(0), pow2_in_var), comp),
                                        transformed_from=locus[i-1])
                            res.append(DXRequires(tmp, transformed_from=locus[i-1]))
                            comp = tmp
                        else:
                            if isinstance(comp, DXAll):
                                prevall = comp.next().left()
                                pr = comp.next().right()

                        while isinstance(pr, DXAll):
                            prevall = DXLogic("==>", prevall, DXAll(pr.bind(), pr.next().left()),
                                                  transformed_from=locus[i-1])
                            pr = pr.next().right()
                            
                        comp = DXAll(comp.bind(),
                                         DXLogic('==>', prevall,
                                                 DXAll(allvar, DXLogic("==>",
                                                                       DXInRange(allvar, DXNum(0), pow2_in_var),
                                                                       DXComp("==", left, pow2_var)))),
                                         transformed_from=locus[i-1])
                        res.append(DXRequires(comp, transformed_from=locus[i-1]))

                    elif i == 1:
                        left = DXLength(DXIndex(curr_bind, allvar), transformed_from=locus[i-1])
                        comp = DXComp('==', left , pow2_var, transformed_from=locus[i-1])
                        tmp = DXAll(allvar, DXLogic('==>', DXInRange(allvar, DXNum(0), pow2_in_var), comp),
                                    transformed_from=allvar)
                        res.append(DXRequires(tmp, transformed_from=locus[i-1]))
                        comp = tmp
                    else:
                        left = DXLength(DXIndex(left.var(), allvar), transformed_from=left.var())
                        if isinstance(comp, DXAll):
                            prevall = comp.next().left()
                            pr = comp.next().right()

                        while isinstance(pr, DXAll):
                            prevall = DXLogic("==>", prevall, DXAll(pr.bind(), pr.next().left()),
                                              transformed_from=prevall)
                            pr = pr.next().right()
                        
                        comp = DXAll(comp.bind(), DXLogic('==>', prevall,
                                                          DXAll(allvar, DXLogic("==>", DXInRange(allvar, DXNum(0), pow2_in_var),
                                                                                DXComp("==", left, pow2_var)))),
                                     transformed_from=comp.bind())
                        res.append(DXRequires(comp, transformed_from=comp))

            return res

        conditions = []
        lcounter = self.counter
        for locus, qty, num in self.varnums:
            if isinstance(qty, TyEn):
                for elem in locus:
                    curr_bind = DXBind(elem.location(), num=num)
                    if isinstance(elem.crange().right(), QXBind):
                        conditions.append(DXRequires(DXComp('>', elem.crange().right().accept(self), DXNum(0)),
                                                     transformed_from=elem.crange().right()))
                    conditions.extend(generateENRequiresForLocus(locus, qty, curr_bind, lcounter))

                amp_bind = DXBind('amp', num=num)
                conditions.extend(generateENRequiresForLocus(locus, qty, amp_bind, lcounter))
            else:
                for elem in locus:
                    right = DXBind(elem.crange().right().ID(), SType("nat")) \
                        if isinstance(elem.crange().right(), QXBind) else DXNum(elem.crange().right().num())
                    left = elem.crange().left()
                    if isinstance(elem.crange().right(), QXBind):
                        conditions.append(DXRequires(DXComp('>', elem.crange().right().accept(self), DXNum(0)),
                                                     transformed_from=elem.crange().right()))
                    if isinstance(left, QXNum) and left.num() == 0:
                        conditions.append(DXRequires(DXComp('==', DXLength(DXBind(elem.location(), num=num)), right),
                                                     transformed_from=elem.location()))
                    else:
                        conditions.append(DXRequires(DXComp('==', DXLength(DXBind(elem.location(), num=num)),
                                                            DXBin('-',right, left.accept(self))),
                                                     transformed_from=elem.location()))

        return conditions
    
    #generate returns with different variables than input so that we can use then in ensures (since lists are immutable in dafny)
    def genOutArgs(self, binds):
        for locus, qty, num in self.outvarnums:
            if isinstance(qty, TyNor):
                for elem in locus:
                    binds += [DXBind(elem.location(), SeqType(SType("bv1")), num)]
            if isinstance(qty, TyHad):
                for elem in locus:
                    binds += [DXBind(elem.location(), SeqType(SType("real")), num)]
            if isinstance(qty, TyEn):
                if isinstance(qty.flag(), QXNum):
                    tyv = qty.flag().num()
                    binds += [DXBind("amp", genType(tyv, SType("real")), num)]
                    for elem in locus:
                        binds += [DXBind(elem.location(), genType(tyv, SeqType(SType("bv1"))), num)]
                else:
                    return None

        return binds

    def genOutEnsures(self):
        '''Generates post-conditions (i.e. ensures) based off of the return binds generated for this method'''

        def generateENEnsuresForLocus(locus, qty, curr_bind, lcounter: int):
            res = []
            ttype = SType('bv1')
            cb_id = curr_bind.ID()
            for i in range(qty.flag().num() + 1):
                rval = None
                if curr_bind.ID() == 'amp' and i == qty.flag().num():
                    break
                if i == qty.flag().num():
                    for lo in locus:
                        if lo.location() == cb_id:
                            rval = lo.crange().right().accept(self) \
                                if isinstance(lo.crange().left(), QXNum) and lo.crange().left().num() == 0 \
                                else DXBin('-', lo.crange().right().accept(self), lo.crange().left().accept(self))
                            break
                else:
                    rval = locus[i].crange().right().accept(self) \
                        if isinstance(locus[i].crange().left(), QXNum) and locus[i].crange().left().num() == 0 \
                        else DXBin('-', locus[i].crange().right().accept(self), locus[i].crange().left().accept(self))
                    rval = DXCall('pow2', [rval])

                cb = curr_bind
                lc = lcounter
                tmp_t = ttype
                while isinstance(tmp_t, SeqType):
                    cb = DXIndex(cb, DXBind('tmp', SType('nat'), lc))
                    lc += 1
                    tmp_t = tmp_t.type()
                
                val = DXComp('==', DXLength(cb), rval)
                
                res += [DXEnsures(val, transformed_from=locus[i])] \
                    if i == 0 else \
                    [DXEnsures(self.genAllSpec_Simple(DXBind('tmp', SType('nat'), lcounter), curr_bind, ttype, val),
                               transformed_from=locus[i])]
                ttype = SeqType(ttype)

            return res


        conditions = []
        lcounter = self.counter
        for locus, qty, num in self.outvarnums:
            if isinstance(qty, TyEn):
                for elem in locus:
                    curr_bind = DXBind(elem.location(), ty = genType(qty.flag().num(), SType('bv1')), num=num)
                    conditions.extend(generateENEnsuresForLocus(locus, qty, curr_bind, lcounter))

                amp_bind = DXBind('amp', num=num)
                conditions.extend(generateENEnsuresForLocus(locus, qty, amp_bind, lcounter))
            else:
                for elem in locus:
                    right = DXBind(elem.crange().right().ID(), SType("nat")) \
                        if isinstance(elem.crange().right(), QXBind) else DXNum(elem.crange().right().num())
                    left = elem.crange().left()
                    if isinstance(left, QXNum) and left.num() == 0:
                        conditions.append(DXEnsures(DXComp('==', DXLength(DXBind(elem.location(), num=num)), right),
                                                    transformed_from=elem.location()))
                    else:
                        conditions.append(DXEnsures(DXComp('==', DXLength(DXBind(elem.location(), num=num)),
                                                           DXBin('-',right, left.accept(self))),
                                                    transformed_from=elem.location()))

        return conditions

    def removeLocus(self, vs:[QXQRange]):
        tmp = []
        for i in range(len(self.varnums)):
            locus,qty,num = self.varnums[i]
            if vs == locus:
                tmp += self.varnums[i+1:len(self.varnums)]
                break
            else:
                tmp += [(locus,qty,num)]

        self.varnums = tmp


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

    def updateOutVarNums(self, qstmt, dstmt):
        if isinstance(dstmt, DXAssign) and (isinstance(qstmt, QXQAssign) or isinstance(qstmt, QXCast)):
                            for i in range(len(self.outvarnums)):
                                loc, qty, num = self.outvarnums[i]
                                if compareLocus(qstmt.locus(), loc) or compareLocus(qstmt.locus(), loc) == []:
                                    self.outvarnums[i] = loc, qty, dstmt.ids()[0].num()




    #argument generation 1) for classical variables, just directly gen.
    #2) for quantum variable, do not gen argument in qafny,
    # but take in the loci in requires, and gen variables according to loci with correct types.
    # We need to create a typechecker here.
    def visitMethod(self, ctx: Programmer.QXMethod):
        self.fvar = str(ctx.ID())
        self.fkenv = self.kenv.get(self.fvar)
        self.ftenvp = self.tenv.get(self.fvar)[0]
        self.ftenvr = self.tenv.get(self.fvar)[1]
        self.varnums = self.genVarNumMap(self.ftenvp)
        self.outvarnums = self.genVarNumMap(self.ftenvr)
        self.genSizeMap()

        self.current_qafny_line_number = ctx.line()
        tmpbind = []
        for bindelem in ctx.bindings(): #collect classical binds
            tmpv = bindelem.accept(self)
            if tmpv is not None:
                tmpbind.append(tmpv)
        
        tmpbind = self.genArgs(tmpbind)

        if tmpbind is None:
            return None

        # conditions (requires/ensures) for the generated Dafny method
        tmpcond = []
        # add conditions derived from the input types (i.e. Q[n] ==> |Q| == n)
        tmpcond.extend(self.genBindRequires())

        for condelem in ctx.conds():
            # add the requires conditions to the generated Dafny conditions
            if isinstance(condelem, QXRequires):
                self.current_qafny_line_number = condelem.line()
                tmpcond.extend(condelem.accept(self))

        self.current_qafny_line_number = ctx.line()
        # add predicates existing from the type environment
        for preds in self.tenv[self.fvar][2]:
            x = preds.accept(self)
            if isinstance(x, list):
                tmpcond.extend(x)
            else:
                tmpcond.append(x)

        axiom = ctx.axiom()
        tmpstmt = []
        if not axiom:
            tc = TypeChecker(self.kenv, self.tenv, self.varnums, self.counter) #should be fkenv and ftenv
            for stmtelem in ctx.stmts():
                self.current_qafny_line_number = stmtelem.line()
                stmtelem.accept(tc)
                s = stmtelem.accept(self)
                if isinstance(s, list):
                    for st in s:
                        self.updateOutVarNums(stmtelem, st)
                    tmpstmt.extend(s)
                else:
                    self.updateOutVarNums(stmtelem, s)
                    tmpstmt.append(s)
        
        self.current_qafny_line_number = ctx.line()
        for i in range(len(self.outvarnums)):
            loc,qty, num = self.outvarnums[i]
            newvars = makeVars(loc, qty, self.counter)
            oldvars = makeVars(loc, qty, num)
            tmpstmt.append(DXAssign(newvars, oldvars, transformed_from=ctx))
            self.outvarnums[i] = (loc, qty, self.counter)
            self.counter += 1

        self.t_ensures = True

        # add conditions derived from the return types
        tmpcond.extend(self.genOutEnsures())

        for condelem in ctx.conds():
            if isinstance(condelem, QXEnsures):
                self.current_qafny_line_number = condelem.line()
                tmpens = condelem.accept(self)
                tmpcond.extend(tmpens)

        tmpreturn = []
        for reelem in ctx.returns():
            self.current_qafny_line_number = reelem.line()
            tmpv = reelem.accept(self)
            if tmpv is not None:
                tmpreturn.append(tmpv)

        self.current_qafny_line_number = ctx.line()
        tmpreturn = self.genOutArgs(tmpreturn)
        self.libFuns.add('abs')
        self.libFuns.add('powN')
        return DXMethod(str(self.fvar), axiom, tmpbind, tmpreturn, tmpcond, tmpstmt, transformed_from=ctx)


    def visitProgram(self, ctx: Programmer.QXProgram):
        tmp = []
        for elem in ctx.topLevelStmts():
            tmp.append(elem.accept(self))
        return DXProgram(tmp, transformed_from=ctx)


    def visitBind(self, ctx: Programmer.QXBind):
        vs = self.currLocus
        if vs is not None:
            loc, qty, vars = vs
            return vars(ctx.ID())

        if isinstance(ctx.type(), TySingle):
            ty = ctx.type().accept(self)
            return DXBind(ctx.ID(), ty, None, transformed_from=ctx)
        if ctx.ID() and not ctx.type():
            return DXBind(ctx.ID(), None, transformed_from=ctx)
        return None


    def visitAssert(self, ctx: Programmer.QXAssert):
        v = ctx.spec().accept(self)
        x = [DXAssert(i, transformed_from=ctx) for i in v] \
            if isinstance(v,list) else DXAssert(v, transformed_from=ctx)
        return x
        

    def visitRequires(self, ctx: Programmer.QXRequires):
        v = ctx.spec().accept(self)
        v = v if isinstance(v, list) else [v]
        x = [DXRequires(i, transformed_from=ctx) for i in v]
        return x


    def visitEnsures(self, ctx: Programmer.QXEnsures):
        v = ctx.spec().accept(self)
        v = v if isinstance(v, list) else [v]
        x = [DXEnsures(i, transformed_from=ctx) for i in v]
        return x

    def visitCRange(self, ctx: Programmer.QXCRange):
        super().visitCRange(ctx)


    def visitCast(self, ctx: Programmer.QXCast):
        self.current_qafny_line_number = ctx.line()
        v = subLocus(ctx.locus(), self.varnums)
        if v is not None:
            loc,qty,num = v
            vs = compareLocus(ctx.locus(), loc)
            if not vs and isinstance(qty, TyHad) and isinstance(ctx.qty(), TyEn):
                newvars = self.upVarsType(num,qty)
                result = [DXInit(x, transformed_from=ctx) for x in newvars.values()]
                newampvar = [x for x in newvars.values() if x.ID() == 'amp']
                othervars = [x for x in newvars.values() if x.ID() != 'amp']
                result += [DXAssign(newampvar + othervars,DXCall("hadEn", makeVars(ctx.locus(),TyHad(),num),
                                                                 transformed_from=ctx), transformed_from=ctx)]
                self.libFuns.add('hadEn')
                self.removeLocus(loc)
                self.varnums = [(loc,ctx.qty(),newvars)] + self.varnums
                #self.counter += 1
                return result
    """
        else:
            v = subLocusGen(ctx.locus(),self.varnums)
            if v is not None:
                (floc, ty, rev, num) = v
                vs = compareLocus(ctx.locus(), floc)
                if not vs and isinstance(ty, TyHad) and isinstance(ctx.qty(), TyEn):
                    result = [DXAssign(makeVars(ctx.locus(), ctx.qty(), self.counter),
                                       DXCall("hadEn", makeVars(ctx.locus(), TyHad(), num), qafny_line_number=ctx.line_number()), qafny_line_number=ctx.line_number())]
                    self.libFuns.add('hadEn')
                    if num != -1:
                        self.removeLocus(num)
                    else:
                        for i in floc:
                            tloc, tqty, tnum = subLocus([i], self.varnums)
                            self.removeLocus(tnum)
                    self.varnums = [(floc, ty, self.counter)] + self.varnums
                    self.counter += 1
                    return result
                
                elif not vs and isinstance(ctx.qty(), TyEn):
                    result = []
                    calllist = []
                    flag = False
                    for i in range(len(ctx.locus())):
                        tloc, tqty, tnum = subLocus([ctx.locus()[i]], self.varnums)
                        if isinstance(tqty, TyNor):
                            flag = True
                        calllist.extend(makeVars(tloc, tqty, tnum))
                            

                    newvars = makeVars(floc, ctx.qty(), self.counter)
                    result += [DXInit(x, qafny_line_number=ctx.line_number()) for x in newvars]

                    if flag:
                        result += [DXAssign(newvars, DXCall('hadNorEn', calllist, qafny_line_number=ctx.line_number()), qafny_line_number=ctx.line_number())]
                        self.libFuns.add('hadNorEn')
                    else:
                        result += [DXAssign(newvars, DXCall("hadEn", calllist, qafny_line_number=ctx.line_number()), qafny_line_number=ctx.line_number())]
                        self.libFuns.add('hadEn')

                    if num != -1:
                        self.removeLocus(num)
                    else:
                        for i in floc:
                            tloc, tqty, tnum = subLocus([i], self.varnums)
                            self.removeLocus(tnum)
                    self.varnums = [(floc, ty, self.counter)] + self.varnums
                    self.counter += 1
                    return result
                    """


                            
                    


    def visitInit(self, ctx: Programmer.QXInit):
        return DXInit(ctx.binding().accept(self), transformed_from=ctx)

    def genAllSpec(self, sbind, compleft, compright, isamp):
            type = compleft.type()
            comp = DXComp("==", compleft, compright, transformed_from=compleft)
            counter = self.counter + 1
            var = sbind

            if not isamp:
                type = type.type()

            while isinstance(type, SeqType):
                rlength = DXLength(compleft, transformed_from=compleft)
                if not isamp and isinstance(type.type(), SType):
                    compleft = DXCall('castBVInt', [DXIndex(compleft, var, transformed_from=compleft)],
                                      transformed_from=compleft)
                    self.libFuns.add('castBVInt')
                else:
                    compleft = DXIndex(compleft, var, transformed_from=compleft)

                if isinstance(comp, DXAll):
                    prevall = comp.next().left()
                    pr = comp.next().right()

                    while isinstance(pr, DXAll):
                        prevall = DXLogic("==>", prevall, DXAll(pr.bind(), pr.next().left()),
                                          transformed_from=prevall)
                        pr = pr.next().right()
                    comp = DXAll(comp.bind(), DXLogic('==>', prevall,
                                                      DXAll(var, DXLogic("==>",
                                                                         DXInRange(var, DXNum(0,
                                                                                              transformed_from=var),
                                                                                   rlength, transformed_from=var),
                                                                         DXComp("==", compleft, compright,
                                                                                transformed_from=compleft),
                                                                         transformed_from=var),
                                                            transformed_from=var),
                                                      transformed_from=prevall),
                                 transformed_from=comp.bind())
                else:
                    comp = DXAll(var, DXLogic("==>", DXInRange(var,
                                                               DXNum(0, transformed_from=var), rlength,
                                                               transformed_from=var),
                                              DXComp("==", compleft, comp.right(), transformed_from=compleft),
                                              transformed_from=var), transformed_from=var)

                type = type.type()
                var = DXBind(var.ID(), var.type(), counter, transformed_from=var)
                counter += 1

            return comp
    
    def genAllSpec_Simple(self, sbind, var, type, val):
        res = None

        while isinstance(type, SeqType):
            var = DXIndex(var, sbind, transformed_from=var)
            type = type.type()
            if isinstance(type, SeqType):
                sbind = DXBind(sbind.ID(), sbind.type(), sbind.num() + 1, transformed_from=sbind)

        while isinstance(var, DXIndex):
            var = var.bind()
            res = DXAll(sbind, DXLogic('==>', DXInRange(sbind, DXNum(0, transformed_from=sbind),
                                                        DXLength(var, transformed_from=var),
                                                        transformed_from=sbind), res if res else val,
                                       transformed_from=sbind), transformed_from=sbind)
            sbind = DXBind(sbind.ID(), sbind.type(), sbind.num() - 1, transformed_from=sbind)

        return res

    
    def replaceInBin(self, bin, eval : list[str], rval):
        res = None
        l = None
        r = None
        if isinstance(bin.left(), DXBin):
            l = self.replaceInBin(bin.left(), eval, rval)
        
        if isinstance(bin.right(), DXBin):
            r = self.replaceInBin(bin.right(), eval, rval)

        if l is None:
            if isinstance(bin.left(), DXBind):
                if bin.left().ID() in eval:
                    l = rval
                else:
                    l = bin.left()
            elif isinstance(bin.left(), DXNum):
                l = bin.left()

        if r is None:
            if isinstance(bin.right(), DXBind):
                if bin.right().ID() in eval:
                    r = rval  
                else:
                    r = bin.right()
            elif isinstance(bin.right(), DXNum):
                r = bin.right()

        return DXBin(bin.op(), l, r, transformed_from=bin)    

        
            
    #generate predicates.
    def genPreds(self, locus: [QXQRange], t:QXQTy, vars: [DXBind], newVars: [DXBind],
                 ids : [str], kets:[QXKet], phase: QXAExp, unchanged_range: [QXQRange]):
        #vars = makeVars(locus, t, num)

        if isinstance(t, TyEn):
            pVar = [x for x in vars if x.ID() == 'amp'][0]
            kVars = [x for x in vars if x.ID() != 'amp']

            for x in range(len(kVars)): 
                indVar = kVars[x]
                tcount = self.counter
                for i in range(t.flag().num()):
                    indVar = DXIndex(indVar, DXBind('tmp', SType('nat'),
                                                    tcount, transformed_from=indVar),
                                     transformed_from=self.currLocus)
                    tcount += 1
                kVars[x] = indVar

            tcount = self.counter
            for i in range(t.flag().num()):
                pVar = DXIndex(pVar, DXBind('tmp', SType('nat'), tcount,
                                            transformed_from=self.currLocus),
                               transformed_from=self.currLocus)
                tcount += 1
            
            #newVars = makeVars(locus, t, newNum)
            newPVar = [x for x in newVars if x.ID() == 'amp'][0]
            newKVars = [x for x in newVars if x.ID() != 'amp']

            unchanged_range = [str(x.location()) for x in unchanged_range]
            new_ucr = []
            tmpKVars = []

            for i in newKVars:
                if i.ID() in unchanged_range:
                    new_ucr.append(i)
                else:
                    tmpKVars.append(i)
            

            newKVars = tmpKVars
            
            tmp_ucr = []
            tmp_kVars = []
            for i in kVars:
                x = i
                while isinstance(x, DXIndex):
                    x = x.bind()
                x = x.ID()

                if x in unchanged_range:
                    tmp_ucr.append(DXCall('castBVInt',[i], transformed_from=self.currLocus))
                else:
                    tmp_kVars.append(DXCall('castBVInt',[i], transformed_from=self.currLocus))
                self.libFuns.add('castBVInt')
            
            unchanged_range = tmp_ucr

            tmpSubs = []
            for i in range(len(ids)):
                subst = SubstDAExp(ids[i], tmp_kVars[i])
                tmpSubs += [subst]


            res = []
            for ket in kets:
                re = ket.accept(self)
                for esub in tmpSubs:
                    re = esub.visit(re)
                res += [re]

            preds = []  

            vars = sorted(vars, key = lambda x: x.ID())
            newVars = sorted(newVars, key = lambda x: x.ID())

            arg_type = SType('bv1')
            for ent in range(t.flag().num() + 1):
                tmp = []
                for i in range(len(vars)):
                    if ent == t.flag().num() and vars[i].ID() == 'amp':
                        continue
                    if ent == 0:
                        tmp.append(DXLength(newVars[i], transformed_from=self.currLocus))
                        tmp.append(DXLength(vars[i], transformed_from=self.currLocus))
                    else:
                        p_oldvar = self.createIndexFromType(vars[i], arg_type,
                                                            DXBind('tmp', SType('nat'), self.counter,
                                                                   transformed_from=self.currLocus))
                        p_newvar = self.createIndexFromType(newVars[i], arg_type,
                                                            DXBind('tmp', SType('nat'), self.counter,
                                                                   transformed_from=self.currLocus))
                        tmp.append(DXLength(p_oldvar, transformed_from=self.currLocus))
                        tmp.append(DXLength(p_newvar, transformed_from=self.currLocus))

                tres = DXComp('==', tmp[0], tmp[1], transformed_from=self.currLocus)
                for i in range(2, len(tmp)):
                    tres = DXComp('==', tres, tmp[i], transformed_from=self.currLocus)

                if ent == 0:
                    preds += [tres]
                else:
                    preds += [self.genAllSpec_Simple(DXBind('tmp', SType('nat'), self.counter,
                                                            transformed_from=self.currLocus),
                                                     newVars[0], arg_type, tres)]
                arg_type = SeqType(arg_type)
                


            for i in range(len(newKVars)):
                preds += [self.genAllSpec(DXBind('tmp', SType('nat'), self.counter,
                                                 transformed_from=self.currLocus),
                                          newKVars[i], res[i], False)]

            for i in range(len(new_ucr)):
                preds += [self.genAllSpec(DXBind('tmp', SType('nat'), self.counter,
                                                 transformed_from=self.currLocus),
                                          new_ucr[i], unchanged_range[i], False)]

            newp = phase.accept(self)
            for esub in tmpSubs:
                newp = esub.visit(newp)

            newp = DXBin('*', pVar, newp, transformed_from=pVar)

            preds += [self.genAllSpec(DXBind('tmp', SType('nat'), self.counter,
                                             transformed_from=self.currLocus), newPVar, newp, True)]

            return preds

        if isinstance(t, TyNor):
            tmpSubs = []
            for i in range(len(ids)):
                if isinstance(vars[i].type(),SeqType) and isinstance(vars[i].type().type(), SType) and vars[i].type().type().type() == 'bv1':
                    vars[i] = DXCall('castBVInt', [vars[i]], transformed_from=self.currLocus)
                    self.libFuns.add('castBVInt')
                subst = SubstDAExp(ids[i], vars[i])
                tmpSubs += [subst]
            #newVars = makeVars(locus, t, newNum)

            res = []
            for ket in kets:
                re = ket.accept(self)
                for esub in tmpSubs:
                    re = esub.visit(re)
                res += [re]

            preds = []

            vars = sorted(vars, key = lambda x: x.exps()[0].ID())
            newVars = sorted(newVars, key = lambda x: x.ID())
            for i in range(len(newVars)):
                preds += [DXComp('==', DXLength(newVars[i], transformed_from=self.currLocus),
                                 DXLength(vars[i].exps()[0], transformed_from=self.currLocus),
                                 transformed_from=self.currLocus)]
                preds += [DXComp("==", DXCall('castBVInt', [newVars[i]],
                                              transformed_from=self.currLocus), res[i],
                                 transformed_from=self.currLocus)]
                self.libFuns.add('castBVInt')
            return preds



    def dealExps(self, locus: [QXQRange], pexp:DXAExp, lexp:[DXAExp], exps: [QXStmt]):
        for elem in exps:
            if isinstance(elem, QXQAssign) and isinstance(elem.exp(), QXOracle):
                loc = elem.locus()
                ids = elem.exp().ids()
                kets = elem.exp().vectors()
                phase = elem.exp().phase()

                tmpSubs = []
                for i in range(len(ids)):
                    tarExp = locateAExp(locus, loc[i], lexp)
                    if tarExp is None:
                        return None
                    subst = SubstDAExp(ids[i], tarExp)
                    tmpSubs += [subst]

                res = []
                for ket in kets:
                    re = ket.accept(self)
                    for esub in tmpSubs:
                        re = esub.visit(re)
                    res += [re]

                for i in range(len(loc)):
                    lexp = replaceAExp(locus, loc[i], lexp, res[i])

                newp = phase.accept(self)
                for esub in tmpSubs:
                    newp = esub.visit(newp)

                return DXBin("*", pexp, newp, transformed_from=pexp), lexp
            
            '''elif isinstance(elem, QXQAssign) and isinstance(elem.exp(), QXSingle):
                if elem.exp().op() == 'H':
                    for i in locus:
                        v = compareLocus(i, elem.locus())
                        if v is not None:
                            lv, qty, num = subLocus(i, self.varnums)
                            lexp = replaceAExp(locus, lv, lexp, )
                            nty = genType(qty.num()+1,SeqType(SType("bv1")))'''


                    



    def genKetList(self, varmap: dict, flag: int, num:int, ids: [str], kets: [QXKet]):
        tmp = []
        for i in range(len(ids)):
            if isinstance(kets[i].vector(), QXBind) and ids[i] == kets[i].vector().ID():
                var = varmap.get(str(ids[i])).ID()
                tmp += [DXAssign([DXBind(var, genType(flag, SeqType(SType("bv1"))), self.counter,
                                         transformed_from=var)],
                                 DXBind(var, genType(flag,SeqType(SType("bv1"))), num,
                                        transformed_form=var),
                                 transformed_from=var)]
            elif isinstance(kets[i].vector(), QXBin):
                var = varmap.get(str(ids[i])).ID()
                val = kets[i].vector().accept(self)
                tmp += [DXAssign([DXBind(var,genType(flag,SeqType(SType("bv1"))),self.counter)],
                                 DXCall("lambdaBaseEn",[val,DXBind(var,genType(flag,SeqType(SType("bv1"))),num,
                                                                   transformed_from=var)],
                                        transformed_from=self.var), transformed_from=var)]
                self.libFuns.add('lambdaBaseEn')
        return tmp


    def visitQAssign(self, ctx: Programmer.QXQAssign):
        if self.currLocus is not None:
            loca, qtya, varsa = self.currLocus
            cs = compareLocus(ctx.locus(), loca)
            if cs is None:
                vs = self.includeIfLocus(ctx.locus())
                if vs is not None:
                    pred, loc, qty, vars = vs
                    self.currLocus = loc, qty, vars
                    news = dict()
                    for elem in loc:
                        va = vars.get(elem.location())
                        news.update({elem.location(): va})
                    self.originLocus = loc, news
                    v = ctx.exp().accept(self)
                    if v is not None:
                        return (pred + ctx.exp().accept(self))
            else:
                loc, varsb = self.originLocus
                self.originLocus = cs, varsb
                return ctx.exp().accept(self)
                #return res

        return None

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        return super().visitMeasure(ctx)

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        return QXCAssign([DXBind(x) for x in ctx.ids()], ctx.aexp().accept(self), transformed_from=ctx)

    def getMapIndex(self, bind):
        for loc, qty, n in self.varnums:
            if n == bind.num():
                for i in loc:
                    if i.ID() == bind.ID():
                        toString = TargetToString()
                        return i.ID() + i.crange().left().accept(toString) + ',' + i.crange().right().accept(toString)
                    
    
    def findRangeLocus(self, arg1: QXQRange):
        loc1 = []
        complete_range = False
        for loc, qty, num in self.varnums:
            for i in loc:
                if i.location() == arg1.location() and ((compareAExp(i.crange().left(), arg1.crange().left())
                                                         and compareAExp(i.crange().right(), arg1.crange().right()))):
                    loc1 += loc
                    loc1qty = qty
                    loc1num = num
                    complete_range = True
                    break
                elif i.location() == arg1.location() and compareAExp(i.crange().left(), arg1.crange().left()):
                    loc1 += loc
                    loc1qty = qty
                    loc1num = num
                    break
            if loc1 is not None and len(loc1) > 0:
                break  

        return loc1, loc1qty, loc1num, complete_range

    #given a locus, we generate type cast predicate and expression
    #to cast a state predicate for a locus with a certain type to another type
    #with the ability of merging loci together.
    #every Dafny variable is the quantum array variable's name + a counter number
    #when needed to generate new variable, we increment the counter and output var+number.
    def mergeLocus(self, ran: list[QXQRange]):
        fqty = None
        locus_list = []
        cut_list = []
        res = []
        no_en = True #check if the type is en or not
        newNum = self.counter #generate new variables
        self.counter += 1


        for i in ran:
            loc, qty, num, cr = self.findRangeLocus(i)
            if not cr:
                continue
            no_en = not isinstance(qty, TyEn)
            fqty = compareType(qty, fqty)
            insert = True
            for j in locus_list:
                if num == j[2]:
                    insert = False
                    break
            if insert:
                locus_list.append((loc, qty, num)) #loc, its type and the variable flag
    
        for i in ran:
            loc, qty, num, cr = self.findRangeLocus(i)
            if cr:
                continue
            no_en = not isinstance(qty, TyEn)
            fqty = compareType(qty, fqty)
            insert = True
            for j in locus_list:
                if num == j[2]:
                    insert = False
                    break

            if insert:
                if not cr and (isinstance(qty, TyHad) or isinstance(qty, TyNor)):
                    match_loc = None
                    match_qty = None
                    match_num = None
                    for tmp in range(len(locus_list)):
                        tmpitem = locus_list[tmp]
                        tloc, rqty, rnum = tmpitem 
                        
                        for ran in tloc:
                            if ran.location() == i.location():
                                match_loc = tloc
                                match_qty = rqty
                                match_num = rnum
                                break

                        if match_loc:
                            break
                    
                    cut_list.append((loc, qty, num, match_loc, match_qty, match_num ))
                    

        target_locus = None
        for item in locus_list:
            if type(item[1]) == type(fqty) and (item[1].flag().num() == fqty.flag().num() if (isinstance(item[1], TyEn) and isinstance(fqty, TyEn)) else True):
                target_locus = item

            
        if not target_locus and no_en and len(ran) == 2:
            if isinstance(locus_list[0][1], TyNor):
                norloc = locus_list[0]
                hadloc = locus_list[1]
            else:
                norloc = locus_list[1]
                hadloc = locus_list[0]
            self.libFuns.add('hadNorEn')
            res += [DXAssign([DXBind('amp', None, newNum, transformed_from=norloc[0][0]),
                              DXBind(hadloc[0][0].location(), None, newNum, transformed_from=hadloc[0][0].location()),
                              DXBind(norloc[0][0].location(), None, newNum, transformed_from=norloc[0][0].location())],
                            DXCall('hadNorEn', [DXBind(hadloc[0][0].location(), None, hadloc[2], transformed_from=hadloc[0][0].location()),
                                                DXBind(norloc[0][0].location(), None, norloc[2], transfromed_from=norloc[0][0].location())],
                                   transformed_from=hadloc[0][0].location()), True, transformed_from=hadloc[0][0])]
            self.removeLocus(hadloc[2])
            self.removeLocus(norloc[2])
            self.varnums += [(hadloc[0] + norloc[0], fqty, newNum)]
            return res, hadloc[0] + norloc[0], fqty, newNum

        if len(cut_list) > 0:
            for item in cut_list:
                loc, qty, num, match_loc, match_qty, match_num = item
                if isinstance(qty, TyHad):
                    self.libFuns.add('cutHad')
                    res += [DXAssign([DXBind(loc[0].location(), None, newNum)],
                                     DXCall('cutHad', [DXBind(loc[0].location(), None, num)]),
                                     True, transformed_from=loc[0].location())]
                    self.removeLocus(num)
                    self.varnums += [([QXQRange(loc[0].location(),
                                                crange =  QXCRange(QXBin('+',loc[0].crange().left(), QXNum(1)), loc[0].crange().right()))], qty, newNum)]
                    self.counter += 1
                    res += [DXAssign([DXBind(loc[0].location(), None, self.counter)],
                                     DXIndex(DXBind(loc[0].location(), None, num), DXNum(0)), True, transformed_from=loc[0].location())]
                    cutnum = self.counter
                    self.counter += 1


                    if match_loc:
                        matchRange = [x for x in match_loc if x.location() == loc[0].location()][0]
                        if isinstance(match_qty, TyEn) and match_qty.flag().num() == 1:
                            self.libFuns.add('mergeBitEn')
                            self.libFuns.add('duplicateMergeBitEn')
                            res += [DXAssign([DXBind(loc[0].location(), None, self.counter)],
                                             DXCall('mergeBitEn',
                                                    [DXBind(loc[0].location(), None, match_num), DXBind(loc[0].crange().left().ID())]),
                                             True, transformed_from=loc[0].location())]
                            for l in match_loc:
                                if l.location() != loc[0].location():
                                    res += [DXAssign([DXBind(l.location(), None, self.counter)],
                                                     DXCall('duplicateMergeBitEn', [DXBind(l.location(), None, match_num)]),
                                                     True, transformed_from=l.location())]
                            self.libFuns.add('mergeAmpEn')
                            res += [DXAssign([DXBind('amp', None, self.counter)],
                                             DXCall('mergeAmpEn', [DXBind('amp', None, match_num), DXBind(loc[0].location(), None, cutnum)]),
                                             True, transformed_from=loc[0].location())]
                            self.libFuns.add('omega0')
                            res += [DXCall('omega0', [], True, transformed_from=loc[0].location())]
                            self.libFuns.add('mergeBitTrigger')
                            res += [DXCall('mergeBitTrigger', [DXBind(loc[0].location(), None, match_num),
                                                               DXBind(loc[0].location(), None, self.counter),
                                                               DXLength(DXIndex(DXBind(loc[0].location(), None, match_num), DXNum(0)))],
                                           True, transformed_from=loc[0].location())]
                            self.libFuns.add('triggerSqrtMul')
                            res += [DXCall('triggerSqrtMul', [], True, transformed_from=loc[0].location())]

                            self.removeLocus(match_num)
                            tmp = [([x for x in match_loc if x.location() != loc[0].location()]
                                    + [QXQRange(loc[0].location(),
                                                crange = QXCRange(matchRange.crange().left(), QXBin('+',matchRange.crange().right(), QXNum(1))))],
                                    match_qty, self.counter)]
                            self.varnums += tmp
                            self.counter += 1
                            return res, tmp[0][0], tmp[0][1], tmp[0][2]
                        
    
        final_loc = []
        for item in locus_list:
            loc, qty, num = item
            if type(qty) != type(fqty):
                if isinstance(qty, TyNor) and isinstance(fqty, TyEn):
                    self.libFuns.add('norEn' + str(fqty.flag().num()))
                    for ran in loc:
                        res += [DXAssign([DXBind(ran.location(), None, newNum)],
                                         DXCall('norEn' + str(fqty.flag().num()),
                                                [DXBind(ran.location(), None, num), DXBind(target_locus[0][0].location(), None, target_locus[2])]),
                                         True, transformed_from=target_locus[0][0].location())]
                        self.removeLocus(num)
                    self.removeLocus(target_locus[2]) 
            else:   
                res += [DXAssign([DXBind(x.location(), None, newNum)],
                                 DXBind(x.location(), None, target_locus[2]),
                                 True, transfromed_from=x.location()) for x in target_locus[0]]
                res += [DXAssign([DXBind('amp', None, newNum)],
                                 DXBind('amp', None, target_locus[2]), True, transformed_from=target_locus[2])]

            final_loc.extend(loc)

        self.varnums += [(final_loc, fqty, newNum)]

        return res, final_loc, fqty, newNum
    
    def getBindFromIndex(self, index : DXIndex):
        res = index
        while isinstance(res, DXIndex):
            res = res.bind()

        return res
    
    def createIndexFromType(self, bind : DXBind, type : SeqType, index : DXBind):
        res = bind
        while isinstance(type, SeqType):
            res = DXIndex(res, index)
            index = DXBind(index.ID(), index.type(), index.num() + 1)
            type = type.type()
        return res

    def visitIf(self, ctx: Programmer.QXIf):

        self.current_qafny_line_number = ctx.line()
        #deal with classical boolean expression
        if isinstance(ctx.bexp(), QXBool):
            bex = ctx.bexp().accept(self)

            tmpMap = deepcopy(self.varnums)
            terms = []
            for elem in ctx.stmts():
                terms += elem.accept(self)

            self.varnums = tmpMap
            elses = []
            for elem in ctx.else_stmts():
                elses += elem.accept(self)
            #typeCheck = TypeChecker(self.fkenv, self.tenv, self.varnums, self.counter)
            #typeCheck.visit(ctx)
            #self.fkenv = typeCheck.kenv()
            #self.varnums = typeCheck.renv()
            #self.counter = typeCheck.counter

            return [DXIf(bex, terms, elses, transformed_from=ctx)]

        #deal with quantum conditional
        #upgrade_en = False
        #res = []
        #nLoc, nqty, nnum = None, None, None
        #bool_exp_id = None
        #bool_exp_index = None
        #is_qrange = False
        #is_qcomp = False
        #bool_store_id = ''


        #algorithm for hanlding Qif case, the above is classical if
        #first step, find locus on boolean and expression, no need of type checking
        #if locus merged is not in en typed, then type case all those Had/Nor typed locus into
        #en type

        #Second, look up the conditional, if it is QXQIndex, then we can collect the index in the stack
        #otherwise, we need to generate a loop to apply the conditional to the index

        # collect statements
        # should use Locus Collector to infer the stmts correction first.
        # do not understand the logic here.
        # the bool_exp_id, bool_exp_index can be merged with the other two cases.

        lc = LocusCollector()
        lc.visit(ctx.bexp())
        for stmt in ctx.stmts():
            lc.visit(stmt)

        #includeIfLocus provides a final locus, type, and num, as well as the result lib function call in Dafny in tres
        tres, nLoc, nqty, nnum = self.includeIfLocus(lc.renv)
        self.currLocus = nLoc, nqty, nnum

        if isinstance(ctx.bexp(), QXQIndex):
            bres = ctx.bexp().accept(self)
            changeVar = UpdateVars(nnum, self.counter)
            changeVar.visit(bres)
            self.currLocus = nLoc, nqty, changeVar.vars()
            self.currLocus = changeVar.counter()
            self.conStack += [EnFactor(('==', bres, DXNum(1)))]
            ifexp = []
        else:
            ifexp = ctx.bexp().accept(self)

        #loop_oldVars = makeVars(nLoc, nqty, nnum)
        #loop_oldVars = {x.ID(): x for x in oldVars}
        #loop_oldVars = nnum
        #loop_newVars, self.counter = self.upVars(loop_oldVars, self.counter)
        #loop_newVars = {x.ID(): x for x in newVars}
        #nLoc_dict = {x.location(): x for x in nLoc}


        #if isinstance(ctx.bexp(), QXQComp):
        #    bool_exp_id = ctx.bexp().index().ID()
        #elif isinstance(ctx.bexp(), QXQRange):
        #    bool_exp_id = ctx.bexp().location()
        #else:
        #    bool_exp_id = ctx.bexp().ID()

        #bool_exp_id = ctx.bexp().ID()
        #newBind = loop_oldVars.get(bool_exp_id).newBind(self.counter)
        #self.counter += 1

        #self.conStack += [EnFactor(DXComp('==', DXCall('castBVInt', [newBind]), DXNum(1),
        #                        qafny_line_number=self.current_qafny_line_number))]

        result = ifexp

        for stmt in ctx.stmts():
            tmp = stmt.accept(self)
            if tmp is None:
                return None
            else:
                result += [tmp]

        return result

    def visitFor(self, ctx: Programmer.QXFor):
        tmp_current_qafny_line_number = self.current_qafny_line_number
        self.current_qafny_line_number = ctx.line()
        x = ctx.ID()
        tmpinvs = []
        for inv in ctx.inv():
            tmpinvs += [inv.accept(self)]

        tmpstmts = []
        for elem in ctx.stmts():
            tmpstmts += elem.accept(self)

        lbound = ctx.crange().left().accept(self)
        rbound = ctx.crange().right().accept(self)
        vx = DXBind(x, SType("nat"))
        self.current_qafny_line_number = tmp_current_qafny_line_number
        return [DXInit(vx, lbound, transformed_from=ctx),
                DXWhile(DXComp("<", vx, rbound, transformed_from=ctx),
                        tmpstmts, tmpinvs, transformed_from=ctx)]


    #This might be oversimplified. We might need to cast
    #some locus for the Requires
    #and then recall the coming back states
    #I mean this might live inside a quantum conditional
    #To transfer a quantum conditional, we might need to do so
    def visitCall(self, ctx: Programmer.QXCall):
        return DXCall(str(ctx.ID()), [x.accept(self) for x in ctx.exps()], transformed_from=ctx)

    def visitSingleT(self, ctx: Programmer.TySingle):
        return SType(ctx.type(), transformed_from=ctx)

    def visitArrayT(self, ctx: Programmer.TyArray):
        ty = ctx.type().accept(self)
        return SeqType(ty, transformed_from=ctx)

    def visitFun(self, ctx: Programmer.TyFun):
        super().visitFun(ctx, transformed_from=ctx)

    def visitQ(self, ctx: Programmer.TyQ):
        return ctx.flag().accept(self)

    def visitCNot(self, ctx: Programmer.QXCNot):
        v = ctx.next().accept(self)
        return DXNot(v, transformed_from=ctx)

    def visitNor(self, ctx: Programmer.TyNor):
        return super().visitNor(ctx)

    def visitTyHad(self, ctx: Programmer.TyHad):
        return super().visitTyHad(ctx)

    def visitEn(self, ctx: Programmer.TyEn):
        super().visitEn(ctx)

    def visitQSpec(self, ctx: Programmer.QXQSpec):
        tmp_current_qafny_line_number = self.current_qafny_line_number
        self.current_qafny_line_number = ctx.line()
        loc, qty, num = subLocus(ctx.locus(), self.outvarnums if self.t_ensures else self.varnums)
        self.qvars = makeVars(ctx.locus(), ctx.qty(), num)
        self.locus = ctx.locus()
        res = []
        for i in ctx.states():
            result = i.accept(self)
            if isinstance(result, list):
                res.extend(result)
            else:
                res.append(result)
        self.current_qafny_line_number = tmp_current_qafny_line_number
        return res


    def visitAA(self, ctx: Programmer.TyAA):
        return super().visitAA(ctx)

    def visitSKet(self, ctx: Programmer.QXSKet):
        return ctx.vector().accept(self)

    def visitVKet(self, ctx: Programmer.QXVKet):
        return ctx.vector().accept(self)

    def visitTensor(self, ctx: Programmer.QXTensor):
        tmp_current_qafny_line_number = self.current_qafny_line_number
        self.current_qafny_line_number = ctx.line()
        if ctx.ID() is None:
            x = DXBind("tmp", SType("nat"), self.counter)
            self.counter += 1
        else:
            x = DXBind(str(ctx.ID()))

        tmp = []

        if self.t_ensures:
            r = subLocus(self.locus, self.outvarnums)
        else:
            r = subLocus(self.locus, self.varnums)

        if r is not None:
            l, qty, num = r
            for i in range(len(self.locus)):
                left = self.locus[i].crange().left().accept(self)
                right = self.locus[i].crange().right().accept(self)
                if not(isinstance(left, DXNum) and left.num() == 0):
                    right = DXBin('-', right, left, transformed_from=ctx)
                v = ctx.kets()[i][0].accept(self) if isinstance(ctx.kets()[i], list) else ctx.kets()[i].accept(self)
                if isinstance(qty, TyNor):
                    tmp+=[DXComp('==',DXCall('castBVInt', [DXBind(self.locus[i].location(), num = num)]), v, transformed_from=ctx)]
                    self.libFuns.add('castBVInt')
                elif isinstance(qty, TyHad):
                    tmp += [DXAll(x, DXLogic("==>", DXInRange(x, DXNum(0), right), DXComp("==", DXIndex(self.qvars[self.locus[i].location()], x), v)), transformed_from=ctx)]
                if not self.t_ensures:
                    if isinstance(qty, TyNor) and isinstance(v, DXNum) and v.num() == 0:
                        tmp += [DXAll(x, DXLogic("==>", DXInRange(x, DXNum(0), right), DXComp("==", DXIndex(self.qvars[self.locus[i].location()], x), v)), transformed_from=ctx)]
                    elif isinstance(qty, TyNor):
                        pass
                    else:
                        tmp += [DXAll(x, DXLogic("==>", DXInRange(x, DXNum(0), right), DXComp("==", DXIndex(self.qvars[i], x), v)), transformed_from=ctx)]
                #tmp += [DXAll(x, DXLogic("==>", DXInRange(x, DXNum(0), right), DXComp("==", DXIndex(self.qvars[i], x), v)))]

            if not self.t_ensures:
                loc, qty, num = subLocus(self.locus, self.varnums)
                for i in range(len(loc)):
                    subd = {}
                    v = ctx.kets()[i][0].accept(self) if isinstance(ctx.kets()[i], list) else ctx.kets()[i].accept(self)
                    subd['qty'] = qty
                    subd['val'] = v
                    subd['length'] = loc[i].crange().right().ID() if isinstance(loc[i].crange().right(), QXBind) else loc[i].crange().right().num()
                    toString = TargetToString()
                    self.initial_locus_data[loc[i].location() + loc[i].crange().left().accept(toString) + ',' + loc[i].crange().right().accept(toString)] = subd

        self.current_qafny_line_number = tmp_current_qafny_line_number
        return tmp


    def visitSum(self, ctx: Programmer.QXSum):
        tmp = []
        vars = [x for x in self.qvars if x.ID() != 'amp']
        tmp_current_qafny_line_number = self.current_qafny_line_number
        self.current_qafny_line_number = ctx.line()
        for i in range(len(vars)):
            v = ctx.kets()[i].accept(self)
            eq = DXComp("==",DXCall('castBVInt', [makeIndex(vars[i], ctx.sums())]),v, transformed_from=ctx)
            self.libFuns.add('castBVInt')
            for con in ctx.sums()[::-1]:
                x = DXBind(con.ID(), SType("nat"))
                arange = DXInRange(x,con.range().left().accept(self), con.range().right().accept(self), transformed_from=ctx)
                if con.condition():
                    arange = DXBin('&&', arange, con.condition().accept(self))
                eq = DXAll(x, DXLogic("==>",arange,eq), transformed_from=ctx)
            tmp += [eq]

        num = self.qvars[0].num()
        ampvar = makeIndex(DXBind("amp", SType("real"), num),ctx.sums())
        v = ctx.amp().accept(self)
        eq = DXComp("==", ampvar, v, transformed_from=ctx)
        for con in ctx.sums()[::-1]:
            x = DXBind(con.ID(), SType("nat"))
            arange = DXInRange(x, con.range().left().accept(self), con.range().right().accept(self), transforemd_from=ctx)
            if con.condition():
                    arange = DXBin('&&', arange, con.condition().accept(self))
            eq = DXAll(x, DXLogic("==>", arange, eq), transformed_from=ctx)

        if not self.t_ensures:
            loc, qty, num = subLocus(self.locus, self.varnums)
            ampl = None
            if isinstance(qty, TyEn):
                ampl = v
            for i in range(len(loc)):
                subd = {}
                v = ctx.kets()[i].accept(self)
                subd['qty'] = qty
                subd['val'] = v
                subd['length'] = loc[i].crange().right().ID() if isinstance(loc[i].crange().right(), QXBind) else loc[i].crange().right().num()
                subd['amp'] = ampl
                toString = TargetToString()
                self.initial_locus_data[loc[i].location() + loc[i].crange().left().accept(toString) + ',' + loc[i].crange().right().accept(toString)] = subd
        
        self.current_qafny_line_number = tmp_current_qafny_line_number
        return ([eq]+tmp)

    def visitLogic(self, ctx: Programmer.QXLogic):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXLogic(ctx.op(), left, right, transformed_from=ctx)


    def visitBool(self, ctx: Programmer.QXComp):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXComp(ctx.op(), left, right, transformed_from=ctx)

    def visitBoolLiteral(self, ctx: Programmer.QXBoolLiteral):
        return DXBoolValue(ctx.value())

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        loc, qty, vars = self.currLocus
        v = ctx.index().accept(self)
        #self.conStack += [EnFactor(('==', DXIndex(vars(ctx.ID()), v), DXNum(1)))]
        return DXIndex(vars.get(ctx.ID()), v)
            #return DXIndex(DXBind(ctx.ID(), None, n),ctx.index().accept(self), qafny_line_number=ctx.line_number())
        #return (None, DXIndex(DXBind(ctx.ID(), None, n),ctx.index().accept(self), qafny_line_number=ctx.line_number()))

    def visitCon(self, ctx: Programmer.QXCon):
        return super().visitCon(ctx)

    def buildLenEqAux(self, vs: [DXAExp]):
        res = None
        if len(vs) <= 1:
            return res
        left = vs[0]
        for i in range(1,len(vs)):
            right = vs[i]
            if res is None:
                res = DXComp('==', DXLength(left), DXLength(right))
            else:
                res = DXLogic('&&',res,DXComp('==', DXLength(left), DXLength(right)))
            left = right
        return res

    def buildLenEqAuxB(self, res: DXBool, x:DXAExp, m:int, countVars:[DXBind], tmpVars: [DXBind]):
        if not countVars:
            return res
        else:
            return DXAll(countVars[0], DXLogic('==>',
                                   DXLogic('&&', DXComp('<=', DXNum(0), countVars[0]),
                                           DXComp('<', countVars[0], DXLength(constructIndex(x,tmpVars[0:m])))),
                                               self.buildLenEqAuxB(res, x, m+1, countVars[1:],tmpVars)))


    def buildLenEqAuxA(self, res: DXBool, x:DXAExp, m:int, loopVars: [DXBind], countVars:[DXBind], tmpVars: [DXBind]):
        if not countVars:
            return res
        else:
            if m <= 1:
                return self.buildLenEqAuxB(res, x, 0, countVars, tmpVars)
            return DXAll(countVars[0], DXLogic('==>',
                                   DXLogic('&&', DXComp('<=', DXNum(0), countVars[0]),
                                           DXComp('<', countVars[0], (loopVars[0]))),
                                   self.buildLenEqAuxA(res,x,m-1,loopVars[1:], countVars[1:],tmpVars)))

    #n is the flag from en type
    def buildLenEq(self, vs:[DXAExp], n:int, m:int, loopVars: [DXBind], tmpVars: [DXBind]):
        newVars = tmpVars[0:n-1]
        news = []
        for value in vs:
            news += [constructIndex(value, newVars)]

        res = self.buildLenEqAux(news)

        return self.buildLenEqAuxA(res,vs[0],m, loopVars, tmpVars[0:n-1], tmpVars)


    def buildLoopCount(self, x:DXBind, y:DXAExp):
        return DXLogic('&&', DXComp('<=', DXNum(0), x), DXComp('<=', x, DXLength(y)))

    def buildBExpPredB(self, op:str, left: DXAExp, right: DXAExp, ind:DXIndex,
                      m:int, x:DXAExp, loopVars: [DXBind], tmpVars: [DXBind], countVars : [DXBind]):
        if not loopVars:
            return DXComp('==',DXBin(op, left, right), ind)
        else:
            return DXAll(tmpVars[0], DXLogic('==>', DXLogic('&&', DXComp('<=', DXNum(0), tmpVars[0]),
                                                            DXComp('<', tmpVars[0], DXLength(constructIndex(x,countVars[0:m])))),
                                      self.buildBExpPredB(op, DXIndex(left, tmpVars[0]), DXIndex(right, tmpVars[0]),
                                                          DXIndex(ind, tmpVars[0]), m+1, x, loopVars[1:], tmpVars[1:],countVars))
                   , transformed_from=left)


    def buildBExpPredA(self, op:str, left: DXAExp, right: DXAExp, ind:DXIndex,
                      m:int, x:DXAExp, loopVars: [DXBind], tmpVars: [DXBind], countVars : [DXBind]):
        if not loopVars:
            return DXComp('==',DXBin(op, left, right), ind)
        else:
            if m <= 1:
                return self.buildBExpPredB(op, left, right, ind, 0, x, loopVars, tmpVars, countVars)
            return DXAll(tmpVars[0], DXLogic('==>',
                                      DXLogic('&&', DXComp('<=', DXNum(0), tmpVars[0]), DXComp('<', tmpVars[0], loopVars[0])),
                                      self.buildBExpPredA(op, DXIndex(left, tmpVars[0]), DXIndex(right, tmpVars[0]),
                                                          DXIndex(ind, tmpVars[0]), m-1,x, loopVars[1:], tmpVars[1:],countVars))
                   , transformed_from=left)

    def buildWhileBExp(self, op:str, left: DXAExp, right: DXAExp, ind:DXIndex,
                       vars:dict, n:int,m:int, loopVars: [DXBind], tmpVars: [DXBind]):

        if n == m:
            return DXAssign([constructIndex(ind,loopVars)],
                            DXBin(op, constructIndex(left,loopVars), constructIndex(ind,loopVars)))

        x = list(vars.values())[0]

        invariants = [self.buildLoopCount(loopVars[m-1], constructIndex(x, tmpVars[0:m-1]))]
        invariants += [self.buildLenEq(vars.values(), n, m, loopVars, tmpVars)]
        invariants += [self.buildBExpPredA(op, left, right, ind, m, x, loopVars, tmpVars, tmpVars)]

        #loopCount = constructIndex(list(vars.values())[0], loopVars)
        #looping_var = DXBind('tmp', SType('nat'), self.counter)
        #self.counter += 1

        #invariants = [self.buildLoopCount(looping_var, loopCount)]

        #invariants += [self.buildLenEq(vars, tmpVars)]

        #loopVars += [looping_var]
        #tmpVars += [DXBind('tmp', SType('nat'), self.counter)]
        #self.counter += 1

        return DXWhile(DXComp('<',loopVars[m-1],DXLength(constructIndex(x, tmpVars[0:m-1]))),
                [self.buildWhileBExp(op, left, right, ind, vars, n, m+1, loopVars, tmpVars)],
                       invariants, transformed_from=left)

    def visitQComp(self, ctx: Programmer.QXQComp):

        lc = LocusCollector()
        lc.visit(ctx)
        loc, qty, vars = self.currLocus
        newVars = dict()
        newVars.update({x.location(): vars.get(x.location()) for x in lc.renv()})

        changeVar = UpdateVars(self.counter, vars)

        v1 = ctx.left().accept(self)
        changeVar.visit(v1)
        v2 = ctx.right().accept(self)
        changeVar.visit(v2)
        ind = ctx.index().accept(self)
        changeVar.visit(ind)

        self.currLocus = loc, qty, changeVar.vars()
        self.counter = changeVar.counter()

        loopVars = []
        tmpVars = []
        for i in range(qty.flag()):
            loopVars += [DXBind('step', SType('nat'), self.counter)]
            self.counter += 1
            tmpVars += [DXBind('step', SType('nat'), self.counter)]
            self.counter += 1

        self.conStack += [EnFactor(('==', ind, DXNum(1)))]
        return ([DXInit(x, transformed_from=ctx) for x in loopVars] +
                [self.buildWhileBExp(ctx.op(), v1, v2, ind, newVars, qty.flag(), 1, loopVars, tmpVars)])
        #this is not an index, need a way to refer to the gen id

        #result = [DXAssign(DXIndex(ctx.index().ID(), ctx.index().index()),
        #                   DXComp(ctx.op(), DXCall('castBVInt', v1, v2, qafny_line_number=self.current_qafny_line_number)))]

    def visitQNot(self, ctx: Programmer.QXQNot):
        if isinstance(ctx.next(), QXQNot):
            return ctx.next().next().accept(self)

        pred, index = ctx.next().accept(self)
        return (DXNot(pred, transformed_from=ctx), index)

    def visitAll(self, ctx: Programmer.QXAll):
        x = ctx.bind().accept(self)
        p = ctx.next().accept(self)
        return DXAll(x, p, transformed_from=ctx)

    def visitBin(self, ctx: Programmer.QXBin):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        if ctx.op() == '^' and isinstance(left, DXNum) and left.num() == 2:
            return DXCall('pow2',[ctx.right().accept(self)], transformed_from=ctx)


        if ctx.op() == '/' and isinstance(left, DXNum) and left.num() == 1:
            return DXBin(ctx.op(), DXNum(1.0), right, transformed_from=ctx)
        
        return DXBin(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), transformed_from=ctx)

    def visitIfExp(self, ctx: Programmer.QXIfExp):
        return DXIfExp(ctx.bexp().accept(self), ctx.left().accept(self), ctx.right().accept(self), transformed_from=ctx)

    def visitUni(self, ctx: Programmer.QXUni):
        if ctx.op() == 'sqrt':
            return DXCall('sqrt', [DXCast(SType('real'), ctx.next(), transformed_from=ctx)])
        return DXUni(ctx.op(), ctx.next().accept(self), transformed_from=ctx)

    def visitSingle(self, ctx: Programmer.QXSingle):
        v = subLocus(self.currLocus, self.varnums)
        if v is not None:
            loc,qty,num = v #num is the old dict for DxBinds

            if isinstance(qty, TyNor) and ctx.op() == "H":
                vs = compareLocus(self.currLocus, loc)
                if not vs:
                    # self.replaceType(num,TyHad())
                    self.removeLocus(loc)
                    newvars = self.upVarsType(TyHad(), num)
                    self.varnums += [(self.currLocus, TyHad(), newvars)]


                else:
                    self.removeLocus(loc)
                    self.counter += 1
                    newvars = self.upVarsType(TyHad(), num)
                    self.varnums = [(self.currLocus, TyHad(),newvars), (vs, TyNor(), num)] + self.varnums
                result = [DXInit(x, transformed_from=ctx) for x in newvars]
                result += [DXAssign(newvars.values(), DXCall("hadNorHad", num.values(),
                                                    transformed_from=ctx),
                                    transformed_from=ctx)]
                self.libFuns.add('hadNorHad')
                self.counter += 1
                return result

            #this part might need to change, and look from the code for sushen
            if isinstance(qty, TyEn)  and ctx.op() == "H":
                newvars = self.upVarsType(qty, num)
                flagNum = qty.flag().num()
                #newTy = TyEn(QXNum(flagNum + 1))
                # vs = compareLocus(ctx.locus(), loc)
                #self.replaceType(num, TyEn(QXNum(flagNum + 1)))
                tmr = []
                '''for rem in vs:
                    tmr += [DXAssign(makeVars([rem], TyEn(QXNum(flagNum + 1)), self.counter),
                                     DXCall("castBaseEn", makeVars([rem], qty, num)))]
                    self.libFuns.add('castBaseEn')
                    self.counter += 1'''
                result = tmr + [DXAssign(newvars.values(),
                                         DXCall(
                                             "En" + str(flagNum) + 'to' + "En" + str(flagNum + 1) + '_' + str(len(loc)),
                                             num.values(), transformed_from=ctx), True,
                                         transformed_from=ctx)]
                #TODO: the above only contain code for update type casting, how about the code to apply Had on a place and get phases?

                self.libFuns.add("En" + str(flagNum) + 'to' + "En" + str(flagNum + 1) + '_' + str(len(loc)))
                result += [DXCall('triggerSqrtMul', [], True, transformed_from=ctx)]
                result += [DXCall('ampeqtrigger', [], True, transformed_from=ctx)]
                result += [DXCall('pow2mul', [], True, transformed_from=ctx)]
                result += [DXCall('pow2sqrt', [], True, transformed_from=ctx)]

                self.libFuns.add('triggerSqrtMul')
                self.libFuns.add('ampeqtrigger')
                self.libFuns.add('pow2mul')
                self.libFuns.add('pow2sqrt')

                # self.removeLocus(num)
                self.counter += 1
                # self.varnums = [(loc,TyEn(QXNum(flagNum + 1)),self.counter)] + self.varnums
                return result

            if isinstance(qty, TyEn) and ctx.op() == "QFT":
                newvars = self.upVarsType(qty, num)
                flagNum = qty.flag().num()
                # vs = compareLocus(ctx.locus(), loc)
                #self.replaceType(num, TyEn(QXNum(flagNum + 1)))
                tmr = []

                #newvars = makeVars(loc, TyEn(QXNum(flagNum + 1)), self.counter)
                #modify the following to make it look like Had
                applicationids = []
                if isinstance(self.currLocus, list):
                    for _loc in self.currLocus:
                        if isinstance(_loc, QXQRange):
                            applicationids.append(_loc.location())
                        elif isinstance(_loc, str):
                            applicationids.append(_loc)
                elif isinstance(self.currLocus, QXQRange):
                    applicationids.append(self.currLocus.location())
                elif isinstance(self.currLocus, str):
                    applicationids.append(self.currLocus)
                application_newvars = [x for x in newvars if x.ID() in applicationids]
                other_newvars = [x for x in newvars if x.ID() not in applicationids and x.ID() != 'amp']
                amp_newvar = [x for x in newvars if x.ID() == 'amp']
                oldvars = makeVars(loc, qty, num)
                application_oldvars = [x for x in oldvars if x.ID() in applicationids]
                other_oldvars = [x for x in oldvars if x.ID() not in applicationids and x.ID() != 'amp']
                amp_oldvar = [x for x in oldvars if x.ID() == 'amp']
                result = tmr + [DXAssign(other_newvars + application_newvars + amp_newvar,
                                         DXCall(
                                             "QFT_En" + str(flagNum) + 'to' + "En" + str(flagNum + 1) + '_' + str(
                                                 len(loc)),
                                             other_oldvars + application_oldvars + amp_oldvar,
                                             transformed_from=ctx), True,
                                         transformed_from=ctx)]
                #modify the above

                self.libFuns.add("QFT_En" + str(flagNum) + 'to' + "En" + str(flagNum + 1) + '_' + str(len(loc)))
                result += [DXCall('triggerSqrtMul', [], True, transformed_from=ctx)]
                result += [DXCall('ampeqtrigger', [], True, transformed_from=ctx)]
                result += [DXCall('pow2mul', [], True, transformed_from=ctx)]
                result += [DXCall('pow2sqrt', [], True, transformed_from=ctx)]

                self.libFuns.add('triggerSqrtMul')
                self.libFuns.add('ampeqtrigger')
                self.libFuns.add('pow2mul')
                self.libFuns.add('pow2sqrt')

                # self.removeLocus(num)
                self.counter += 1
                # self.varnums = [(loc,TyEn(QXNum(flagNum + 1)),self.counter)] + self.varnums
                return result

        return None

    def buildWhileCondition(self, loopVars: [DXBind], st: [DXStmt], conditions: [StackFactor]):
        if not conditions:
            return st

        if isinstance(conditions[0], EnFactor):
            op, left, right = conditions[0].condition()
            return [DXIf(DXComp(op, constructIndex(left, loopVars), right, left.qafny_line_number()),
                         self.buildWhileCondition(loopVars, st, conditions[1:]), [])]
        return []

    def buildOraclePredB(self, v:DXComp, m:int, x:DXAExp, loopVars: [DXBind], tmpVars: [DXBind], countVars : [DXBind]):
        if not loopVars:
            return v
        else:
            return DXAll(tmpVars[0], DXLogic('==>', DXLogic('&&', DXComp('<=', DXNum(0), tmpVars[0]),
                                                            DXComp('<', tmpVars[0], DXLength(constructIndex(x,countVars[0:m])))),
                                      self.buildOraclePredB(v, m+1, x, loopVars[1:], tmpVars[1:],countVars))
                   , qafny_line_number=v.qafny_line_number())


    def buildOraclePredA(self, v:DXComp,m:int, x:DXAExp, loopVars: [DXBind], tmpVars: [DXBind], countVars : [DXBind]):
        if not loopVars:
            return v
        else:
            if m <= 1:
                return self.buildOraclePredB(v, 0, x, loopVars, tmpVars, countVars)
            return DXAll(tmpVars[0], DXLogic('==>',
                                      DXLogic('&&', DXComp('<=', DXNum(0), tmpVars[0]), DXComp('<', tmpVars[0], loopVars[0])),
                                      self.buildOraclePredA(v, m-1,x, loopVars[1:], tmpVars[1:],countVars))
                   , qafny_line_number=v.qafny_line_number())

    def buildWhileOracle(self, oldComps:[DXComp], comps:[DXComp], st: [DXStmt], vars:[DXAExp],
                         n:int,m:int, loopVars: [DXBind], tmpVars: [DXBind]):

        if n == m:
            return self.buildWhileCondition(loopVars, st, self.conStack)

        x = vars[0]

        invariants = [self.buildLoopCount(loopVars[m-1], constructIndex(x, tmpVars[0:m-1]))]
        invariants += [self.buildLenEq(vars, n, m, loopVars, tmpVars)]
        invariants += [self.buildOraclePredA(comp, m, x, loopVars, tmpVars, tmpVars) for comp in list(oldComps)]
        invariants += [self.buildOraclePredA(comp, m, x, loopVars, tmpVars, tmpVars) for comp in list(comps)]


        #loopCount = constructIndex(list(vars.values())[0], loopVars)
        #looping_var = DXBind('tmp', SType('nat'), self.counter)
        #self.counter += 1

        #invariants = [self.buildLoopCount(looping_var, loopCount)]

        #invariants += [self.buildLenEq(vars, tmpVars)]

        #loopVars += [looping_var]
        #tmpVars += [DXBind('tmp', SType('nat'), self.counter)]
        #self.counter += 1

        return DXWhile(DXComp('<',loopVars[m-1],DXLength(constructIndex(x, tmpVars[0:m-1]))),
                [self.buildWhileOracle(oldComps, comps, st, vars, n, m+1, loopVars, tmpVars)],
                       invariants, qafny_line_number=st[0].qafny_line_number())

    def visitOracle(self, ctx: Programmer.QXOracle):

        loca, vars = self.originLocus
        loc, qty, num = self.currLocus
        #if v is not None:
        #    loc, qty, num = v

        result = []
        if isinstance(qty, TyHad):
            tmpStmt = QXCast(TyEn(QXNum(1)), loc).accept(self)
            result.append(tmpStmt)
            qty = TyEn(QXNum(1))
            # num += 1
        loopVars = []
        tmpVars = []
        for i in range(qty.flag()):
            loopVars += [DXBind('step', SType('nat'), self.counter)]
            self.counter += 1
            tmpVars += [DXBind('step', SType('nat'), self.counter)]
            self.counter += 1


        #subst the variables in the current oracle state with indexing for stmts and predicates
        stmtSubsts = []
        for i in len(loca):
            stmtSubsts += [SubstDAExp(ctx.bindings()[i], constructIndex(loca[i],loopVars))]

        substs = []
        for i in len(loca):
            substs += [SubstDAExp(ctx.bindings()[i], loca[i])]


        #history predicate substitution
        oldSubsts = []
        for i in len(loca):
            oldSubsts += [SubstDAExp(loca[i].location(), vars.get(loca[i].location()))]

        indexSubsts = []
        for key in len(vars.keys()):
            indexSubsts += [SubstIndex(key, tmpVars)]


        newVars = dict()
        for elem in loca:
            newVars.update({elem.location: num.get(elem.location).newBind(self.counter)})
            self.counter += 1

        #substituion for stmts
        endStmts = []

        for i in len(loca):
            thisStmt = ctx.vectors()[i]
            for subst in stmtSubsts:
                thisStmt = subst.visit(thisStmt)
            endStmts += [DXAssign([constructIndex(newVars.get(loca[i].location()), loopVars)],
                                  thisStmt, transformed_from=ctx)]

        thisStmt = ctx.amp()
        for subst in stmtSubsts:
            thisStmt = subst.visit(thisStmt)

        endStmts = ([DXAssign([constructIndex(newVars.get('amp'), loopVars)],
                             thisStmt, transformed_from=ctx)] + endStmts)


        #substituion for predicates, including two parts, startPreds include history predicate for old value
        #endPreds include new predicates for new variable with respect to old value.
        startPreds = []

        for i in len(loca):
            thisPred = num.get(loca[i].location())
            for subst in oldSubsts:
                thisPred = subst.visit(thisPred)
            for subst in indexSubsts:
                thisPred = subst.visit(thisPred)
            startPreds += [DXComp('==',[constructIndex(num.get(loca[i].location()), tmpVars)],
                                  thisPred, transformed_from=ctx)]

        thisPred = num.get('amp')
        for subst in oldSubsts:
            thisPred = subst.visit(thisPred)
        for subst in indexSubsts:
            thisPred = subst.visit(thisPred)

        startPreds = ([DXComp('==',constructIndex(num.get('amp'), tmpVars),
                              thisPred, transformed_from=ctx)] + startPreds)


        #substitution to get the output predicates for the current oracle operation
        endPreds = []

        for i in len(loca):
            thisPred = ctx.vectors()[i]
            for subst in substs:
                thisPred = subst.visit(thisPred)
            for subst in oldSubsts:
                thisPred = subst.visit(thisPred)
            for subst in indexSubsts:
                thisPred = subst.visit(thisPred)
            endPreds += [DXAssign([constructIndex(newVars.get(loca[i].location()), tmpVars)],
                                  thisPred, transformed_from=ctx)]

        thisPred = ctx.amp()
        for subst in substs:
            thisPred = subst.visit(thisPred)
        for subst in oldSubsts:
            thisPred = subst.visit(thisPred)
        for subst in indexSubsts:
            thisPred = subst.visit(thisPred)

        endPreds = ([DXComp('==',constructIndex(newVars.get('amp'), tmpVars),
                              thisPred, transformed_from=ctx)] + endPreds)

        values = []
        for key in newVars.keys():
            values += [newVars.get(key), num.get(key)]

        self.libFuns.add('pow2')
        self.libFuns.add('omega')
        self.libFuns.add('sqrt')
        self.libFuns.add('omega0')

        res = ([DXInit(x, transformed_from=ctx) for x in loopVars] +
                [self.buildWhileOracle(startPreds,endPreds, endStmts, values, qty.flag(), 1, loopVars, tmpVars)])

        #update the stored map for history code
        for i in len(loca):
            newExp = ctx.vectors()[i]
            for subst in substs:
                newExp = subst.visit(newExp)
            for subst in oldSubsts:
                newExp = subst.visit(newExp)
            vars.update({loca[i].location(): newExp})

        return res

    def visitNum(self, ctx: Programmer.QXNum):
        return DXNum(ctx.num())

    def visitHad(self, ctx: Programmer.QXHad):
        # ensure omega is in our list of library functions
        self.libFuns.add('omega')
        if ctx.state() == "+":
            return DXCall("omega", [DXNum(0), DXNum(2)], transformed_from=ctx)
        else:
            return DXCall("omega", [DXNum(1), DXNum(2)], transformed_from=ctx)

    def visitQRange(self, ctx: Programmer.QXQRange):
        if ctx.range().right() == DXBin('+', ctx.range().left(), QXNum(1)):
            return QXQIndex(ctx.location(), ctx.range().left(), transformed_from=ctx).accept(self)

        v = QXBind(ctx.location(), transformed_from=ctx).accept(self)
        return v
    
    def visitVarState(self, ctx):
        pass


"""
        unchanged_range = []
        for i in loc:
            unchanged_range.append(i.location())

        name = "newFun" + str(self.counter)
        self.counter += 1

        newVars = self.upVarsSub(num)

        rpreds = []
        preds = []
        ids = [x.ID() if isinstance(x, QXBind) else x for x in ctx.bindings()]
        for vec in ctx.vectors():
            vec_d = vec.vector().accept(self)

            def lambda_check(lself, x):
                if isinstance(x, DXBin) and (x.op() == '/' or x.op() == '%') and isinstance(x.right(),
                                                                                            DXBind) and x.right().ID() not in ids:
                    lself.outputs.append(x.right())

            lamb_subst = SubstLambda(lambda_check)
            lamb_subst.visit(vec_d)
            for b in lamb_subst.outputs:
                rpreds += [DXRequires(DXComp('>', b, DXNum(0), qafny_line_number=ctx.line_number()),
                                      qafny_line_number=ctx.line_number())]

        preds += self.genPreds(loc, qty, num.values(), newVars.values(), ids, ctx.vectors(), ctx.amp(), unchanged_range)
        newConds = rpreds
        for pred in preds:
            newConds += [DXEnsures(pred, qafny_line_number=ctx.line_number())]

        varids = []

        def lambda_check(lself, x):
            if isinstance(x, DXBind):
                lself.outputs.append(x)

        for i in ctx.vectors():
            tmp = i.vector().accept(self)
            lamb_subst = SubstLambda(lambda_check)
            lamb_subst.visit(tmp)
            varids += [
                DXBind(t.ID(), self.kenv[self.fvar][0][t.ID()].accept(self),
                       qafny_line_number=ctx.line_number())
                for t in lamb_subst.outputs if t.ID() not in ids]

        lamb_subst = SubstLambda(lambda_check)
        lamb_subst.visit(ctx.amp().accept(self))
        varids += [
            DXBind(t.ID(), self.kenv[self.fvar][0][t.ID()].accept(self), qafny_line_number=ctx.line_number())
            for t in lamb_subst.outputs if t.ID() not in ids]

        cvars = num.values() + varids

        self.addFuns += [
            DXMethod(name, True, cvars, newVars.values(), newConds, [], qafny_line_number=ctx.line_number())]

        for i in newVars.values():
            result.append(DXInit(i, qafny_line_number=ctx.line_number()))

        result.append(DXAssign(newVars.values(), DXCall(name, cvars, qafny_line_number=ctx.line_number()),
                               qafny_line_number=ctx.line_number()))

        result.append(DXCall('omega0', [], True, qafny_line_number=ctx.line_number()))

        self.removeLocus(loc)
        self.varnums = [(loc, qty, newVars)] + self.varnums
        self.libFuns.add('pow2')
        self.libFuns.add('omega')
        self.libFuns.add('sqrt')
        self.libFuns.add('omega0')
        return result

"""


"""
        for loc, qty, num in self.varnums:
            for l in loc:
                if l.location() == ctx.index().ID():
                    n = num
        return (DXComp(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), qafny_line_number=ctx.line_number()),
                DXIndex(DXBind(ctx.index().ID(), num = n),ctx.index().accept(self), qafny_line_number=ctx.line_number()))


if isinstance(ctx.bexp(), QXQComp):

    if isinstance(ctx.bexp(), QXQRange):
        ifbexp = DXComp('==', DXCall('castBVInt', [loop_oldVars[bool_exp_id]]), DXNum(1),
                        qafny_line_number=self.current_qafny_line_number)
    elif isinstance(ctx.bexp(), QXQIndex):
        ifbexp = DXComp('==', DXIndex(loop_oldVars[bool_exp_id], bool_exp_index), DXNum(1),
                        qafny_line_number=self.current_qafny_line_number)
    else:
        if isinstance(ctx.bexp().left(), QXQRange):
            ifbexp = DXComp(ctx.bexp().op(), DXCall('castBVInt', [loop_oldVars[ctx.bexp().left().ID()]]),
                            ctx.bexp().right().accept(self), qafny_line_number=self.current_qafny_line_number)
        elif isinstance(ctx.bexp().right(), QXQRange):
            ifbexp = DXComp(ctx.bexp().op(), ctx.bexp().left().accept(self),
                            DXCall('castBVInt', [loop_oldVars[ctx.bexp().right().ID()]]),
                            qafny_line_number=self.current_qafny_line_number)
        self.libFuns.add('bool2BV1')
        result += [DXAssign([DXBind('res')], DXCall('bool2BV1', [ifbexp]), True,
                            qafny_line_number=self.current_qafny_line_number)]

if isinstance(ctx.bexp(), QXQIndex):
    lc = LocusCollector()
    lc.visit(ctx.bexp())
    bool_exp_id = lc.renv[0].location()
    bool_exp_index = ctx.bexp().index().accept(self)
    for stmt in ctx.stmts():
        lc.visit(stmt)
        if isinstance(stmt, QXQAssign) and isinstance(stmt.exp(), QXSingle) and stmt.exp().op() == 'H':
            upgrade_en = True

    tres, nLoc, nqty, nnum = self.mergeLocus(lc.renv)

elif isinstance(ctx.bexp(), QXQRange):
    lc = LocusCollector()
    lc.visit(ctx.bexp())
    bexp_locus_length = None
    for loc, qty, num in self.varnums:
        for l in loc:
            if l.location() == ctx.bexp().location():
                bexp_locus_length = l.crange().right()
    bool_exp_id = lc.renv[0].location()
    for stmt in ctx.stmts():
        lc.visit(stmt)
        if isinstance(stmt, QXQAssign) and isinstance(stmt.exp(), QXSingle) and stmt.exp().op() == 'H':
            upgrade_en = True

    if (isinstance(ctx.bexp().crange().left(), QXBind) and isinstance(ctx.bexp().crange().right(), QXBin)
            and isinstance(ctx.bexp().crange().right().right(),
                           QXNum) and ctx.bexp().crange().right().right().num() == 1):
        bool_exp_index = DXBind(ctx.bexp().crange().left().ID(), qafny_line_number=ctx.line_number())
    elif (isinstance(ctx.bexp().crange().right(), QXNum) and (isinstance(bexp_locus_length, QXBind)) or
          (isinstance(bexp_locus_length, QXBind) and bexp_locus_length.ID() != lc.renv[
              0].crange().right().ID())):
        if isinstance(ctx.bexp().crange().left(), QXNum):
            bool_exp_index = DXBind(ctx.bexp().crange().left().num(), qafny_line_number=ctx.line_number())
        else:
            bool_exp_index = DXBind(ctx.bexp().crange().left().ID(), qafny_line_number=ctx.line_number())
    else:
        is_qrange = True
    tres, nLoc, nqty, nnum = self.mergeLocus(lc.renv)

elif isinstance(ctx.bexp(), QXQComp):
    is_qcomp = True
    lc = LocusCollector()
    lc.visit(ctx.bexp())
    bool_exp_id = ctx.bexp().left().location() if isinstance(ctx.bexp().left(), QXQRange) else ctx.bexp().right().ID()
    bool_store_id = ctx.bexp().index().ID()
    for stmt in ctx.stmts():
        lc.visit(stmt)

        if isinstance(stmt, QXQAssign) and isinstance(stmt.exp(), QXSingle) and stmt.exp().op() == 'H':
            upgrade_en = True

    tres, nLoc, nqty, nnum = self.mergeLocus(lc.renv)

"""


"""

        if isinstance(ctx.bexp(), QXQComp):

            if isinstance(ctx.bexp(), QXQRange):
                ifbexp = DXComp('==', DXCall('castBVInt', [loop_oldVars[bool_exp_id]]), DXNum(1),
                                qafny_line_number=self.current_qafny_line_number)
            elif isinstance(ctx.bexp(), QXQIndex):
                ifbexp = DXComp('==', DXIndex(loop_oldVars[bool_exp_id], bool_exp_index), DXNum(1),
                                qafny_line_number=self.current_qafny_line_number)
            else:
                if isinstance(ctx.bexp().left(), QXQRange):
                    ifbexp = DXComp(ctx.bexp().op(), DXCall('castBVInt', [loop_oldVars[ctx.bexp().left().ID()]]),
                                    ctx.bexp().right().accept(self), qafny_line_number=self.current_qafny_line_number)
                elif isinstance(ctx.bexp().right(), QXQRange):
                    ifbexp = DXComp(ctx.bexp().op(), ctx.bexp().left().accept(self),
                                    DXCall('castBVInt', [loop_oldVars[ctx.bexp().right().ID()]]),
                                    qafny_line_number=self.current_qafny_line_number)
                self.libFuns.add('bool2BV1')
                result += [DXAssign([DXBind('res')], DXCall('bool2BV1', [ifbexp]), True,
                                    qafny_line_number=self.current_qafny_line_number)]

        if isinstance(ctx.bexp(), QXQIndex):
            lc = LocusCollector()
            lc.visit(ctx.bexp())
            bool_exp_id = lc.renv[0].location()
            bool_exp_index = ctx.bexp().index().accept(self)
            for stmt in ctx.stmts():
                lc.visit(stmt)
                if isinstance(stmt, QXQAssign) and isinstance(stmt.exp(), QXSingle) and stmt.exp().op() == 'H':
                    upgrade_en = True

            tres, nLoc, nqty, nnum = self.mergeLocus(lc.renv)

        elif isinstance(ctx.bexp(), QXQRange):
            lc = LocusCollector()
            lc.visit(ctx.bexp())
            bexp_locus_length = None
            for loc, qty, num in self.varnums:
                for l in loc:
                    if l.location() == ctx.bexp().location():
                        bexp_locus_length = l.crange().right()
            bool_exp_id = lc.renv[0].location()
            for stmt in ctx.stmts():
                lc.visit(stmt)
                if isinstance(stmt, QXQAssign) and isinstance(stmt.exp(), QXSingle) and stmt.exp().op() == 'H':
                    upgrade_en = True

            if (isinstance(ctx.bexp().crange().left(), QXBind) and isinstance(ctx.bexp().crange().right(), QXBin)
                    and isinstance(ctx.bexp().crange().right().right(),
                                   QXNum) and ctx.bexp().crange().right().right().num() == 1):
                bool_exp_index = DXBind(ctx.bexp().crange().left().ID(), qafny_line_number=ctx.line_number())
            elif (isinstance(ctx.bexp().crange().right(), QXNum) and (isinstance(bexp_locus_length, QXBind)) or
                  (isinstance(bexp_locus_length, QXBind) and bexp_locus_length.ID() != lc.renv[
                      0].crange().right().ID())):
                if isinstance(ctx.bexp().crange().left(), QXNum):
                    bool_exp_index = DXBind(ctx.bexp().crange().left().num(), qafny_line_number=ctx.line_number())
                else:
                    bool_exp_index = DXBind(ctx.bexp().crange().left().ID(), qafny_line_number=ctx.line_number())
            else:
                is_qrange = True
            tres, nLoc, nqty, nnum = self.mergeLocus(lc.renv)

        elif isinstance(ctx.bexp(), QXQComp):
            is_qcomp = True
            lc = LocusCollector()
            lc.visit(ctx.bexp())
            bool_exp_id = ctx.bexp().left().location() if isinstance(ctx.bexp().left(), QXQRange) else ctx.bexp().right().ID()
            bool_store_id = ctx.bexp().index().ID()
            for stmt in ctx.stmts():
                lc.visit(stmt)

                if isinstance(stmt, QXQAssign) and isinstance(stmt.exp(), QXSingle) and stmt.exp().op() == 'H':
                    upgrade_en = True

            tres, nLoc, nqty, nnum = self.mergeLocus(lc.renv)


        res += tres
        fNum = self.counter
        fqty = nqty

        self.counter += 1
        oldVars = makeVars(nLoc, nqty, nnum)
        # newVars = []

        # update en types
        if upgrade_en:
            fqty = TyEn(QXNum(nqty.flag().num() + 1))
            newVars = makeVars(nLoc, fqty, fNum)

            for i in range(len(self.varnums)):
                vloc, vqty, vnum = self.varnums[i]
                if compareLocus(vloc, nLoc):
                    self.varnums[i] = [vloc, fqty, fNum]
        else:
            newVars = makeVars(nLoc, fqty, fNum)

        # assign x as a new vars
        res += [DXAssign([x], DXList(), True, qafny_line_number=ctx.line_number()) for x in newVars]

        loop_oldVars = {x.ID(): x for x in oldVars}
        loop_newVars = {x.ID(): x for x in newVars}
        nLoc_dict = {x.location(): x for x in nLoc}

        result = []




        self.conStack += [ifbexp]

        #after the index has placed into stack, we loop to ctx.exp().accept(self) for next level




        elif isinstance(ctx.bexp(), QXQComp):
            is_qcomp = True
            lc = LocusCollector()
            lc.visit(ctx.bexp())
            bool_exp_id = ctx.bexp().left().location() if isinstance(ctx.bexp().left(), QXQRange) else ctx.bexp().right().ID()
            bool_store_id = ctx.bexp().index().ID()
            for stmt in ctx.stmts():
                lc.visit(stmt)

                if isinstance(stmt, QXQAssign) and isinstance(stmt.exp(), QXSingle) and stmt.exp().op() == 'H':
                    upgrade_en = True

            tres, nLoc, nqty, nnum = self.mergeLocus(lc.renv)

        #nLoc, nqty and nnum is for locus after merging but before had or lambda operation
        #fLoc, fqty and fNum is for locus after the had or lambda operation

        res += tres
        fNum = self.counter
        fqty = nqty

        self.counter += 1
        oldVars = makeVars(nLoc, nqty, nnum)
        #newVars = []

        #update en types
        if upgrade_en:
            fqty = TyEn(QXNum(nqty.flag().num() + 1))
            newVars = makeVars(nLoc, fqty, fNum)

            for i in range(len(self.varnums)):
                vloc, vqty, vnum = self.varnums[i]
                if compareLocus(vloc, nLoc):
                    self.varnums[i] = [vloc, fqty, fNum]
        else:
            newVars = makeVars(nLoc, fqty, fNum)


        #assign x as a new vars
        res += [DXAssign([x], DXList(), True, qafny_line_number=ctx.line_number()) for x in newVars]

        loop_oldVars = {x.ID() : x for x in oldVars}
        loop_newVars = {x.ID() : x for x in newVars}
        nLoc_dict = {x.location() : x for x in nLoc}

        res += [DXAssign([DXBind('tmp', None, 0)], DXNum(0), True, qafny_line_number=ctx.line_number())]


        loop_values = {x : loop_oldVars[x] for x in loop_oldVars}

        res.append(while_stmt)

        self.libFuns.add('powN')
        self.libFuns.add('powNTimesMod')
        self.libFuns.add('pow2add')
        self.libFuns.add('triggerSqrtMul')
        self.libFuns.add('pow2mul')
        self.libFuns.add('omega0')
        res.append(DXCall('powNTimesMod', [], True, qafny_line_number=self.current_qafny_line_number))
        res.append(DXCall('pow2add', [], True, qafny_line_number=self.current_qafny_line_number))
        res.append(DXCall('triggerSqrtMul', [], True, qafny_line_number=self.current_qafny_line_number))
        res.append(DXCall('pow2mul', [], True, qafny_line_number=self.current_qafny_line_number))
        res.append(DXCall('omega0', [], True, qafny_line_number=self.current_qafny_line_number))

        for i in range(len(self.varnums)):
            loc, qty, num = self.varnums[i]
            if compareLocus(loc, nLoc) or compareLocus(loc, nLoc) == []:
                self.varnums[i] = [loc, fqty, fNum]

        for i in range(len(self.outvarnums)):
            loc, qty, num = self.outvarnums[i]
            for vloc, vqty, vnum in self.varnums:
                if compareLocus(vloc, loc) or compareLocus(vloc, loc) == []:
                    self.outvarnums[i] = [loc, vqty, vnum]

        return res

"""

"""
        def transfer_had_lambda(stmts : list, qstmt : QXStmt, qif : QXIf, loop_oldVars : dict,
                                loop_newVars : dict, tmpvars : dict, is_qcomp : bool, bool_exp_id : str,
                                bool_exp_index : QXAExp, bool_store_id : str, inv_dict : dict, loop_values : dict, loop_num : int, is_qrange : bool):

            hadamard_flag = False
            if isinstance(qstmt, QXQAssign) and isinstance(qstmt.exp(), QXSingle) and qstmt.exp().op() == 'H':
                hadamard_flag = True

            application_range_id = qstmt.locus()[0].location()
            application_range_old_var = [loop_oldVars[x] for x in loop_oldVars if x == application_range_id][0]
            if hadamard_flag:

                if is_qcomp:
                    if isinstance(qif.bexp().left(), QXQRange):
                        ifbexp = DXComp(qif.bexp().op(), DXCall('castBVInt', [loop_oldVars[qif.bexp().left().location()]]), qif.bexp().right().accept(self), qafny_line_number=self.current_qafny_line_number)
                    elif isinstance(qif.bexp().right(), QXQRange):
                        ifbexp = DXComp(qif.bexp().op(),qif.bexp().left().accept(self) , DXCall('castBVInt', [loop_oldVars[qif.bexp().right().ID()]]), qafny_line_number=self.current_qafny_line_number)
                    self.libFuns.add('bool2BV1')
                    stmts += [DXAssign([DXBind('res')], DXCall('bool2BV1', [ifbexp]), True, qafny_line_number=self.current_qafny_line_number)]
                    stmts += [DXAssign([tmpvars[bool_store_id]], DXCall('duplicateSeq', [DXBind('res'), DXCall('pow2', [DXLength(application_range_old_var)])]), True, qafny_line_number=self.current_qafny_line_number)]
                    stmts += [DXAssign([tmpvars[x]], DXCall('duplicateSeq', [loop_oldVars[x], DXCall('pow2', [DXLength(application_range_old_var)])]), True, qafny_line_number=self.current_qafny_line_number) for x in tmpvars if x != 'amp' and x != bool_store_id]
                    loop_values[bool_store_id] = DXNum(1)
                else:
                    if is_qrange:
                        ifbexp =  DXComp('==', DXCall('castBVInt',[loop_oldVars[bool_exp_id]]), DXNum(1), qafny_line_number=self.current_qafny_line_number)
                    else:
                        ifbexp = DXComp('==', DXIndex(loop_oldVars[bool_exp_id], bool_exp_index), DXNum(1), qafny_line_number=self.current_qafny_line_number)
                    stmts += [DXAssign([tmpvars[x]], DXCall('duplicateSeq', [loop_oldVars[x], DXCall('pow2', [DXLength(application_range_old_var)])]), True, qafny_line_number=self.current_qafny_line_number) for x in tmpvars if x != 'amp']

                self.libFuns.add('duplicateSeq')
                self.libFuns.add('duplicateAmp')

                stmts += [DXAssign([tmpvars['amp']], DXCall('duplicateAmp', [loop_oldVars['amp'], DXCall('pow2', [DXLength(application_range_old_var)])]), True, qafny_line_number=self.current_qafny_line_number)]

                self.libFuns.add('partialcastEn1toEn2')
                self.libFuns.add('ampMul')

                ifstmts = [DXAssign([tmpvars[application_range_id]], DXCall('partialcastEn1toEn2', [application_range_old_var]), qafny_line_number=self.current_qafny_line_number)]
                ifstmts += [DXAssign([tmpvars['amp']], DXCall('ampMul', [tmpvars['amp'], DXCall('pow2', [DXLength(application_range_old_var)]), application_range_old_var]), qafny_line_number=self.current_qafny_line_number)]
                stmts += [DXIf(ifbexp, ifstmts, [])]

                loop_values[application_range_id] = DXBind('tmp', SType('nat'), self.counter + nqty.flag().num() - loop_num)
                loop_values['amp'] = DXBin('*', DXBin('/', DXNum(1.0), DXCall('sqrt', [DXCast(SType('real'),DXCall('pow2', [DXLength(loop_newVars[application_range_id])]))])), DXCall('omega',[DXCall('castBVInt', [loop_oldVars[application_range_id]]), DXNum(2)]), qafny_line_number=self.current_qafny_line_number)

            else:
                if is_qcomp:
                    if isinstance(qif.bexp().left(), QXQRange):
                        ifbexp = DXComp(qif.bexp().op(), DXCall('castBVInt', [loop_oldVars[qif.bexp().left().ID()]]), qif.bexp().right().accept(self), qafny_line_number=self.current_qafny_line_number)
                    elif isinstance(qif.bexp().right(), QXQRange):
                        ifbexp = DXComp(qif.bexp().op(),qif.bexp().left().accept(self) , DXCall('castBVInt', [loop_oldVars[qif.bexp().right().ID()]]), qafny_line_number=self.current_qafny_line_number)
                    self.libFuns.add('bool2BV1')
                    stmts += [DXAssign([DXBind('res')], DXCall('bool2BV1', [ifbexp]), True, qafny_line_number=self.current_qafny_line_number)]
                else:
                    if is_qrange:
                        ifbexp = DXComp('==', DXCall('castBVInt', [loop_oldVars[bool_exp_id]]), DXNum(1), qafny_line_number=self.current_qafny_line_number)
                    else:
                        ifbexp = DXComp('==', DXIndex(loop_oldVars[bool_exp_id], bool_exp_index), DXNum(1), qafny_line_number=self.current_qafny_line_number)


                lambda_fn_name = 'qif_lambda' + str(self.counter)

                application_locus = []
                application_range_old_vars = []
                for tl in qstmt.locus():
                    application_locus += [x for x in nLoc if x.location() == tl.location()]
                    application_range_old_vars += [loop_oldVars[x] for x in loop_oldVars if x == tl.location()]

                lambda_op = qstmt
                lambda_bindings = [x.ID() if isinstance(x, QXBind) else x for x in lambda_op.exp().bindings()]

                for x in loop_newVars:
                    if x != application_range_id and x != 'amp':
                        inv_dict[x] = [self.genAllSpec(DXBind('tmp', None, self.counter, qafny_line_number=self.current_qafny_line_number), loop_newVars[x], loop_oldVars[x], False)]

                application_range_old_num = application_range_old_vars[0]
                while isinstance(application_range_old_num, DXIndex):
                    application_range_old_num = application_range_old_num.bind()


                lambda_preds = self.genPreds(application_locus, TyNor(), 1, 2, lambda_bindings, lambda_op.exp().vectors(), lambda_op.exp().amp(), [])

                ge0 = []
                for vec in lambda_op.exp().vectors():
                    r = vec.accept(self)

                    def identify_division_zero_lamda(lself, x):
                        if isinstance(x, DXBin) and r.op() == '%' and isinstance(r.right(), DXBind):
                            for i in lself.outputs:
                                if i == r.right():
                                    return
                            lself.outputs.append(r.right())

                    subst_lamb = SubstLambda(identify_division_zero_lamda)
                    subst_lamb.visit(r)
                    ge0 += subst_lamb.outputs

                tmpSubs = []
                valSubst = []

                for i in range(len(lambda_bindings)):
                    subst = SubstDAExp(lambda_bindings[i], loop_oldVars[lambda_op.locus()[i].location()])
                    tmpSubs += [subst]
                    valSubst = SubstDAExp(lambda_bindings[i], loop_values[lambda_op.locus()[i].location()])

                    def subsfunc(lself, x):
                        if isinstance(x, DXBind) and isinstance(x.type(), SeqType):
                            return DXCall('castBVInt', [x])
                        elif isinstance(x, DXIndex):
                            tmp = subsfunc(x.bind())
                            if tmp:
                                return DXCall('castBVInt', [x])

                    lambSubst = SubstLambda(subsfunc)
                    substres = valSubst.visit(lambda_op.exp().vectors()[i].accept(self))
                    loop_values[lambda_op.locus()[i].location()] = lambSubst.visit(substres)

                newp = lambda_op.exp().amp().accept(self)

                for esub in tmpSubs:
                    newp = esub.visit(newp)

                loop_values['amp'] = newp
                newp = DXBin('*', DXBind('amp1'), newp, qafny_line_number=self.current_qafny_line_number)


                lambda_preds += [self.genAllSpec(DXBind('tmp', SType('nat')), DXBind('amp2'), newp, True)]

                newConds = []

                for i in ge0:
                    newConds += [DXRequires(DXLogic('>', i, DXNum(0)), qafny_line_number=self.current_qafny_line_number)]

                for i in lambda_preds:
                    newConds += [DXEnsures(i, qafny_line_number=self.current_qafny_line_number)]

                ic = BindingCollector()
                varids = []
                for i in lambda_op.exp().vectors():
                    tmp = [x.accept(self) for x in i.vector().accept(ic) if x.ID() not in lambda_bindings]
                    for j in tmp:
                        if j.type() is None:
                            if j.ID() in self.kenv[self.fvar][0]:
                                varids.append(DXBind(j.ID(), self.kenv[self.fvar][0][j.ID()].accept(self), qafny_line_number=self.current_qafny_line_number))
                        else:
                            varids.append(j)

                cvars = application_range_old_vars + [loop_oldVars['amp']] + varids
                newConds += [DXEnsures(DXComp('==', DXLength(DXBind(x.location() + '1', SeqType(SType('bv1')))), DXLength(DXBind(x.location() + '2', SeqType(SType('bv1'))))), qafny_line_number=self.current_qafny_line_number) for x in application_locus]
                self.addFuns += [DXMethod(lambda_fn_name, True, [DXBind(x.location() + '1', SeqType(SType('bv1'))) for x in application_locus] + [DXBind('amp1', SType('real'))] + varids, [DXBind(x.location() + '2', SeqType(SType('bv1'))) for x in application_locus]+ [DXBind('amp2', SType('real'))], newConds, [], qafny_line_number=self.current_qafny_line_number)]

                stmts += [DXInit(tmpvars[x], qafny_line_number=self.current_qafny_line_number) for x in tmpvars]
                if_stmts = [DXAssign([tmpvars[x.location()] for x in application_locus]+[tmpvars['amp']], DXCall(lambda_fn_name, cvars), qafny_line_number=self.current_qafny_line_number)]

                for x in tmpvars:
                    found = False
                    if x == 'amp':
                        continue
                    for al in application_locus:
                        if x == al.location():
                            found = True
                            break
                    if not found:
                        if_stmts += [DXAssign([tmpvars[x]], loop_oldVars[x], qafny_line_number=self.current_qafny_line_number)]

                else_stmts = [DXAssign([tmpvars[x]], loop_oldVars[x], qafny_line_number=self.current_qafny_line_number) for x in tmpvars]
                if_block  = DXIf(ifbexp, if_stmts, else_stmts, qafny_line_number=self.current_qafny_line_number)

                stmts.append(if_block)


                stmts.append(DXCall('omega0', [], True))
                self.libFuns.add('omega0')





        def buildWhile(looping_var, wctx, num, loop_oldVars, loop_newVars, nLoc, nqty, nnum, fqty,
                       is_qcomp, bool_exp_id, bool_exp_index, bool_store_id, is_sub_loop, if_bexp_vals, loop_values, is_qrange):

            stmts = []

            loop_oldVars = {x : DXIndex(loop_oldVars[x], looping_var, qafny_line_number=self.current_qafny_line_number) for x in loop_oldVars}

            bool_exp_old_var = [loop_oldVars[x].bind() for x in loop_oldVars if x == bool_exp_id][0]

            while_predicate = DXComp('<', looping_var, DXLength(bool_exp_old_var), qafny_line_number=self.current_qafny_line_number)

            invariants = []

            invariants += [DXLogic('<=', DXLogic('<=', DXNum(0), looping_var), DXLength(loop_oldVars[x].bind()), qafny_line_number=self.current_qafny_line_number) for x in loop_oldVars] 
            invariants += [DXLogic('==', DXLength(loop_newVars[x]), looping_var, qafny_line_number=self.current_qafny_line_number) for x in loop_newVars]

            #inner most while loop
            is_inner_loop = num == nqty.flag().num() - 1
            is_outer_loop = num + 1 < nqty.flag().num()           
            if is_inner_loop or is_sub_loop:
                hadamard_exist_flag = False
                sub_loop_append = '1' if is_sub_loop else ''
                tmp_vars = {x : DXBind('tmp_' + x + sub_loop_append, SeqType(SType('bv1'))) for x in loop_oldVars if x != 'amp'}
                tmp_vars['amp'] = DXBind('tmp_amp' + sub_loop_append, SType('real'))
                inv_dict = {x : [] for x in loop_oldVars}
                hadamard_id_list = []
                tmp_current_qafny_line_number = self.current_qafny_line_number
                for qstmt in wctx.stmts():
                    self.current_qafny_line_number = qstmt.line_number()
                    if (isinstance(qstmt, QXQAssign) and isinstance(qstmt.exp(), QXSingle) and qstmt.exp().op() == 'H'):
                        hadamard_exist_flag = True
                        hadamard_id_list.append(qstmt.locus()[0].location())
                    if (isinstance(qstmt, QXQAssign) and isinstance(qstmt.exp(), QXOracle)) or (isinstance(qstmt, QXQAssign) and isinstance(qstmt.exp(), QXSingle) and qstmt.exp().op() == 'H'):
                        transfer_had_lambda(stmts, qstmt, wctx, loop_oldVars, loop_newVars, tmp_vars, is_qcomp, bool_exp_id, bool_exp_index, bool_store_id, inv_dict, loop_values, num, is_qrange)

                    elif isinstance(qstmt, QXIf):
                        is_sub_qcomp = False
                        is_sub_qrange = False
                        sub_bool_exp_id = ''
                        sub_bool_store_id = ''
                        sub_bool_exp_index = None
                        if isinstance(qstmt.bexp(), QXQComp):
                            is_sub_qcomp = True
                            sub_bool_exp_id = qstmt.bexp().left().ID() if isinstance(qstmt.bexp().left(), QXQRange) else qstmt.bexp().right().ID()
                            sub_bool_store_id = qstmt.bexp().index().ID()
                        else:
                            lc = LocusCollector()
                            lc.visit(qstmt.bexp())
                            sub_bool_exp_id = lc.renv[0].location()
                            if isinstance(qstmt.bexp(), QXQIndex):
                                sub_bool_exp_index = qstmt.bexp().index().accept(self)
                            elif isinstance(qstmt.bexp(), QXQRange):
                                sub_bexp_locus_length = None
                                for loc, _qty, _n in self.varnums:
                                    for l in loc:
                                        if l.location() == qstmt.bexp().location():
                                            sub_bexp_locus_length = l.crange().right()

                                if isinstance(qstmt.bexp().crange().left(), QXBind) and isinstance(qstmt.bexp().crange().right(), QXBin) and isinstance(qstmt.bexp().crange().right().right(), QXNum) and qstmt.bexp().crange().right().right().num() == 1:
                                    sub_bool_exp_index = DXBind(qstmt.bexp().crange().left().ID())
                                elif (isinstance(qstmt.bexp().crange().right(), QXNum) and (isinstance(sub_bexp_locus_length, QXBind))  or  
                                    (isinstance(sub_bexp_locus_length, QXBind) and sub_bexp_locus_length.ID() !=  lc.renv[0].crange().right().ID())):
                                    sub_bool_exp_index = DXBind(qstmt.bexp().crange().left().num()) if isinstance(qstmt.bexp().crange().left(), QXNum) else DXBind(qstmt.bexp().crange().left().ID())
                                else:
                                    is_sub_qrange = True

                        if_bexp_vals.append(sub_bool_exp_id)

                        if hadamard_exist_flag:
                            sub_loop_newVars = {x : DXBind('tmp_' + str(num + 1) + x, SeqType(SeqType(SType('bv1'))), num + 1) for x in tmp_vars if x != 'amp'}
                            sub_loop_newVars['amp'] = DXBind('tmp_' + str(num + 1) + 'amp', SeqType(SType('real')), num + 1)
                            sub_stmts = []
                            sub_stmts += [DXAssign([sub_loop_newVars[x]], DXList(), True, qafny_line_number=self.current_qafny_line_number) for x in sub_loop_newVars]
                            next_looping_var = DXBind('tmp_sub', None, num)
                            sub_stmts += [DXAssign([next_looping_var], DXNum(0), True, qafny_line_number=self.current_qafny_line_number)]


                            sub_loop_values = {x : tmp_vars[x] for x in tmp_vars}

                            sub_stmts += [buildWhile(next_looping_var, qstmt, 0, tmp_vars, sub_loop_newVars, nLoc, TyEn(QXNum(1)), nnum, TyEn(QXNum(1)), is_sub_qcomp, sub_bool_exp_id, sub_bool_exp_index, sub_bool_store_id, True, if_bexp_vals, sub_loop_values, is_sub_qrange)]
                            sub_stmts += [DXAssign([tmp_vars[x]], sub_loop_newVars[x], qafny_line_number=self.current_qafny_line_number) for x in sub_loop_newVars]

                            for lpv in sub_loop_values:
                                if lpv == 'amp':
                                    continue
                                eqv = EqualityVisitor()
                                ch_flag = not eqv.visit(sub_loop_values[lpv], tmp_vars[lpv])
                                if ch_flag:
                                    dxifbexp = None
                                    if is_sub_qcomp:
                                        if isinstance(qstmt.bexp().left(), QXQRange):
                                            dxifbexp = DXComp(qstmt.bexp().op(), tmp_vars[sub_bool_exp_id], qstmt.bexp().right().accept(self), qafny_line_number=self.current_qafny_line_number)
                                        else:
                                            dxifbexp = DXComp(qstmt.bexp().op(), qstmt.bexp().left().accept(self), tmp_vars[sub_bool_exp_id], qafny_line_number=self.current_qafny_line_number)
                                    elif is_sub_qrange:
                                        lc = LocusCollector()
                                        lc.visit(qstmt.bexp())
                                        dxifbexp = DXComp('==', DXCall('castBVInt', [tmp_vars[sub_bool_exp_id]]), DXNum(1), qafny_line_number=self.current_qafny_line_number)
                                    else:
                                        lc = LocusCollector()
                                        lc.visit(qstmt.bexp())
                                        dxifbexp = DXComp('==', DXCall('ketIndex',[tmp_vars[sub_bool_exp_id], sub_bool_exp_index]), DXNum(1), qafny_line_number=self.current_qafny_line_number)

                                    elseval = DXCall('castBVInt',[tmp_vars[lpv]], qafny_line_number=self.current_qafny_line_number) if isinstance(tmp_vars[lpv].type(), SeqType) else tmp_vars[lpv]
                                    sub_loop_values[lpv] = DXIfExp(dxifbexp, sub_loop_values[lpv], elseval)

                                    tmp_val = sub_loop_values[lpv]
                                    for lpv1 in tmp_vars:
                                        #revert the above lambda to its previous state for the rest of the loop
                                        def lambda_replace(lself, x):
                                            if isinstance(x, DXBind):
                                                if x.ID() == self.getBindFromIndex(tmp_vars[lpv1]).ID():
                                                    if self.getBindFromIndex(loop_oldVars[lpv1]).ID() in hadamard_id_list:
                                                        return loop_newVars[lpv1]
                                                    return loop_oldVars[lpv1]


                                        lamb_subst = SubstLambda(lambda_replace)
                                        if lpv != 'amp':
                                            tmp_val = lamb_subst.visit(tmp_val)
                                        else:
                                            if ch_flag:
                                                tp_amp_v = lamb_subst.visit(sub_loop_values[lpv])
                                                tmp_val = DXBin('*',loop_newVars[lpv], tp_amp_v, qafny_line_number=self.current_qafny_line_number)

                                    loop_values[lpv] = tmp_val

                            for fstmt in stmts:
                                if isinstance(fstmt, DXIf):
                                    fstmt.left().extend(sub_stmts)

                            if_bexp_vals.remove(sub_bool_exp_id)

                self.current_qafny_line_number = tmp_current_qafny_line_number
                #Invariant generation
                invnum = self.counter
                newvar = self.getBindFromIndex(loop_oldVars[bool_exp_id])
                newvar = DXBind(newvar.ID(), SType('bv1'), newvar.num())
                inv_old_var = {x : self.getBindFromIndex(loop_oldVars[x]) for x in loop_oldVars}

                inv_new_var = {loop_newVars[x].ID() : DXBind(loop_newVars[x].ID(), SType('bv1'), loop_newVars[x].num()) for x in loop_newVars} #only for amp inv generation


                for ix in range(nqty.flag().num()):
                    if ix < num:
                        newvar = DXIndex(newvar, DXBind('tmp',None,ix), qafny_line_number=self.current_qafny_line_number) 
                        inv_old_var = {x : DXIndex(inv_old_var[x], DXBind('tmp',None,ix), qafny_line_number=self.current_qafny_line_number) for x in inv_old_var}
                    else:
                        newvar = DXIndex(newvar, DXBind('tmp',None,invnum), qafny_line_number=self.current_qafny_line_number)
                        inv_old_var = {x : DXIndex(inv_old_var[x], DXBind('tmp',None,invnum), qafny_line_number=self.current_qafny_line_number) for x in inv_old_var}
                        invnum += 1


                for ix in range(1 + int(hadamard_exist_flag)):
                    inv_new_var = {x : DXIndex(inv_new_var[x], DXBind('tmp', None, self.counter + ix), qafny_line_number=self.current_qafny_line_number) for x in inv_new_var}

                ifbexp_inv = None
                if is_qcomp:
                    if isinstance(wctx.bexp().left(), QXQRange):
                        ifbexp_inv = DXComp(wctx.bexp().op(), DXCall('castBVInt', [newvar]), wctx.bexp().right().accept(self), qafny_line_number=self.current_qafny_line_number)
                    elif isinstance(wctx.bexp().right(), QXQRange):
                        ifbexp_inv = DXComp(wctx.bexp().op(),wctx.bexp().left().accept(self) , DXCall('castBVInt', [newvar]), qafny_line_number=self.current_qafny_line_number)
                else:
                    if is_qrange:
                        ifbexp_inv = DXComp('==', DXCall('castBVInt',[newvar]), DXNum(1), qafny_line_number=self.current_qafny_line_number)
                    else:
                        ifbexp_inv = DXComp('==', DXIndex(newvar, bool_exp_index), DXNum(1), qafny_line_number=self.current_qafny_line_number)

                #generation of length equality invariants for new variables eg. forall k :: 0 <= k < |p7| ==> |p7[k]| == pow2(n)
                st = SType('bv1')
                for inv_l in range(fqty.flag().num() - num):
                    st = SeqType(st)
                    tlvars = {dc : self.getBindFromIndex(loop_newVars[dc]) for dc in loop_newVars}
                    tlvars_1 = {dc : self.createIndexFromType(tlvars[dc], st, DXBind('tmp', SType('nat'), self.counter)) for dc in tlvars}
                    if inv_l + 1 < fqty.flag().num() - num:
                        nLoc_index = inv_l + 1 + num
                        rlen = nLoc[nLoc_index].crange().right().accept(self) if isinstance(nLoc[nLoc_index].crange().left(), QXNum) and nLoc[nLoc_index].crange().left().num() == 0 else DXBin('-', nLoc[nLoc_index].crange().right().accept(self), nLoc[nLoc_index].crange().left().accept(self))
                        rlen = DXCall('pow2', [rlen])
                        invariants += [self.genAllSpec_Simple(DXBind('tmp', None, self.counter), tlvars[x], st, DXComp('==', DXLength(tlvars_1[x]), rlen)) for x in tlvars]
                    else:
                        invariants += [self.genAllSpec_Simple(DXBind('tmp', None, self.counter), tlvars[x], st, DXComp('==', DXLength(tlvars_1[x]), (nLoc_dict[x].crange().right().accept(self) if isinstance(nLoc_dict[x].crange().left(), QXNum) and nLoc_dict[x].crange().left().num() == 0 else  DXBin('-', nLoc_dict[x].crange().right().accept(self), nLoc_dict[x].crange().left().accept(self))))) for x in tlvars if x != 'amp']


                for i in inv_dict:
                    rval = loop_values[i]
                    oldval = inv_old_var[i]

                    #to correct the indexing of the oldval based on loop level eg. p1 to p1[tmp8][tmp9]
                    def lambda_replace(lself, x):
                        if isinstance(x, DXIndex) or isinstance(x, DXBind):
                            v1 = self.getBindFromIndex(x)
                            v2 = self.getBindFromIndex(oldval)

                            if v1.ID() == v2.ID():
                                if v1.num() and v2.num() and v1.num() == v2.num():
                                    return oldval
                                elif not v1.num():
                                    return oldval


                    lamb_subst = SubstLambda(lambda_replace)
                    rval = lamb_subst.visit(rval)

                    if i != 'amp':
                        for ln_var in loop_newVars:
                            #to get the indexing right for the newvars
                            def lambda_subst(lself, x):
                                if isinstance(x, DXBind) and x.ID() == loop_newVars[ln_var].ID():
                                    if x.num() and loop_newVars[ln_var].num():
                                        if x.num() == loop_newVars[ln_var].num(): 
                                            return inv_new_var[x.ID()]
                                    else:
                                        return inv_new_var[x.ID()]
                            lamb_subst = SubstLambda(lambda_subst)
                            rval = lamb_subst.visit(rval)

                        rval = DXCall('castBVInt', [rval]) if (isinstance(rval, DXIndex) or (isinstance(rval, DXBind) and isinstance(rval.type(), SeqType))) else rval
                        oldval = DXCall('castBVInt', [oldval]) if (isinstance(oldval, DXIndex) or (isinstance(oldval, DXBind) and isinstance(oldval.type(), SeqType))) else oldval

                    else:
                        rval = loop_values['amp']

                        for ln_var in loop_newVars:
                            #in invariants we have 1.0/sqrt(pow2(x)), below code will change to 1.0/sqrt(pow2(x[tmp1][tmp2]))
                            def lambda_subst(lself, x):
                                if isinstance(x, DXBind) and x.ID() == loop_newVars[ln_var].ID():
                                    if x.num() and loop_newVars[ln_var].num():
                                        if x.num() == loop_newVars[ln_var].num(): 
                                            return inv_new_var[x.ID()]
                                    else:
                                        return inv_new_var[x.ID()]

                            #in invariants we have omega(castBVInt(x)) with x having incorrect indexing, following code will correct it
                            def lambda_subst1(lself, x):
                                if isinstance(x, DXIndex):
                                    tmp_l1 = x
                                    while not isinstance(tmp_l1, DXBind):
                                        tmp_l1 = tmp_l1.bind()

                                    tmp_inv_ov = inv_old_var[ln_var]
                                    while not isinstance(tmp_inv_ov, DXBind):
                                        tmp_inv_ov = tmp_inv_ov.bind()

                                    if tmp_l1.ID() == tmp_inv_ov.ID() and tmp_l1.num() and tmp_l1.num() == tmp_inv_ov.num():
                                        return inv_old_var[ln_var]

                            lamb_subst = SubstLambda(lambda_subst)
                            rval = lamb_subst.visit(rval)

                            lamb_subst = SubstLambda(lambda_subst1)
                            rval = lamb_subst.visit(rval)

                        rval = DXBin('*', oldval, rval, qafny_line_number=self.current_qafny_line_number)

                    inv_dict[i] = self.genAllSpec(DXBind('tmp', None, self.counter), loop_newVars[i], DXIfExp(ifbexp_inv, rval, oldval) if loop_values[i] != loop_oldVars[i] else oldval, i == 'amp')
                    invariants += [inv_dict[i]]



                for ifbv in if_bexp_vals:
                    if_bexp_new_var = self.getBindFromIndex(loop_newVars[ifbv])
                    tmpforallvar = DXBind('tmp', SType('nat'), self.counter)
                    if_bexp_new_var_indexed = self.createIndexFromType(if_bexp_new_var, if_bexp_new_var.type().type(), tmpforallvar)
                    if_bexp_old_var_indexed = inv_old_var[ifbv]
                    samebitexp = DXCall('samebit', [if_bexp_new_var_indexed, if_bexp_old_var_indexed, DXLength(if_bexp_old_var_indexed)], qafny_line_number=self.current_qafny_line_number)


                    invariants += [self.genAllSpec_Simple(DXBind('tmp', SType('nat'), self.counter), if_bexp_new_var, if_bexp_new_var.type().type(), samebitexp)]


                stmts += [DXAssign([loop_newVars[x]], DXBin('+', loop_newVars[x], DXList([tmp_vars[x]])), qafny_line_number=self.current_qafny_line_number) for x in tmp_vars]
                stmts += [DXCall('omega0', [], True)]
                self.libFuns.add('omega0')

            #outer while loops
            elif num + 1 <  nqty.flag().num():
                stmts += [DXAssign([DXBind('tmp' + str(num + 1) + x)], DXList(), True, qafny_line_number=self.current_qafny_line_number) for x in loop_newVars]

                next_looping_var = DXBind('tmp', None, num + 1)
                stmts.append(DXAssign([next_looping_var], DXNum(0), True, qafny_line_number=self.current_qafny_line_number))
                tmp_new_vars = {x : DXBind('tmp' + str(num + 1) + x, loop_newVars[x].type().type()) for x in loop_newVars}
                nestedWhile = buildWhile(next_looping_var, ctx, num + 1, loop_oldVars, tmp_new_vars, nLoc, nqty, nnum, fqty, is_qcomp, bool_exp_id, bool_exp_index, bool_store_id, False, if_bexp_vals, loop_values, is_qrange)
                stmts.append(nestedWhile)

                #invariant generation for outer loops
                innerloop_invariants = nestedWhile.inv()
                inv_new = []


                invnum = self.counter
                newvar = DXBind(bool_exp_id, SType('bv1'), nnum)
                inv_old_var = {x : DXBind(x, SType('bv1'), nnum) for x in loop_oldVars}

                for ix in range(nqty.flag().num()):
                    if ix < num:
                        newvar = DXIndex(newvar, DXBind('tmp',None,ix), qafny_line_number=self.current_qafny_line_number) 
                        inv_old_var = {x : DXIndex(inv_old_var[x], DXBind('tmp',None,ix)) for x in inv_old_var}
                    else:
                        newvar = DXIndex(newvar, DXBind('tmp',None,invnum), qafny_line_number=self.current_qafny_line_number)
                        inv_old_var = {x : DXIndex(inv_old_var[x], DXBind('tmp',None,invnum), qafny_line_number=self.current_qafny_line_number) for x in inv_old_var}
                        invnum += 1


                #generation of length equality invariants for new variables eg. forall k :: 0 <= k < |p7| ==> |p7[k]| == pow2(n)
                st = SType('bv1')
                for inv_l in range(fqty.flag().num() - num):
                    st = SeqType(st)
                    tlvars = {dc : self.getBindFromIndex(loop_newVars[dc]) for dc in loop_newVars}
                    tlvars_1 = {dc : self.createIndexFromType(tlvars[dc], st, DXBind('tmp', SType('nat'), self.counter)) for dc in tlvars}
                    if inv_l + 1 < fqty.flag().num() - num:
                        rlen = nLoc[inv_l + 1].crange().right().accept(self) if isinstance(nLoc[inv_l + 1].crange().left(), QXNum) and nLoc[inv_l + 1].crange().left().num() == 0 else DXBin('-', nLoc[inv_l + 1].crange().right().accept(self), nLoc[inv_l + 1].crange().left().accept(self))
                        rlen = DXCall('pow2', [rlen])
                        inv_new += [self.genAllSpec_Simple(DXBind('tmp', None, self.counter), tlvars[x], st, DXComp('==', DXLength(tlvars_1[x]), rlen)) for x in tlvars]
                    else:
                        inv_new += [self.genAllSpec_Simple(DXBind('tmp', None, self.counter), tlvars[x], st, DXComp('==', DXLength(tlvars_1[x]), (nLoc_dict[x].crange().right().accept(self) if isinstance(nLoc_dict[x].crange().left(), QXNum) and nLoc_dict[x].crange().left().num() == 0 else  DXBin('-', nLoc_dict[x].crange().right().accept(self), nLoc_dict[x].crange().left().accept(self))))) for x in tlvars if x != 'amp']

                for inv in innerloop_invariants:

                    tmp = inv

                    #to change the pre existing variables indexing based on the loop level
                    def lambda_replace_oldvar(lself, x):
                        if isinstance(x, DXIndex):
                            tmp_lr = x
                            while not isinstance(tmp_lr, DXBind):
                                tmp_lr = tmp_lr.bind()

                            if tmp_lr.num() and tmp_lr.num() == nnum:
                                return inv_old_var[tmp_lr.ID()]

                    lamb_subst = SubstLambda(lambda_replace_oldvar)
                    tmp = lamb_subst.visit(tmp)

                    if isinstance(inv, DXAll):

                        while isinstance(tmp, DXAll):
                            tmp = tmp.next()
                            if isinstance(tmp, DXLogic) and tmp.op() == '==>':
                                tmp = tmp.right()

                        if isinstance(tmp, DXComp):
                            right = tmp.right()
                            left = tmp.left()
                            for vars in tmp_new_vars:

                                #to change the tmp vars from inner loop to the tmp vars of the current loop
                                def lambda_replace(lself, x):
                                    if isinstance(x, DXIndex) and isinstance(x.bind(), DXBind) and x.bind().ID() == tmp_new_vars[vars].ID():
                                        return DXIndex(loop_newVars[vars], x.index())

                                subst_lamb = SubstLambda(lambda_replace)
                                right = subst_lamb.visit(right)
                                left = subst_lamb.visit(left)

                                #to remove one index of outer looping variable from x[i][j][tmp1] to x[i][tmp1]
                                selfcount = self.counter
                                def lambda_replace1(lself, x):
                                    if isinstance(x, DXIndex) and isinstance(x.bind(), DXIndex) and x.index().num() == selfcount:
                                        tmp1 = x
                                        while not isinstance(tmp1, DXBind):
                                            tmp1 = tmp1.bind()
                                        if tmp1.num() and tmp1.num() == nnum:
                                            return DXIndex(x.bind().bind(), x.index())

                                subst_lamb = SubstLambda(lambda_replace1)
                                right = subst_lamb.visit(right)

                                #to add another index for the vars -> x[tmp1][tmp2] to x[tmp1][tmp2][tmp3]
                                def lambda_replace2(lself, x):
                                    if isinstance(x, DXIndex):
                                        tmp1 = x
                                        while not isinstance(tmp1, DXBind):
                                            tmp1 = tmp1.bind()
                                        if tmp1.ID() == loop_newVars[vars].ID() and tmp1.num() and tmp1.num() == loop_newVars[vars].num():
                                            return DXIndex(x, DXBind(x.index().ID(), x.index().type(), x.index().num() + 1))

                                subst_lamb = SubstLambda(lambda_replace2)    
                                right = subst_lamb.visit(right)

                            #to change the value of some equalities such as castBVInt(p6..) = k to castBVINt(p6) = j since the forall variable changes 
                            if isinstance(right, DXBind) and right.ID() == 'tmp' and right.num() > self.counter:
                                right = DXBind('tmp', right.type(), right.num() + 1)
                            elif isinstance(right, DXIfExp) and isinstance(right.left(), DXBind) and right.left().ID() == 'tmp':
                                right = DXIfExp(right.bexp(), DXBind('tmp', right.left().type(), right.left().num() + 1), right.right(), qafny_line_number=self.current_qafny_line_number)

                            amp_flag = True
                            if isinstance(left, DXCall) and left.ID() == 'castBVInt':
                                amp_flag = False
                                left = left.exps()[0]
                            while isinstance(left, DXIndex):
                                left = left.bind()

                            if isinstance(left, DXBind):
                                #handling inner loop invariants which are value equalities eg. forall k :: 0 <= k < |p1| ==> castBVInt(p1[k]) == k
                                inv_new += [self.genAllSpec(DXBind('tmp', None, self.counter), left, right, amp_flag)]
                            else:
                                #handling inner loop invariants which are length equalities eg. forall k :: 0 <= k < |p1| ==> |p1[k]| == pow2(n)
                                '''if isinstance(left, DXLength):
                                    tmp_allspec = left.var()
                                    while not isinstance(tmp_allspec, DXBind):
                                        tmp_allspec = tmp_allspec.bind()

                                    all_spec_type_tmp = left.var() 
                                    all_spec_type = SType('bv1')
                                    while isinstance(all_spec_type_tmp, DXIndex):
                                        all_spec_type_tmp = all_spec_type_tmp.bind()
                                        all_spec_type = SeqType(all_spec_type)

                                    inv_new += [self.genAllSpec_Simple(DXBind('tmp', None, self.counter), tmp_allspec, all_spec_type, DXComp(tmp.op(), left, right))]'''




                        elif isinstance(tmp, DXCall) and tmp.ID() == 'samebit':
                            exp = tmp


                            for vars in tmp_new_vars:
                                #in samebit ivnariants we have x[tmp1][tmp2] from previous loop, the following code will add another index x[tmp1][tmp2][tmp3]
                                def lambda_replace(lself, x):
                                    if isinstance(x, DXIndex):
                                        tmp_l = x
                                        while not isinstance(tmp_l, DXBind):
                                            tmp_l = tmp_l.bind()
                                        if tmp_l.ID() == tmp_new_vars[vars].ID():
                                            return DXIndex(x, DXBind('tmp', None, x.index().num() + 1))

                                subst_lamb = SubstLambda(lambda_replace)
                                exp = subst_lamb.visit(exp)

                                #change the variable from the inner loop invariants to the current loop variable
                                def lambda_replace1(lself, x):
                                    if isinstance(x, DXBind) and x.ID() == tmp_new_vars[vars].ID():
                                        return loop_newVars[vars]

                                subst_lamb = SubstLambda(lambda_replace1)
                                exp = subst_lamb.visit(exp)

                            tmp_var_genAll = exp.exps()[0]
                            while not isinstance(tmp_var_genAll, DXBind):
                                tmp_var_genAll = tmp_var_genAll.bind()

                            inv_new += [self.genAllSpec_Simple(DXBind('tmp', None, self.counter),tmp_var_genAll, tmp_var_genAll.type().type(), exp)]



                invariants += inv_new
                stmts += [DXAssign([loop_newVars[x]], DXBin('+', loop_newVars[x], DXList([tmp_new_vars[x]])), qafny_line_number=self.current_qafny_line_number) for x in loop_newVars]

            stmts.append(DXAssign([looping_var], DXBin('+', looping_var, DXNum(1)), qafny_line_number=self.current_qafny_line_number))

            return DXWhile(while_predicate, stmts, invariants, qafny_line_number=self.current_qafny_line_number)


        while_stmt = buildWhile(DXBind('tmp', None, 0), ctx, 0, loop_oldVars, loop_newVars, nLoc, nqty, nnum, fqty, is_qcomp, bool_exp_id, bool_exp_index, bool_store_id, False, [bool_exp_id], loop_values, is_qrange)


    def visitIfOld(self, ctx: Programmer.QXIf):
        if isinstance(ctx.bexp(), QXBool):
            bex = ctx.bexp().accept(self)
            terms = []
            for elem in ctx.stmts():
                terms += elem.accept(self)
            typeCheck = TypeChecker(self.fkenv, self.tenv, self.varnums, self.counter)
            typeCheck.visit(ctx)
            self.fkenv = typeCheck.kenv()
            self.varnums = typeCheck.renv()
            self.counter = typeCheck.counter

            return DXIf(bex, terms, [])

        #the below one is an example for en(1) typed only
        #we might need to deal with other cases like aa type, and had type
        lcollect = LocusCollector()
        lcollect.visit(ctx.bexp())
        hadamard_flag = False

        #bexp_locus = lcollect.renv[0]

        #lcollect = LocusCollector()

        lid = ''
        for elem in ctx.stmts():
            if isinstance(elem, QXQAssign) and isinstance(elem.exp(), QXSingle) and elem.exp().op() == 'H':
                hadamard_flag = True
                lid = elem.locus()[0].location()
            lcollect.visit(elem)

        #stmtsLocus = lcollect.renv[0]

        newLoc =lcollect.renv
        #newLoc = self.mergeLocus(bexp_locus, stmtsLocus)

        for i in range(len(newLoc)):
            for l, qty, num in self.varnums:
                for j in l:
                    if j.location() == newLoc[i].location():
                        newLoc[i] = j
                        break


        result = []
        vs = subLocus(newLoc, self.varnums)
        if vs is None:
            v = subLocusGen(newLoc, self.varnums)
            if v is None:
                return None
            floc, ty, rev, num = v
            rea = QXCast(TyEn(QXNum(1)), floc)

            # merge the boolean guard bit into the locus of the body 

            cast = rea.accept(self)
            if cast:
                result.extend(cast)
                num = self.counter - 1



            vs = floc,ty, num

        nLoc, nTy, nNum = vs

        if hadamard_flag:
            oldvars = makeVars(nLoc, nTy, nNum)
            newType = TyEn(QXNum(nTy.flag().num() + 1))
            newvars = makeVars(nLoc, newType, self.counter)
            self.counter += 1
            looping_var = DXBind("nvar", SType("nat"), self.counter)
            self.counter += 1

            result += [DXInit(x, DXList([])) for x in newvars]
            result += [DXInit(looping_var, DXNum(0))]
            invariants = []

            for i in oldvars:
                invariants.append(DXLogic('&&', DXComp('<=', DXNum(0), looping_var), DXComp('<=', looping_var, DXLength(i))))

            for i in newvars:
                invariants.append(DXComp('==', DXLength(i), looping_var))

            for i in newvars:
                if i.ID() == 'amp':

                    tmpt = None
                    tmpf = None
                    for ii in oldvars:
                        if ii.ID() != 'amp':
                            r = self.initial_locus_data[self.getMapIndex(ii)]['length']
                            r = DXBind(r) if isinstance(r, str) else DXNum(r)
                            if tmpt is None:
                                tmpt = r
                            else:
                                tmpt = DXBin('+', tmpt, r)

                            if 'ampf' not in self.initial_locus_data[self.getMapIndex(ii)]:
                                tmpf = DXBin('/', DXNum(1.0), DXCall('sqrt',[DXCast(SType('real'),DXCall('pow2',[nLoc[0].crange().right().accept(self)]))]))
                            else:
                                tmpf = self.initial_locus_data[self.getMapIndex(ii)]['ampf']

                    matching_old_var = [x for x in oldvars if x.ID() == lid][0]
                    old_unchanged_val = self.initial_locus_data[self.getMapIndex(matching_old_var)]['val']
                    if isinstance(old_unchanged_val, DXBin):
                        old_unchanged_val = self.replaceInBin(old_unchanged_val, ['k', 'j'], DXBind('tmp',SType('nat'), self.counter))
                    tmpt = DXCast(SType('real'), DXCall('pow2', [tmpt]))

                    ampf = tmpf
                    ampt =  DXBin('*', DXBin('/', DXNum(1.0), DXCall('sqrt', [tmpt])), DXCall('omega', [old_unchanged_val, DXNum(2)]))
                    rightamp = DXIfExp(DXComp('==', DXBind('tmp',SType('nat'), self.counter), DXNum(1)), ampt, ampf)
                    invariants.append(self.genAllSpec(DXBind('tmp',SType('nat'), self.counter), i, rightamp, True))
                    self.libFuns.add('sqrt')
                    self.libFuns.add('pow2')
                else:
                    if i.ID() == lid:
                        matching_old_var = [x for x in oldvars if x.ID() == lid][0]
                        old_unchanged_val = self.initial_locus_data[self.getMapIndex(matching_old_var)]['val']
                        if isinstance(old_unchanged_val, DXBin):
                            old_unchanged_val = self.replaceInBin(old_unchanged_val, ['k', 'j'], DXBind('tmp',SType('nat'), self.counter))
                        invariants.append(self.genAllSpec(DXBind('tmp',SType('nat'), self.counter), i, 
                                                          DXIfExp(DXLogic('==', DXBind('tmp', SType('nat'), self.counter), DXNum(1)), DXBind('tmp', SType('nat'), self.counter + 1), old_unchanged_val), False))
                    else:
                        invariants.append(self.genAllSpec(DXBind('tmp',SType('nat'), self.counter), i, DXBind('tmp', SType('nat'), self.counter ), False))


            loop_stmts = []
            pred, v = ctx.bexp().accept(self)
            if isinstance(ctx.bexp(), QXQIndex):
                pred = DXComp('==', DXIndex(DXIndex(v.bind(), looping_var), ctx.bexp().index().accept(self)), DXNum(1))
                self.libFuns.add('castBVInt')


            if_stmts = []
            else_stmts = []
            post_if_stmts = []
            for i in ctx.stmts():
                if isinstance(i, QXQAssign) and isinstance(i.exp(), QXSingle) and i.exp().op() == 'H':
                    currids = [x.ID() for x in i.locus()]
                    tmpvars = [DXBind('tmp_' + t) for t in currids]
                    tmp_amp_var = DXBind('tmp_amp')
                    if_stmts += [DXInit(x) for x in tmpvars]
                    old_matching_vars = sorted([x for x in oldvars if x.ID() in currids], key = lambda _: _.ID())
                    new_matching_vars = sorted([x for x in newvars if x.ID() in currids], key = lambda _: _.ID())
                    other_old_vars = sorted([x for x in oldvars if (x.ID() not in currids) and (x.ID() != 'amp')], key = lambda _: _.ID())
                    other_old_assignment_vars = [DXIndex(x, looping_var) for x in other_old_vars]
                    if_stmts += [DXAssign([list(filter(lambda _: _.ID()[4:] == x.ID(), tmpvars))[0], tmp_amp_var], DXCall('partialcastEn1toEn2', [DXIndex(x, looping_var), *other_old_assignment_vars])) for x in old_matching_vars]
                    if_stmts += [DXAssign([x], [DXBin('+', x, DXList([list(filter(lambda _: _.ID()[4:] == x.ID(), tmpvars))[0]]))]) for x in new_matching_vars]
                    self.libFuns.add('partialcastEn1toEn2')

                    else_stmts +=  [DXInit(x) for x in tmpvars]
                    else_stmts += [DXAssign([list(filter(lambda _: _.ID()[4:] == x.ID(), tmpvars))[0]], DXCall('duplicateSeq', [DXIndex(x, looping_var), DXCall('pow2',[DXLength(DXIndex(x, looping_var))])])) for x in old_matching_vars]
                    else_stmts += [DXAssign([x], [DXBin('+', x, DXList([list(filter(lambda _: _.ID()[4:] == x.ID(), tmpvars))[0]]))]) for x in new_matching_vars]
                    self.libFuns.add('duplicateSeq')

                    tmp_var = DXBind('tmp_' + v.bind().ID())

                    loop_stmts += [DXInit(tmp_amp_var), DXInit(tmp_var), DXAssign([tmp_var], DXCall('duplicateSeq', [DXIndex(v.bind(), looping_var), DXCall('pow2',[DXLength(DXIndex(old_matching_vars[0], looping_var))])]))]
                    new_bind_var = list(filter(lambda _: _.ID() == v.bind().ID(), newvars))[0]
                    new_amp_var = list(filter(lambda _: _.ID() == 'amp', newvars))[0]
                    loop_stmts += [DXAssign([new_bind_var], DXBin('+', new_bind_var, DXList([tmp_var])))]
                    #loop_stmts += [DXAssign([tmp_amp_var], DXCall('createAmp', [DXLength(DXIndex(old_matching_vars[0], looping_var))]))]
                    #if_stmts += [DXAssign([tmp_amp_var], DXCall('createAmp', [DXBin('*',DXCall('pow2',[DXLength(DXIndex(oldvars[1], looping_var))]), DXCall('pow2',[DXLength(DXIndex(oldvars[2], looping_var))]))]))]
                    else_stmts += [DXAssign([tmp_amp_var], DXCall('duplicateAmp',[DXIndex(list(filter(lambda _: _.ID() == 'amp', oldvars))[0], looping_var), DXCall('pow2',[DXLength(DXIndex(old_matching_vars[0], looping_var))])]))]
                    post_if_stmts += [DXAssign([new_amp_var], DXBin('+', new_amp_var, DXList([tmp_amp_var])))]
                    self.libFuns.add('duplicateSeq')
                    self.libFuns.add('duplicateAmp')
                    self.replaceType(nNum, newType)


                else:
                    tmp = i.accept(self)
                    if isinstance(tmp, list):
                        if_stmts.extend(tmp)
                    else:
                        if_stmts.append(tmp)


            #Generate invariants for the resultant sequences
            nLoc, nqty, nnum = subLocus(nLoc, self.varnums)
            lcounter = self.counter
            oloc = [x for x in newvars if x.ID() != lid]
            for i in newvars:
                for ti in range(nqty.flag().num()-1):
                    #self.genallSpec(DXBind('tmp',SType('nat'), self.counter), )
                    allvar = DXBind('tmp', SType('nat'), lcounter)
                    lcounter += 1

                    if ti == 0:
                        pow2_var = DXCall('pow2',[DXVar(nLoc[ti+1].crange().right().ID() if isinstance(nLoc[ti+1].crange().right(), QXBind) else str(nLoc[ti+1].crange().right().num()))])
                        #if isinstance(pow2_var.exps()[0], DXNum) or isinstance(pow2_var.exps()[0], DXVar):
                            #pow2_var = DXNum(2**int(pow2_var.exps()[0].ID()))
                        left = DXLength(DXIndex(i, allvar))
                        comp = DXComp('==', left , pow2_var)
                        tmp = DXAll(allvar, DXLogic('==>', DXInRange(allvar, DXNum(0), DXLength(i)), comp))
                        invariants.append(tmp)
                        comp = tmp
                    else:
                        pow2_in_var = DXCall('pow2',[DXVar(nLoc[ti-1].crange().right().ID())]) if isinstance(nLoc[ti-1].crange().right(), QXBind) else DXCall('pow2',[DXVar(str(nLoc[ti-1].crange().right().num()))])
                        pow2_var = DXCall('pow2',[DXVar(nLoc[ti].crange().right().ID() if isinstance(nLoc[ti].crange().right(), QXBind) else str(nLoc[ti].crange().right().num()))]) if ti < len(nLoc) else DXVar(nLoc[ti-1].crange().right().ID() if isinstance(nLoc[ti-1].crange().right(), QXBind) else str(nLoc[ti-1].crange().right().num()))
                        #if isinstance(pow2_var.exps()[0], DXNum) or isinstance(pow2_var.exps()[0], DXVar):
                            #pow2_var = DXNum(2**int(pow2_var.exps()[0].ID()))
                        left = DXLength(DXIndex(left.var(), allvar))
                        if isinstance(comp, DXAll):
                            prevall = comp.next().left()
                            pr = comp.next().right()

                        while isinstance(pr, DXAll):
                            prevall = DXLogic("==>", prevall, DXAll(pr.bind(), pr.next().left()))
                            pr = pr.next().right()

                        comp = DXAll(comp.bind(), DXLogic('==>', prevall, DXAll(allvar, DXLogic("==>", DXInRange(allvar, DXNum(0), pow2_in_var), DXComp("==", left, pow2_var)))))
                        invariants.append(comp)





            loop_stmts += [DXIf(pred, if_stmts, else_stmts)]
            loop_stmts += post_if_stmts
            loop_stmts += [DXAssign([looping_var], [DXBin('+', looping_var, DXNum(1))])]

            result += [DXWhile(DXComp('<', looping_var, DXLength(v.bind())), loop_stmts, invariants)]

            return result

        else:
            exps = makeVars(nLoc,nTy, nNum)
            vk = DXBind("nvar", SType("nat"), self.counter)
            self.counter+=1
            exps = updateInd(exps, vk)

            re = self.dealExps(nLoc, exps[0], exps[1:], ctx.stmts())

            newExps = makeVars(nLoc, nTy, self.counter)
            self.counter += 1
            newExps = updateInd(newExps, vk)

            if re is not None:
                pre, kre = re
            else:
                pre, kre = newExps[0], newExps[1:]
                #return None

            #need to add a sub function to store the transitions of predicates
            #need to insert pred to each of the predicates.
            #if we find the subterm has a predicate like requires P, ensures Q
            #then we need to say two things in the following:
            #We first say the inputting the predicate is P, then for Q, we will have
            # for all i, index(locus) <= i < index_end(locus) ==> pred(i) ==> Q(i)
            # forall i, index(locus) <= i < index_end(locus) ==> not pred(i) ==> P(i)
            # this means that in an array, if pred(i) is good, then Q(i), else P(i)
            # we also need to create a heap to store DXMethod
            # when genearting a method, it cannot be inside a stmt
            #tyCheck = TypeChecker(self.fkenv, self.ftenvp, self.fvar,self.ind)
            #tyCheck.visit()

            genExps = [pre]+kre



            vx = DXBind("nvar", SType("nat"), self.counter)
            self.counter += 1

            #genereating the invs, we might need to add more to make Dafny happy
            tmpInv = []
            for elem in newExps:
                tmpInv += [DXLogic("&&", DXComp("<=", DXNum(0),vx),DXComp("<=",vx,DXLength(elem.bind())))]

            for i in range(len(genExps)):
                if newExps[i].bind().ID() == 'amp':
                    x,y = newExps[i], genExps[i]
                else:
                    x,y = DXCall('castBVInt',[newExps[i]]), DXCall('castBVInt',[genExps[i]])
                tmpInv += [DXAll(vk, DXLogic("==>",
                    DXLogic("&&", DXComp("<=", DXNum(0),vk),DXComp("<=",vk,vx)),
                        DXComp("==",x ,y)))]
                self.libFuns.add('castBVInt')

            pred, v = ctx.bexp().accept(self)
            if isinstance(ctx.bexp(), QXQIndex):
                pred = DXComp('==', DXIndex(DXIndex(v.bind(), vx), v.index()), DXNum(1))

            terms = []
            for elem in ctx.stmts():
                res = elem.accept(self)
                terms += res
                if isinstance(elem, QXQAssign):
                    updatestmt = DXAssign([x.bind() for x in newExps], res[-1].ids())
                    terms += [updatestmt]
                    self.updateOutVarNums(elem, updatestmt)


            wil = DXWhile(DXComp("<",vx, DXLength(v.bind())), [DXIf(pred,terms,[]), DXAssign([vx], DXBin('+', vx, DXNum(1)))],tmpInv)
            result += [DXInit(vx, DXNum(0)), wil]
            return result
"""