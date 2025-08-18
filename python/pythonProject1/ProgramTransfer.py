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
    """Checks for exact equality between two quantum ranges."""
    return (str(q1.location()) == str(q2.location()) and compareAExp(q1.crange().left(),q2.crange().left())
            and compareAExp(q1.crange().right(),q2.crange().right()))


def compareQRange(q1: QXQRange, q2: QXQRange):
    """Checks if two quantum ranges refer to the same variable and start at the same index."""
    return str(q1.location()) == str(q2.location()) and compareAExp(q1.crange().left(),q2.crange().left())

def compareRangeLocus(q1: QXQRange, qs: [QXQRange]):
    """
    Checks if a quantum range q1 is a prefix of a locus qs.
    If it is, it returns the remainder of the locus.
    """
    vs = []
    for i in range(len(qs)):
        if compareQRange(q1,qs[i]):
            if compareAExp(q1.crange().right(), qs[i].crange().right()):
                return (vs + (qs[i+1:len(qs)]))
            else:
                print("Found prefix match in compareRangeLocus:", q1, qs[i], qs[i].line_number())
                return (vs + [QXQRange(q1.location(), qs[i].index(),
                                       QXCRange(qs[i].crange().left(),
                                                QXBin("+",q1.crange().right(), QXNum(1)),
                                                qs[i].crange().line_number()), qs[i].line_number())]
                        + (qs[i+1:len(qs)]))
        vs = vs + [qs[i]]
    return None

#compareLocus and return the reminder locus
def compareLocus(q1: [QXQRange], q2: [QXQRange]):
    """Checks if locus q1 is a sub-locus of q2 and returns the remainder."""
    vs = q2
    for elem in q1:
        vs = compareRangeLocus(elem, vs)
        if vs is None:
            return None
    return vs

#check if q2 is in the database, and then return locus,qty,varnum, where q2 is part of locus
def subLocus(q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):
    """Finds which locus in the current state `qs` contains the target locus `q2`."""
    vs = q2
    qsf = []
    for locus,qty,var in qs:
        vs = compareLocus(q2, locus)
        if vs is not None:
            return locus,qty,var
    return None


def genType(n:int, t:DXType):
    """Generates a nested Dafny sequence type of depth n."""
    for i in range(n):
        t = SeqType(t)
    return t

def makeVars(locus:[QXQRange], t:QXQTy, n:int):
    """
    Creates the Dafny variable bindings (DXBind) for a given Qafny locus and type.
    This is a core function for mapping Qafny's type system to Dafny's type system.
    """
    tmp = dict()
    if isinstance(t, TyNor):
        for elem in locus:
            tmp.update({elem.location(): DXBind(elem.location(), SeqType(SType("bv1")),n)})

    elif isinstance(t, TyEn):
        num = t.flag().num()
        for elem in locus:
            tmp.update({elem.location(): DXBind(elem.location(), genType(num, SeqType(SType("bv1"))),n)})
        # amplitude comes after the value in hadEn, that should be the precedent
        tmp['amp'] = DXBind("amp", genType(num, SType("real")), n)
    #    tmp += DXBind("amp", genType(num, SType("real")), n)
    
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
            return TyAA(t1.flag(), t2.qrange(), t2.line_number())
    if isinstance(t1, TyAA) and isinstance(t2, TyEn):
        if t1.flag() < t2.flag():
            return TyAA(t2.flag(), t1.qrange(), t1.line_number())
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
        tmp = DXIndex(tmp, DXBind(elem.ID(),SType("nat")))
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
        tmp += [DXIndex(elem,ind)]
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
        self.currBinds = None

        # additional Dafny methods, as template libs generated from the method
        # in printing, one needs to print out the additional methods first
        self.addFuns = []

        # additional Dafny methods that need to be pulled from DafnyLibrary
        self.libFuns = set()

        # flag variable to handle transfering of ensures differently to map the correct output variable number to the ensures
        self.t_ensures = False

        #dictionary to store all initial locus data like length and values
        self.initial_locus_data = {}

        #qafny line number to input into Dafny AST
        self.current_line = None

    #add DX functions to cast types
    def joinIfLocus(self, q: QXQRange, aLocus :[QXQRange], aTy: QXQTy, aVars: dict,
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
                                     q.line_number())], TyNor, aVars(q.location()).num())]
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
                                     q.line_number())], TyHad, aVars(q.location()).num())]
                    self.counter += 1
                    return vs
        return None

    def genHadEnCastPred(self, vars: dict, qty: QXQTy, line_number: int):
        """
        Generates the Dafny AST for casting a TyHad state to a TyEn state.
        This involves creating new Dafny variables for the TyEn representation and
        generating a call to the 'hadEn' library function.
        """
        newvars = self.upVarsType(vars, qty)
        result = [DXInit(x, line=line_number) for x in newvars.values()]
        newampvar = [x for x in newvars.values() if x.ID() == 'amp']
        othervars = [x for x in newvars.values() if x.ID() != 'amp']
        result += [DXAssign(newampvar + othervars, DXCall("hadEn", vars.values(), line=line_number),
                            line=line_number)]
        self.libFuns.add('hadEn')
        return newvars, result

    def genEnNorExtendPred(self, x:DXBind, y:DXBind, qty:QXQTy, line_number: int):
        """
        Generates the Dafny AST for extending an existing TyEn locus by merging a
        classical (TyNor) qubit into it. It generates a call to the 'mergeBitEn'
        library function.
        """
        self.libFuns.add('mergeBitEn')
        v = DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXCall('mergeBitEn',
                                [DXLength(x), y]),True, line=line_number)
        self.counter += 1
        return v

    #TODO: need to modify the meaning of duplicateMergeBitEn to be a AA type choice,
    #the number of elements in seq will not change, but we will have case for y == 0 and y == 1
    def genEnHadAASeqPred(self, vars: dict, y:DXBind, qty:QXQTy, line_number: int):
        """
        Generates the Dafny AST for the complex operation of merging a TyHad control
        qubit with a TyEn target locus. This results in a TyAA state, representing
        the two branches of the quantum conditional.
        """
        x = vars.values()[0]
        res = []

        yleft = y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)
        res += DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXNum(0),True, line=line_number)
        self.counter += 1

        yright = y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)
        res += DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXNum(1),True, line=line_number)
        self.counter += 1

        self.libFuns.add('duplicateMergeBitEn')
        self.libFuns.add('mergeAmpEn')
        self.libFuns.add('omega')
        half = DXBin('/', QXNum(1), DXUni('sqrt', 2))
        omega = DXCall('omega', [DXNum(1),DXNum(2)], True, line=self.current_line)
        for elem in vars.values():
            if elem.ID() != 'amp':
                #the duplicateMergeBitEn will simply copy the same result from the old elem to be a new one
                #and perform y == 0 ==> duplicate as well as y == 1 ==> duplicate
                res += [DXAssign([DXBind(elem.ID(), genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                     DXCall('duplicateMergeBitEn',  [yleft, DXNum(0), DXLength(x), elem]),
                     True, line=line_number)]
                self.counter += 1
                res += [DXAssign([DXBind(elem.ID(), genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                     DXCall('duplicateMergeBitEn',  [yright, DXNum(1), DXLength(x), elem]),
                     True, line=line_number)]
                self.counter += 1
            else:
                res += [DXAssign([DXBind('amp', genType(qty.flag(), (SType("real"))), self.counter)],
                     DXCall('mergeAmpEn', [yleft, DXNum(0), DXLength(x), elem, half]),
                     True, line=line_number)]
                self.counter += 1
                res += [DXAssign([DXBind('amp', genType(qty.flag(), (SType("real"))), self.counter)],
                     DXCall('mergeAmpEn', [yleft, DXNum(0), DXLength(x), elem,
                                           DXBin('*', half, DXBin('*', omega, y))]),
                     True, line=line_number)]
                self.counter += 1


    def superLocus(self, q2: [QXQRange], ty:QXQTy):
        vs = []
        for i in range(len(self.varnums)):
            loc, qty, vars = self.varnums[i]
            if ty == qty:
                rem = compareLocus(loc, q2)
                if rem is not None:
                    return rem, loc, qty, vars, (vs + self.varnums[i+1:])
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
                _, pred = self.genHadEnCastPred(vars, qty, loc[0].line_number())
                qty = TyEn(QXNum(1), qty.line_number())
                norRe, norVars, tmpVarNumsa = self.collectNorLocus(remind, tmpVarNums)
                loc = loc + norRe
                for v in norVars.values():
                    pred += [self.genEnNorExtendPred(vars.values()[0], v, qty, loc[0].line_number())]
                vars.update({k:v for k,v in norVars.items()})
                self.varnums = ([(loc, qty, vars)]) + tmpVarNumsa
                return pred, loc, qty, vars

            if isinstance(ty, TyEn):
                norRe, norVars, tmpVarNumsa = self.collectNorLocus(remind, tmpVarNums)
                loc = loc + norRe
                pred = []
                for v in norVars.values():
                    pred += [self.genEnNorExtendPred(vars.values()[0], v, qty, loc[0].line_number())]
                vars.update({k:v for k,v in norVars.items()})
                vb = findHadLocus(remind, tmpVarNumsa)
                if vb is not None:
                    qa, oldVars, tmpVarNumsb = vb
                    pred += self.genEnHadAASeqPred(vars, oldVars.values()[0], ty, loc[0].line_number())
                    qty = TyAA(qty.flag(), qa, loc[0].line_number())
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

    def up_vars(self, v: dict):
        """Creates a new map of variable names to new unique integer identifiers."""
        tmp = {}
        for key in v.keys():
            tmp[key] = self.counter
            self.counter += 1
        return tmp

    def upVars(self, v: dict):
        """Updates Dafny variables with incremented counters."""
        tmp = dict()
        for key in v.keys():
            tmp.update({key: v.get(key).newBind(self.counter)})
            self.counter += 1

        return tmp

    def upVarsSub(self, v: dict, qs:set):
        tmp = dict()
        for key in v.keys():
            if key in qs:
                tmp.update({key: v.get(key).newBind(self.counter)})
                self.counter += 1
            else:
                tmp.update({key: v.get(key)})
        return tmp

    #create new DXBind with TyNor and TyHad type Only
    #and for TyEn, we increment the type to 1
    def upVarsType(self, v: dict, t: QXQTy):
        tmp = dict()
        for key in v.keys():
            print('upVarsType', key, v.get(key), t)
            if isinstance(t, TyNor):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("bv1")), self.counter)})
            elif isinstance(t, TyHad):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("real")), self.counter)})
            elif isinstance(t, TyEn):              
                if not 'amp' in tmp:
                    tmp['amp'] = DXBind('amp', genType(1, SType("real")), self.counter)
                    tmp.update({key: v.get(key).newBindType(genType(1, SeqType(SType("bv1"))), self.counter)})
                else:
                    tmp.update({key: v.get(key).newBindType(SeqType(v.get(key).type()), self.counter)})
            print('\nupVarsType en amp var', key, tmp.get(key))
        self.counter += 1

        return tmp

    def genVarNumMap(self, tenv: list):
        """Initializes the varnums map from the type environment."""
        tmp = []
        for locus, qty in tenv:
            tdis = dict()

            tdis.update(makeVars(locus, qty, self.counter))
    #                tdis.update({r.location(): makeVars([r], qty).get(r.location()).newBind(self.counter)})
            self.counter += 1
            # Only add an amp ID for types that have an explicit amplitude component.
    #        if isinstance(qty, (TyEn, TyAA)):
    #            tdis['amp'] = self.counter
    #            self.counter += 1
            tmp.append((locus, qty, tdis))
        return tmp

    def upvar(self, var: str):
        tmp = self.varnums.get(var)
        self.varnums.update({var:self.varnums.get(var)+1})
        return tmp

    def calRange(self, v: QXCRange):
        '''Converts a QXCRange to a QXAExpr that represents the length of the q-bit array.'''
        if isinstance(v.left(), QXNum) and v.left().num() == 0:
            return v.right()
        return QXBin("-", v.right(), v.left())

    def genSizeMap(self):
        self.sizemap = dict()
    #    print('self.varnums', self.varnums)
        for locus, qty, var_num in self.varnums:          
            for elem in locus:
                v = self.calRange(elem.crange())
                print('genSizeMap', elem.location(), var_num.get(elem.location()), v)
                self.sizemap.update({(elem.location(), var_num.get(elem.location())):v})
    #    print('sizemap', self.sizemap)


    def genArgs(self, binds):
        for locus, qty, num in self.varnums:
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

    def genBindRequires(self):
        '''Generates pre-conditions (i.e. requires) based off of the binds present in this method.'''

        def gen_pow2_expr(node):
            if isinstance(node, QXBind):
                return DXCall('pow2', [DXBind(node.ID(), SType("nat"))])
            return DXCall('pow2', [DXNum(node.num())])
        def generateENRequiresForLocus(locus, qty, curr_bind, lcounter):
            res = []
            for i in range(qty.flag().num()+1):
                if i == 0:
                    tr = DXCall('pow2', [DXBind(locus[i].crange().right().ID(),SType("nat"))
                                         if isinstance(locus[i].crange().right(), QXBind)
                                         else DXNum(locus[i].crange().right().num())], line=self.current_line)
                    tmp = DXComp('==', DXLength(curr_bind), tr, line=self.current_line)
                    res.append(DXRequires(tmp, line=self.current_line))
                else:
                    allvar = DXBind('tmp', SType('nat'), lcounter)
                    lcounter += 1
                    pow2_in_var = DXCall('pow2',[DXBind(locus[i-1].crange().right().ID(),SType("nat"))],
                                         line=self.current_line) \
                        if isinstance(locus[i-1].crange().right(), QXBind) \
                        else DXCall('pow2',[DXNum(locus[i-1].crange().right().num())],
                                    line=self.current_line)
                    pow2_var = DXCall('pow2',[DXBind(locus[i].crange().right().ID(), SType("nat"))
                                              if isinstance(locus[i].crange().right(), QXBind)
                                              else DXNum(locus[i].crange().right().num())])\
                        if i < len(locus) else (DXBind(locus[i-1].crange().right().ID(), SType("nat"))
                                                if isinstance(locus[i-1].crange().right(), QXBind) else DXNum(locus[i-1].crange().right().num()))
                    pow2_in_var = gen_pow2_expr(locus[i-1].crange().right())
                    pow2_var = (gen_pow2_expr(locus[i].crange().right()) 
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
                        left = DXLength(DXIndex(curr_bind, allvar), line=self.current_line)
                        comp = DXComp('==', left, pow2_var, line=self.current_line)
                        if i == 1:
                            tmp = DXAll(allvar, DXLogic('==>', DXInRange(allvar, DXNum(0), pow2_in_var), comp),
                                        line=self.current_line)
                            res.append(DXRequires(tmp, line=self.current_line))
                            comp = tmp
                        else:
                            if isinstance(comp, DXAll):
                                prevall = comp.next().left()
                                pr = comp.next().right()

                        while isinstance(pr, DXAll):
                            prevall = DXLogic("==>", prevall, DXAll(pr.bind(), pr.next().left()),
                                                  line=self.current_line)
                            pr = pr.next().right()
                            
                        comp = DXAll(comp.bind(),
                                         DXLogic('==>', prevall,
                                                 DXAll(allvar, DXLogic("==>",
                                                                       DXInRange(allvar, DXNum(0), pow2_in_var),
                                                                       DXComp("==", left, pow2_var)))),
                                         line=self.current_line)
                        res.append(DXRequires(comp, line=self.current_line))

                    elif i == 1:
                        left = DXLength(DXIndex(curr_bind, allvar), line=self.current_line)
                        comp = DXComp('==', left , pow2_var, line=self.current_line)
                        tmp = DXAll(allvar, DXLogic('==>', DXInRange(allvar, DXNum(0), pow2_in_var), comp),
                                    line=self.current_line)
                        res.append(DXRequires(tmp, line=self.current_line))
                        comp = tmp
                    else:
                        left = DXLength(DXIndex(left.var(), allvar), line=self.current_line)
                        if isinstance(comp, DXAll):
                            prevall = comp.next().left()
                            pr = comp.next().right()

                        while isinstance(pr, DXAll):
                            prevall = DXLogic("==>", prevall, DXAll(pr.bind(), pr.next().left()),
                                              line=self.current_line)
                            pr = pr.next().right()
                        
                        comp = DXAll(comp.bind(), DXLogic('==>', prevall,
                                                          DXAll(allvar, DXLogic("==>", DXInRange(allvar, DXNum(0), pow2_in_var),
                                                                                DXComp("==", left, pow2_var)))),
                                     line=self.current_line)
                        res.append(DXRequires(comp, line=self.current_line))

            return res

        conditions = []
        lcounter = self.counter
        for locus, qty, num in self.varnums:
            if isinstance(qty, TyEn):
                for elem in locus:
                    curr_bind = DXBind(elem.location(), num=num)
                    if isinstance(elem.crange().right(), QXBind):
                        conditions.append(DXRequires(DXComp('>', elem.crange().right().accept(self), DXNum(0)),
                                                     line=self.current_line))
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
                                                     line=self.current_line))
                    if isinstance(left, QXNum) and left.num() == 0:
                        conditions.append(DXRequires(DXComp('==', DXLength(DXBind(elem.location(), num=num)), right),
                                                     line=self.current_line))
                    else:
                        conditions.append(DXRequires(DXComp('==', DXLength(DXBind(elem.location(), num=num)),
                                                            DXBin('-',right, left.accept(self))),
                                                     line=self.current_line))

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

        def generateENEnsuresForLocus(locus, qty, curr_bind, lcounter):
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
                
                res += [DXEnsures(val, line=self.current_line)] \
                    if i == 0 else \
                    [DXEnsures(self.genAllSpec_Simple(DXBind('tmp', SType('nat'), lcounter), curr_bind, ttype, val),
                               line=self.current_line)]
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
                                                    line=self.current_line))
                    else:
                        conditions.append(DXEnsures(DXComp('==', DXLength(DXBind(elem.location(), num=num)),
                                                           DXBin('-',right, left.accept(self))),
                                                    line=self.current_line))

        return conditions

    def removeLocus(self, vs:[QXQRange]):
        tmp = []
        for i in range(len(self.varnums)):
            locus,qty,var_num = self.varnums[i]
            if vs == locus:
                tmp += self.varnums[i+1:len(self.varnums)]
                break
            else:
                tmp += [(locus,qty,var_num)]

        self.varnums = tmp


#deprecated
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
                                loc, qty, var_num = self.outvarnums[i]
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
        print('ftenvp in method:\n', self.ftenvp)
        print('ftenvr in method:\n', self.ftenvr)
        print('ftenvpreds\n', self.tenv.get(self.fvar)[2])
        self.varnums = self.genVarNumMap(self.ftenvp)
        print('self.varnums in method:\n', self.varnums)
        self.outvarnums = self.genVarNumMap(self.ftenvr)
#        self.genSizeMap()
        self.current_line = ctx.line_number()

        # 1. Generate Dafny arguments from classical bindings and quantum inputs
        tmpbind = []
        for bindelem in ctx.bindings(): #collect classical binds
            tmpv = bindelem.accept(self)
            if tmpv is not None:
                tmpbind.append(tmpv)
        
    #    tmpbind = self.genArgs(tmpbind)
        tmpbind += [item for val in self.varnums for item in val[2].values()]
        print('\ntmpbind\n', tmpbind)
        if tmpbind is None:
            return None

        # conditions (requires/ensures) for the generated Dafny method
        tmpcond = []
        # add conditions derived from the input types (i.e. Q[n] ==> |Q| == n)
    #    tmpcond.extend(self.genBindRequires())

        for condelem in ctx.conds():
            # add the requires conditions to the generated Dafny conditions
            if isinstance(condelem, QXRequires):
                self.current_line = condelem.line_number()
                cond = condelem.accept(self)
                if isinstance(cond, list):
                    tmpcond.extend(cond)
                else:
                    tmpcond.append(cond)
        print('\ntmpcond after requires:\n', tmpcond)

        self.current_line = ctx.line_number()
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
                self.current_line = stmtelem.line_number()
#                stmtelem.accept(tc)
                s = stmtelem.accept(self)
                if isinstance(s, list):
                    tmpstmt.extend(s)
                else:
                    tmpstmt.append(s)

        #Assign final state variables to return variables.

        for loc_out, _, var_map_out in self.outvarnums:
            for loc_final, _, var_map_final in self.varnums:
                print('\ncompare out loc', loc_out, 'with final loc', loc_final)
                print('\nvar_map_out', var_map_out, 'var_map_final', var_map_final)
                if compareLocus(loc_out, loc_final) == []:
                    return_vars = list(var_map_out.values())
                    final_state_vars = list(var_map_final.values())
                    
                    return_vars.sort(key=lambda x: x.ID())
                    final_state_vars.sort(key=lambda x: x.ID())
                    tmpstmt.append(DXAssign(return_vars, final_state_vars))
                    break
        print('\ntmpstmt after stmts:\n', tmpstmt)
    
        #Translate post-conditions
        self.t_ensures = True
        for condelem in ctx.conds():
            if isinstance(condelem, QXEnsures):
                tmpcond.extend(condelem.accept(self))
        self.t_ensures = False
        print('\ntmpcond after ensures:\n', tmpcond)

        tmpreturn = []
        for reelem in ctx.returns():
            self.current_line = reelem.line_number()
            tmpv = reelem.accept(self)
            if tmpv is not None:
                tmpreturn.append(tmpv)
        tmpreturn += [item for val in self.outvarnums for item in val[2].values()]
        self.libFuns.add('abs')
        self.libFuns.add('powN')
        print('\ntmpreturn\n', tmpreturn)
        return DXMethod(str(self.fvar), axiom, tmpbind, tmpreturn, tmpcond, tmpstmt, line=ctx.line_number())


    def visitProgram(self, ctx: Programmer.QXProgram):
        tmp = []
        for elem in ctx.topLevelStmts():
            tmp.append(elem.accept(self))
        return DXProgram(tmp, line=ctx.line_number())


    def visitBind(self, ctx: QXBind):
        # vs = self.currLocus
    #    print('\nvisitBind', ctx.ID())
        # if vs is not None:
        #     loc, qty, vars = vs
        #     return vars.get(ctx.ID())

        if isinstance(ctx.type(), TySingle):
            ty = ctx.type().accept(self)
            return DXBind(ctx.ID(), ty, None, line=ctx.line_number())
        if ctx.ID() and not ctx.type():
            return DXBind(ctx.ID(), None, line=ctx.line_number())
        return None


    def visitAssert(self, ctx: Programmer.QXAssert):
        v = ctx.spec().accept(self)
        x = [DXAssert(i, line=ctx.line_number()) for i in v] \
            if isinstance(v,list) else DXAssert(v, line=ctx.line_number())
        return x
        

    def visitRequires(self, ctx: Programmer.QXRequires):
#        v = ctx.spec().accept(self)
#        v = v if isinstance(v, list) else [v]
#        x = [DXRequires(i, line=ctx.line_number()) for i in v]
        return ctx.spec().accept(self)


    def visitEnsures(self, ctx: QXEnsures):
#        v = ctx.spec().accept(self)
#        v = v if isinstance(v, list) else [v]
#        x = [DXEnsures(i, line=ctx.line_number()) for i in v]
        return ctx.spec().accept(self)

    def visitCRange(self, ctx: Programmer.QXCRange):
        super().visitCRange(ctx)

    def visitCast(self, ctx: Programmer.QXCast):
        self.current_line = ctx.line_number()
        v = subLocus(ctx.locus(), self.varnums)
        if v is not None:
            loc,qty,vars = v
            vs = compareLocus(ctx.locus(), loc)
            if not vs and isinstance(qty, TyHad) and isinstance(ctx.qty(), TyEn):
                newvars, result = self.genHadEnCastPred(vars, ctx.qty(), ctx.line_number())
                # newvars = self.upVarsType(vars,ctx.qty())
                # result = [DXInit(x, line=ctx.line_number()) for x in newvars.values()]
                # newampvar = [x for x in newvars.values() if x.ID() == 'amp']
                # othervars = [x for x in newvars.values() if x.ID() != 'amp']

                # result += [DXAssign(newampvar + othervars,DXCall("hadEn", [vars[l.location()] for l in loc],
                #                                                  line=ctx.line_number()), line=ctx.line_number())]
                # self.libFuns.add('hadEn')
                self.removeLocus(loc)
                self.varnums = [(loc,ctx.qty(),newvars)] + self.varnums
                self.counter += 1
                return result
    """
        else:
            v = subLocusGen(ctx.locus(),self.varnums)
            if v is not None:
                (floc, ty, rev, num) = v
                vs = compareLocus(ctx.locus(), floc)
                if not vs and isinstance(ty, TyHad) and isinstance(ctx.qty(), TyEn):
                    result = [DXAssign(makeVars(ctx.locus(), ctx.qty(), self.counter),
                                       DXCall("hadEn", makeVars(ctx.locus(), TyHad(), num), line=ctx.line_number()), line=ctx.line_number())]
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
                    result += [DXInit(x, line=ctx.line_number()) for x in newvars]

                    if flag:
                        result += [DXAssign(newvars, DXCall('hadNorEn', calllist, line=ctx.line_number()), line=ctx.line_number())]
                        self.libFuns.add('hadNorEn')
                    else:
                        result += [DXAssign(newvars, DXCall("hadEn", calllist, line=ctx.line_number()), line=ctx.line_number())]
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
        return DXInit(ctx.binding().accept(self), line=ctx.line_number())

    def genAllSpec(self, sbind, compleft, compright, isamp):
            type = compleft.type()
            comp = DXComp("==", compleft, compright, line=self.current_line)
            counter = self.counter + 1
            var = sbind

            if not isamp:
                type = type.type()

            while isinstance(type, SeqType):
                rlength = DXLength(compleft, line=self.current_line)
                if not isamp and isinstance(type.type(), SType):
                    compleft = DXCall('castBVInt', [DXIndex(compleft, var, line=self.current_line)],
                                      line=self.current_line)
                    self.libFuns.add('castBVInt')
                else:
                    compleft = DXIndex(compleft, var, line=self.current_line)

                if isinstance(comp, DXAll):
                    prevall = comp.next().left()
                    pr = comp.next().right()

                    while isinstance(pr, DXAll):
                        prevall = DXLogic("==>", prevall, DXAll(pr.bind(), pr.next().left()),
                                          line=self.current_line)
                        pr = pr.next().right()
                    comp = DXAll(comp.bind(), DXLogic('==>', prevall,
                                                      DXAll(var, DXLogic("==>",
                                                                         DXInRange(var, DXNum(0,
                                                                                              line=self.current_line),
                                                                                   rlength, line=self.current_line),
                                                                         DXComp("==", compleft, compright,
                                                                                line=self.current_line),
                                                                         line=self.current_line),
                                                            line=self.current_line),
                                                      line=self.current_line),
                                 line=self.current_line)
                else:
                    comp = DXAll(var, DXLogic("==>", DXInRange(var,
                                                               DXNum(0, line=self.current_line), rlength,
                                                               line=self.current_line),
                                              DXComp("==", compleft, comp.right(), line=self.current_line),
                                              line=self.current_line), line=self.current_line)

                type = type.type()
                var = DXBind(var.ID(), var.type(), counter, line=self.current_line)
                counter += 1

            return comp
    
    def genAllSpec_Simple(self, sbind, var, type, val):
        res = None

        while isinstance(type, SeqType):
            var = DXIndex(var, sbind, line=self.current_line)
            type = type.type()
            if isinstance(type, SeqType):
                sbind = DXBind(sbind.ID(), sbind.type(), sbind.num() + 1, line=self.current_line)

        while isinstance(var, DXIndex):
            var = var.bind()
            res = DXAll(sbind, DXLogic('==>', DXInRange(sbind, DXNum(0, line=self.current_line),
                                                        DXLength(var, line=self.current_line),
                                                        line=self.current_line), res if res else val,
                                       line=self.current_line), line=self.current_line)
            sbind = DXBind(sbind.ID(), sbind.type(), sbind.num() - 1, line=self.current_line)

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

        return DXBin(bin.op(), l, r, line=self.current_line)    

        
            
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
                                                    tcount, line=self.currLocus.current_line),
                                     line=self.currLocus.current_line)
                    tcount += 1
                kVars[x] = indVar

            tcount = self.counter
            for i in range(t.flag().num()):
                pVar = DXIndex(pVar, DXBind('tmp', SType('nat'), tcount,
                                            line=self.currLocus.current_line),
                               line=self.currLocus.current_line)
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
                    tmp_ucr.append(DXCall('castBVInt',[i], line=self.currLocus.current_line))
                else:
                    tmp_kVars.append(DXCall('castBVInt',[i], line=self.currLocus.current_line))
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
                        tmp.append(DXLength(newVars[i], line=self.currLocus.current_line))
                        tmp.append(DXLength(vars[i], line=self.currLocus.current_line))
                    else:
                        p_oldvar = self.createIndexFromType(vars[i], arg_type,
                                                            DXBind('tmp', SType('nat'), self.counter,
                                                                   line=self.currLocus.current_line))
                        p_newvar = self.createIndexFromType(newVars[i], arg_type,
                                                            DXBind('tmp', SType('nat'), self.counter,
                                                                   line=self.currLocus.current_line))
                        tmp.append(DXLength(p_oldvar, line=self.currLocus.current_line))
                        tmp.append(DXLength(p_newvar, line=self.currLocus.current_line))

                tres = DXComp('==', tmp[0], tmp[1], line=self.currLocus.current_line)
                for i in range(2, len(tmp)):
                    tres = DXComp('==', tres, tmp[i], line=self.currLocus.current_line)

                if ent == 0:
                    preds += [tres]
                else:
                    preds += [self.genAllSpec_Simple(DXBind('tmp', SType('nat'), self.counter,
                                                            line=self.currLocus.current_line),
                                                     newVars[0], arg_type, tres)]
                arg_type = SeqType(arg_type)
                


            for i in range(len(newKVars)):
                preds += [self.genAllSpec(DXBind('tmp', SType('nat'), self.counter,
                                                 line=self.currLocus.current_line),
                                          newKVars[i], res[i], False)]

            for i in range(len(new_ucr)):
                preds += [self.genAllSpec(DXBind('tmp', SType('nat'), self.counter,
                                                 line=self.currLocus.current_line),
                                          new_ucr[i], unchanged_range[i], False)]

            newp = phase.accept(self)
            for esub in tmpSubs:
                newp = esub.visit(newp)

            newp = DXBin('*', pVar, newp, line=self.current_line)

            preds += [self.genAllSpec(DXBind('tmp', SType('nat'), self.counter,
                                             line=self.currLocus.current_line), newPVar, newp, True)]

            return preds

        if isinstance(t, TyNor):
            tmpSubs = []
            for i in range(len(ids)):
                if isinstance(vars[i].type(),SeqType) and isinstance(vars[i].type().type(), SType) and vars[i].type().type().type() == 'bv1':
                    vars[i] = DXCall('castBVInt', [vars[i]], line=self.currLocus.current_line)
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
                preds += [DXComp('==', DXLength(newVars[i], line=self.currLocus.current_line),
                                 DXLength(vars[i].exps()[0], line=self.currLocus.current_line),
                                 line=self.currLocus.current_line)]
                preds += [DXComp("==", DXCall('castBVInt', [newVars[i]],
                                              line=self.currLocus.current_line), res[i],
                                 line=self.currLocus.current_line)]
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

                return DXBin("*", pexp, newp, line=self.current_line), lexp
            
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
                tmp += [DXAssign([DXBind(var,genType(flag,SeqType(SType("bv1"))),self.counter,
                                         line=self.current_line)],
                                 DXBind(var,genType(flag,SeqType(SType("bv1"))),num,
                                        line=self.current_line),
                                 line=self.current_line)]
            elif isinstance(kets[i].vector(), QXBin):
                var = varmap.get(str(ids[i])).ID()
                val = kets[i].vector().accept(self)
                tmp += [DXAssign([DXBind(var,genType(flag,SeqType(SType("bv1"))),self.counter)],
                                 DXCall("lambdaBaseEn",[val,DXBind(var,genType(flag,SeqType(SType("bv1"))),num,
                                                                   line=self.current_line)],
                                        line=self.current_line), line=self.current_line)]
                self.libFuns.add('lambdaBaseEn')
        return tmp


    def visitQAssign(self, ctx: Programmer.QXQAssign):
        print('well, visitQAssign', ctx.locus())
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
                    print('\nafter exp', v)
                    if v is not None:
                        return (pred + ctx.exp().accept(self))
            else:
                loc, varsb = self.originLocus
                self.originLocus = cs, varsb
                return ctx.exp().accept(self)
                #return res
        else:
#            print('locus, qs', self.varnums)
            self.currLocus = subLocus(ctx.locus(), self.varnums)
            return ctx.exp().accept(self)

        return None

    def visitMeasure(self, ctx: Programmer.QXMeasure):
        return super().visitMeasure(ctx)

    def visitCAssign(self, ctx: Programmer.QXCAssign):
        return QXCAssign([DXBind(x) for x in ctx.ids()], ctx.aexp().accept(self), line=ctx.line_number())

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
            res += [DXAssign([DXBind('amp', None, newNum, line=self.current_line),
                              DXBind(hadloc[0][0].location(), None, newNum, line=self.current_line),
                              DXBind(norloc[0][0].location(), None, newNum, line=self.current_line)],
                            DXCall('hadNorEn', [DXBind(hadloc[0][0].location(), None, hadloc[2], line=self.current_line),
                                                DXBind(norloc[0][0].location(), None, norloc[2], line=self.current_line)],
                                   line=self.current_line), True, line=self.current_line)]
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
                                     True, line=self.current_line)]
                    self.removeLocus(num)
                    self.varnums += [([QXQRange(loc[0].location(),
                                                crange =  QXCRange(QXBin('+',loc[0].crange().left(), QXNum(1)), loc[0].crange().right()))], qty, newNum)]
                    self.counter += 1
                    res += [DXAssign([DXBind(loc[0].location(), None, self.counter)],
                                     DXIndex(DXBind(loc[0].location(), None, num), DXNum(0)), True, line=self.current_line)]
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
                                             True, line=self.current_line)]
                            for l in match_loc:
                                if l.location() != loc[0].location():
                                    res += [DXAssign([DXBind(l.location(), None, self.counter)],
                                                     DXCall('duplicateMergeBitEn', [DXBind(l.location(), None, match_num)]),
                                                     True, line=self.current_line)]
                            self.libFuns.add('mergeAmpEn')
                            res += [DXAssign([DXBind('amp', None, self.counter)],
                                             DXCall('mergeAmpEn', [DXBind('amp', None, match_num), DXBind(loc[0].location(), None, cutnum)]),
                                             True, line=self.current_line)]
                            self.libFuns.add('omega0')
                            res += [DXCall('omega0', [], True, line=self.current_line)]
                            self.libFuns.add('mergeBitTrigger')
                            res += [DXCall('mergeBitTrigger', [DXBind(loc[0].location(), None, match_num),
                                                               DXBind(loc[0].location(), None, self.counter),
                                                               DXLength(DXIndex(DXBind(loc[0].location(), None, match_num), DXNum(0)))],
                                           True, line=self.current_line)]
                            self.libFuns.add('triggerSqrtMul')
                            res += [DXCall('triggerSqrtMul', [], True, line=self.current_line)]

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
                                         True, line=self.current_line)]
                        self.removeLocus(num)
                    self.removeLocus(target_locus[2]) 
            else:   
                res += [DXAssign([DXBind(x.location(), None, newNum)],
                                 DXBind(x.location(), None, target_locus[2]),
                                 True, line=self.current_line) for x in target_locus[0]]
                res += [DXAssign([DXBind('amp', None, newNum)],
                                 DXBind('amp', None, target_locus[2]), True, line=self.current_line)]

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

        #self.current_line = ctx.line_number()
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

            return [DXIf(bex, terms, elses, line=ctx.line_number())]

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
        print('\nQIf locus collector:', lc.renv)
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
        #                        line=self.current_line))]

        result = ifexp

        for stmt in ctx.stmts():
            tmp = stmt.accept(self)
            if tmp is None:
                return None
            else:
                result += [tmp]

        return result

    def visitFor(self, ctx: Programmer.QXFor):
        tmp_current_line = self.current_line
        self.current_line = ctx.line_number()
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
        self.current_line = tmp_current_line
        return [DXInit(vx, lbound, line=self.current_line),
                DXWhile(DXComp("<", vx, rbound, line=self.current_line),
                        tmpstmts, tmpinvs, line=self.current_line)]


    #This might be oversimplified. We might need to cast
    #some locus for the Requires
    #and then recall the coming back states
    #I mean this might live inside a quantum conditional
    #To transfer a quantum conditional, we might need to do so
    def visitCall(self, ctx: Programmer.QXCall):
        return DXCall(str(ctx.ID()), [x.accept(self) for x in ctx.exps()], line=ctx.line_number())

    def visitSingleT(self, ctx: Programmer.TySingle):
        return SType(ctx.type(), line=ctx.line_number())

    def visitArrayT(self, ctx: Programmer.TyArray):
        ty = ctx.type().accept(self)
        return SeqType(ty, line=ctx.line_number())

    def visitFun(self, ctx: Programmer.TyFun):
        super().visitFun(ctx, line=ctx.line_number())

    def visitQ(self, ctx: Programmer.TyQ):
        return ctx.flag().accept(self)

    def visitCNot(self, ctx: Programmer.QXCNot):
        v = ctx.next().accept(self)
        return DXNot(v, line=ctx.line_number())

    def visitNor(self, ctx: Programmer.TyNor):
        return super().visitNor(ctx)

    def visitTyHad(self, ctx: Programmer.TyHad):
        return super().visitTyHad(ctx)

    def visitEn(self, ctx: Programmer.TyEn):
        super().visitEn(ctx)

    def visitQSpec(self, ctx: QXQSpec):
        
        env = self.outvarnums if self.t_ensures else self.varnums
        res = subLocus(ctx.locus(), env)
        print('\nvisitQSpec', res, ctx.locus(), env)
        if not res:
            return []
        loc,qty,varbind = res
        
        preds = []

        for st in ctx.states():
            if isinstance(st, QXSum):
                preds.extend(self.visitSum(st, loc, qty, varbind))
            elif isinstance(st, QXTensor):
                preds.extend(self.visitTensor(st, loc, qty, varbind))
        
        return [DXRequires(p, line=ctx.line_number()) if not self.t_ensures else DXEnsures(p, line=ctx.line_number()) for p in preds]
       
        # #shape predicates
        # if isinstance(qty, (TyNor, TyHad)):
        #     for elem in loc:
        #         bind = varbind.get(elem.location())
        #         if bind:
        #             qrange = elem.crange().right().accept(self)
        #             preds.append(DXComp(">", qrange, DXNum(0), line=ctx.line_number()))
        #             preds.append(DXComp("==", DXLength(bind), qrange, line=ctx.line_number()))
        
        # elif isinstance(qty, TyEn):
        #     n = qty.flag().num()
        #     if ctx.states() and isinstance(ctx.states()[0], QXSum):
        #         sum_vars = ctx.states()[0].sums()
        #         def _wrap_in_foralls(body, iterators, range_checks):
        #             """Helper to wrap a pred in nested foralls."""
        #             nested_forall = body
        #             for i in range(len(iterators) - 1, -1, -1):
        #                 nested_forall = DXAll(iterators[i], DXLogic("==>", range_checks[i], nested_forall, line=self.current_line), line=self.current_line)
        #             return nested_forall
        #          # A single loop to handle all n+1 dimensions
        #     for i in range(n + 1):
        #         iterators = [DXBind(sum_vars[j].ID(), SType("nat")) for j in range(i)]
        #         range_checks = [DXInRange(it, DXNum(0), sv.range().right().accept(self)) for it, sv in zip(iterators, sum_vars)]
                
        #         for var, dvar in varbind.items():
        #             is_amp = (var == 'amp')                   
        #             # Amplitudes are n-dimensional, basis vectors are (n+1)-dimensional.
        #             # Skip this iteration if the variable is not deep enough.
        #             if is_amp and i >= n:
        #                 continue
        #             for it in iterators:
        #                 dvar = DXIndex(dvar, it)
                    
        #             # Determine the correct bound for this dimension's length assertion.
        #             if i < n:
        #                 bound = sum_vars[i].range().right().accept(self)
        #             else: # Innermost dimension for basis vectors
        #                 qrange = None
        #                 for r in loc:
        #                     if r.location() == var:
        #                         qrange = r.crange().right().accept(self)
        #                         break
        #                 bound = qrange                   
        #             if bound:
        #                 len_pred = DXComp("==", DXLength(dvar), bound, line=self.current_line)
        #                 preds.append(_wrap_in_foralls(len_pred, iterators, range_checks))
        
        # elif isinstance(qty, TyAA): 
        #     pass
                
        # #value predicates
        # self.currBinds = varbind #for use in children nodes
        # for i in ctx.states():
        #     result = i.accept(self)
        #     if isinstance(result, list):
        #         preds.extend(result)
        #     else:
        #         preds.append(result)

        # print('\nqspec preds', preds, '\n')
        # return preds


    def visitAA(self, ctx: Programmer.TyAA):
        return super().visitAA(ctx)

    def visitSKet(self, ctx: Programmer.QXSKet):
        return ctx.vector().accept(self)

    def visitVKet(self, ctx: Programmer.QXVKet):
        return ctx.vector().accept(self)

    def visitTensor(self, ctx:QXTensor, loc:[QXQRange], qty:QXQTy, varbind:dict):
        """
        Translates a QXTensor specification into Dafny assertions.
        This is used for simple, non-superposition states (TyNor and TyHad).
        """          
        if ctx.ID() is None:
            x = DXBind("i", SType("nat"))
        else:
            x = DXBind(str(ctx.ID()), SType("nat"))

        tmp = []

        #shape predicates
        
        for elem in loc:
            dvar = varbind.get(elem.location())
            if dvar:             
                qcount = elem.crange().right().accept(self)
                if not self.t_ensures:
                    tmp.append(DXComp(">", qcount, DXNum(0), line=ctx.line_number()))
                tmp.append(DXComp("==", DXLength(dvar), qcount, line=ctx.line_number()))
                
        #value predicates 
        for i, ket in enumerate(ctx.kets()):
            var = loc[i].location()
            dvar = varbind.get(var)
            if not dvar: continue

            ket_val = ket.accept(self)
            qcount = loc[i].crange().right().accept(self)
            drange = DXInRange(x, DXNum(0), qcount, line=ctx.line_number())

            body = DXComp("==", DXIndex(dvar, x), ket_val, line=ctx.line_number())

            if body:
                tmp.append(DXAll(x, DXLogic("==>", drange, body), line=ctx.line_number()))    
        
        return tmp


    def visitSum(self, ctx: QXSum, loc:[QXQRange], qty:TyEn, varbind:dict):
        """
        Translates a QXSum specification into a complete list of Dafny predicates,
        including both structural (shape) and logical (value) predicates.
        """

        tmp = []
        n = qty.flag().num()
        sum_vars = ctx.sums()
        print('\nvisitSum loc, qty, varbind:', loc, qty, varbind)

        def _warp_in_foralls(body, iterators):
            """Helper to wrap a pred in nested foralls."""
            nested_forall = body
            for i in range(len(iterators) - 1, -1, -1):
                drange = DXInRange(iterators[i], sum_vars[i].range().left().accept(self), sum_vars[i].range().right().accept(self), line=self.current_line)
                if sum_vars[i].condition():
                    drange = DXBin('&&', drange, sum_vars[i].condition().accept(self))
                nested_forall = DXAll(iterators[i], DXLogic("==>", drange, nested_forall, line=self.current_line), line=self.current_line)
            return nested_forall
        
        # Shape predicates
        for elem in loc:
            dvar = varbind.get(elem.location())
            if dvar: 
                left = elem.crange().left().accept(self)            
                qcount = elem.crange().right().accept(self)
                if not self.t_ensures:
                    len_pred = DXComp(">", qcount, DXNum(0), line=ctx.line_number())
                    tmp.append(len_pred) if len_pred not in tmp else None
        
        for i in range(n + 1):
            iterators = [DXBind(sum_vars[j].ID(), SType("nat")) for j in range(i)]

            for var, dvar in varbind.items():
                is_amp = (var == 'amp')
                if is_amp and i >= n:
                    continue
                for it in iterators:
                    dvar = DXIndex(dvar, it)
                # Determine the correct bound for this dimension's length assertion.
                if i < n:
                    bound = sum_vars[i].range().right().accept(self)
                else: # Innermost dimension for basis vectors
                    qrange = next((r for r in loc if r.location() == var), None)
                    bound = qrange.crange().right().accept(self) if qrange else None
                if bound:
                    len_pred = DXComp("==", DXLength(dvar), bound)
                    tmp.append(_warp_in_foralls(len_pred, iterators))

        # Value predicates
        var_ket = {loc[i].location(): ket for i, ket in enumerate(ctx.kets())}   
        for var, dvar in varbind.items():
            if var == 'amp':
                continue
            ket = var_ket.get(var)
            if ket is None:
                continue
            idx = ket.accept(self)
            eq = DXComp("==", DXCall('castBVInt', [makeIndex(dvar, sum_vars)]), idx, line=ctx.line_number())
            self.libFuns.add('castBVInt')
            tmp.append(_warp_in_foralls(eq, [DXBind(sv.ID(), SType("nat"), line=ctx.line_number()) for sv in sum_vars]))

        # Amplitude predicate
        ampvar = varbind.get('amp')
        if ampvar:
            amp_val = ctx.amp().accept(self)
            idx_amp = makeIndex(ampvar, sum_vars)
            body = DXComp("==", idx_amp, amp_val, line=ctx.line_number())
            tmp.append(_warp_in_foralls(body, [DXBind(sv.ID(), SType("nat"), line=ctx.line_number()) for sv in sum_vars]))
        
        return tmp

            








# #        print('self.currBinds', self.currBinds)
#         vars = [x for x in self.currBinds if x != 'amp']
# #        print('\nvars in sum', vars)
# #        tmp_current_line = self.current_line
#         self.current_line = ctx.line_number()
#         for i in range(len(vars)):
#             v = ctx.kets()[i].accept(self)
#             eq = DXComp("==",DXCall('castBVInt', [makeIndex(vars[i], ctx.sums())]),v, line=ctx.line_number())
# #            print('\neq in sum', eq)
#             self.libFuns.add('castBVInt')
#             for con in ctx.sums()[::-1]:
#                 x = DXBind(con.ID(), SType("nat"))
#                 arange = DXInRange(x, con.range().left().accept(self), con.range().right().accept(self), line=ctx.line_number())
#                 if con.condition():
#                     arange = DXBin('&&', arange, con.condition().accept(self))
#                 eq = DXAll(x, DXLogic("==>",arange,eq), line=ctx.line_number())
#             tmp += [eq]
# #        print('\ntmp in sum', tmp)

# #        num = self.qvars[0].num()
# #        ampvar = makeIndex(DXBind("amp", SType("real"), num),ctx.sums())
#         ampvar = makeIndex(self.currBinds.get('amp'), ctx.sums())
#         v = ctx.amp().accept(self)
#         eq = DXComp("==", ampvar, v, line=ctx.line_number())
#         for con in ctx.sums()[::-1]:
#             x = DXBind(con.ID(), SType("nat"))
#             arange = DXInRange(x, con.range().left().accept(self), con.range().right().accept(self), line=ctx.line_number())
#             if con.condition():
#                     arange = DXBin('&&', arange, con.condition().accept(self))
#             eq = DXAll(x, DXLogic("==>", arange, eq), line=ctx.line_number())

#         if not self.t_ensures:
#             loc, qty, num = subLocus(self.locus, self.varnums)
#             ampl = None
#             if isinstance(qty, TyEn):
#                 ampl = v
#             for i in range(len(loc)):
#                 subd = {}
#                 v = ctx.kets()[i].accept(self)
#                 subd['qty'] = qty
#                 subd['val'] = v
#                 subd['length'] = loc[i].crange().right().ID() if isinstance(loc[i].crange().right(), QXBind) else loc[i].crange().right().num()
#                 subd['amp'] = ampl
#                 toString = TargetToString()
#                 self.initial_locus_data[loc[i].location() + loc[i].crange().left().accept(toString) + ',' + loc[i].crange().right().accept(toString)] = subd
        
# #        self.current_line = tmp_current_line
#         return ([eq]+tmp)

    def visitLogic(self, ctx: Programmer.QXLogic):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXLogic(ctx.op(), left, right, line=ctx.line_number())


    def visitBool(self, ctx: Programmer.QXComp):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXComp(ctx.op(), left, right, line=ctx.line_number())

    def visitBoolLiteral(self, ctx: Programmer.QXBoolLiteral):
        return DXBoolValue(ctx.value())

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        loc, qty, vars = self.currLocus
        v = ctx.index().accept(self)
        #self.conStack += [EnFactor(('==', DXIndex(vars(ctx.ID()), v), DXNum(1)))]
        return DXIndex(vars.get(ctx.ID()), v)
            #return DXIndex(DXBind(ctx.ID(), None, n),ctx.index().accept(self), line=ctx.line_number())
        #return (None, DXIndex(DXBind(ctx.ID(), None, n),ctx.index().accept(self), line=ctx.line_number()))

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
                   , line=left.line())


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
                   , line=left.line())

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
                       invariants, line=left.line())

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
        return ([DXInit(x, line=ctx.line_number()) for x in loopVars] +
                [self.buildWhileBExp(ctx.op(), v1, v2, ind, newVars, qty.flag(), 1, loopVars, tmpVars)])
        #this is not an index, need a way to refer to the gen id
        #result = [DXAssign(DXIndex(ctx.index().ID(), ctx.index().index()),
        #                   DXComp(ctx.op(), DXCall('castBVInt', v1, v2, line=self.current_line)))]

    def visitQNot(self, ctx: Programmer.QXQNot):
        if isinstance(ctx.next(), QXQNot):
            return ctx.next().next().accept(self)

        pred, index = ctx.next().accept(self)
        return (DXNot(pred, line=ctx.line_number()), index)

    def visitAll(self, ctx: Programmer.QXAll):
        x = ctx.bind().accept(self)
        p = ctx.next().accept(self)
        return DXAll(x, p, line=ctx.line_number())

    def visitBin(self, ctx: Programmer.QXBin):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        if ctx.op() == '^' and isinstance(left, DXNum) and left.val() == 2:
            return DXCall('pow2',[ctx.right().accept(self)], line=ctx.line_number())


        if ctx.op() == '/' and isinstance(left, DXNum) and left.val() == 1:
            print('\ntransfer', DXBin(ctx.op(),  DXNum(1), right,line=ctx.line_number()))
            return DXBin(ctx.op(), DXNum(1.0), right, line=ctx.line_number())

        return DXBin(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), line=ctx.line_number())

    def visitIfExp(self, ctx: Programmer.QXIfExp):
        return DXIfExp(ctx.bexp().accept(self), ctx.left().accept(self), ctx.right().accept(self), line=ctx.line_number())

    def visitUni(self, ctx: Programmer.QXUni):

        if ctx.op() == 'sqrt':
            return DXCall('sqrt', [DXCast(SType('real'), ctx.next().accept(self), ctx.line_number())])
        return DXUni(ctx.op(), ctx.next().accept(self), line=ctx.line_number())

    def visitSingle(self, ctx: Programmer.QXSingle):
        print('currlocus, varums', self.currLocus, self.varnums)
        v = subLocus(self.currLocus[0], self.varnums)
        print('visitSingle, v', v)
        if v is not None:
            loc,qty,dvar = v #dvar is the old dict for DxBinds

            if isinstance(qty, TyNor) and ctx.op() == "H":
                vs = compareLocus(self.currLocus[0], loc)
                print('\nvs', vs)
                self.removeLocus(loc)
                newvars = self.upVarsType(dvar, TyHad())
                print('\nself.varnums after remove', self.varnums)
                if vs is None:
                    raise Exception("Error in applying Hadamard, locus not found")
                if not vs:         
                    self.varnums += [(self.currLocus[0], TyHad(), newvars)]
                    print('\n------------self.varnums after op', self.varnums)
                else: 
                    self.counter += 1
                    self.varnums = [(self.currLocus[0], TyHad(),newvars), (vs, TyNor(), dvar)] + self.varnums
                print('\nself.varnums after op', self.varnums, '\nnewvars', newvars, '\ndvar', dvar)
                result = [DXInit(x, line=ctx.line_number()) for x in newvars.values()]
                result += [DXAssign(newvars.values(), DXCall("hadNorHad", list(dvar.values()),
                                                    line=ctx.line_number()),
                                    line=ctx.line_number())]
                self.libFuns.add('hadNorHad')
                self.counter += 1
                print('\nresult in had', result)
                return result

            #this part might need to change, and look from the code for sushen
            if isinstance(qty, TyEn)  and ctx.op() == "H":
                newvars = self.upVarsType(dvar, qty)
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
                                             dvar.values(), line=ctx.line_number()), True,
                                         line=ctx.line_number())]
                #TODO: the above only contain code for update type casting, how about the code to apply Had on a place and get phases?

                self.libFuns.add("En" + str(flagNum) + 'to' + "En" + str(flagNum + 1) + '_' + str(len(loc)))
                result += [DXCall('triggerSqrtMul', [], True, line=ctx.line_number())]
                result += [DXCall('ampeqtrigger', [], True, line=ctx.line_number())]
                result += [DXCall('pow2mul', [], True, line=ctx.line_number())]
                result += [DXCall('pow2sqrt', [], True, line=ctx.line_number())]

                self.libFuns.add('triggerSqrtMul')
                self.libFuns.add('ampeqtrigger')
                self.libFuns.add('pow2mul')
                self.libFuns.add('pow2sqrt')

                # self.removeLocus(num)
                self.counter += 1
                # self.varnums = [(loc,TyEn(QXNum(flagNum + 1)),self.counter)] + self.varnums
                return result

            if isinstance(qty, TyEn) and ctx.op() == "QFT":
                newvars = self.upVarsType(dvar, qty)
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
                oldvars = makeVars(loc, qty, dvar)
                application_oldvars = [x for x in oldvars if x.ID() in applicationids]
                other_oldvars = [x for x in oldvars if x.ID() not in applicationids and x.ID() != 'amp']
                amp_oldvar = [x for x in oldvars if x.ID() == 'amp']
                result = tmr + [DXAssign(other_newvars + application_newvars + amp_newvar,
                                         DXCall(
                                             "QFT_En" + str(flagNum) + 'to' + "En" + str(flagNum + 1) + '_' + str(
                                                 len(loc)),
                                             other_oldvars + application_oldvars + amp_oldvar,
                                             line=ctx.line_number()), True,
                                         line=ctx.line_number())]
                #modify the above

                self.libFuns.add("QFT_En" + str(flagNum) + 'to' + "En" + str(flagNum + 1) + '_' + str(len(loc)))
                result += [DXCall('triggerSqrtMul', [], True, line=ctx.line_number())]
                result += [DXCall('ampeqtrigger', [], True, line=ctx.line_number())]
                result += [DXCall('pow2mul', [], True, line=ctx.line_number())]
                result += [DXCall('pow2sqrt', [], True, line=ctx.line_number())]

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
            return [DXIf(DXComp(op, constructIndex(left, loopVars), right, left.line()),
                         self.buildWhileCondition(loopVars, st, conditions[1:]), [])]
        return []

    def buildOraclePredB(self, v:DXComp, m:int, x:DXAExp, loopVars: [DXBind], tmpVars: [DXBind], countVars : [DXBind]):
        if not loopVars:
            return v
        else:
            return DXAll(tmpVars[0], DXLogic('==>', DXLogic('&&', DXComp('<=', DXNum(0), tmpVars[0]),
                                                            DXComp('<', tmpVars[0], DXLength(constructIndex(x,countVars[0:m])))),
                                      self.buildOraclePredB(v, m+1, x, loopVars[1:], tmpVars[1:],countVars))
                   , line=v.line())


    def buildOraclePredA(self, v:DXComp,m:int, x:DXAExp, loopVars: [DXBind], tmpVars: [DXBind], countVars : [DXBind]):
        if not loopVars:
            return v
        else:
            if m <= 1:
                return self.buildOraclePredB(v, 0, x, loopVars, tmpVars, countVars)
            return DXAll(tmpVars[0], DXLogic('==>',
                                      DXLogic('&&', DXComp('<=', DXNum(0), tmpVars[0]), DXComp('<', tmpVars[0], loopVars[0])),
                                      self.buildOraclePredA(v, m-1,x, loopVars[1:], tmpVars[1:],countVars))
                   , line=v.line())

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
                       invariants, line=st[0].line())

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
            newVars.update({elem.location(): num.get(elem.location()).newBind(self.counter)})
            self.counter += 1

        #substituion for stmts
        endStmts = []

        for i in len(loca):
            thisStmt = ctx.vectors()[i]
            for subst in stmtSubsts:
                thisStmt = subst.visit(thisStmt)
            endStmts += [DXAssign([constructIndex(newVars.get(loca[i].location()), loopVars)],
                                  thisStmt, line=ctx.line_number())]

        thisStmt = ctx.amp()
        for subst in stmtSubsts:
            thisStmt = subst.visit(thisStmt)

        endStmts = ([DXAssign([constructIndex(newVars.get('amp'), loopVars)],
                             thisStmt,line=ctx.line_number())] + endStmts)


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
                                  thisPred, line=ctx.line_number())]

        thisPred = num.get('amp')
        for subst in oldSubsts:
            thisPred = subst.visit(thisPred)
        for subst in indexSubsts:
            thisPred = subst.visit(thisPred)

        startPreds = ([DXComp('==',constructIndex(num.get('amp'), tmpVars),
                              thisPred, line=ctx.line_number())] + startPreds)


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
                                  thisPred, line=ctx.line_number())]

        thisPred = ctx.amp()
        for subst in substs:
            thisPred = subst.visit(thisPred)
        for subst in oldSubsts:
            thisPred = subst.visit(thisPred)
        for subst in indexSubsts:
            thisPred = subst.visit(thisPred)

        endPreds = ([DXComp('==',constructIndex(newVars.get('amp'), tmpVars),
                              thisPred, line=ctx.line_number())] + endPreds)

        values = []
        for key in newVars.keys():
            values += [newVars.get(key), num.get(key)]

        self.libFuns.add('pow2')
        self.libFuns.add('omega')
        self.libFuns.add('sqrt')
        self.libFuns.add('omega0')

        res = ([DXInit(x, line=ctx.line_number()) for x in loopVars] +
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
            return DXCall("omega", [DXNum(0), DXNum(2)], line=ctx.line_number())
        else:
            return DXCall("omega", [DXNum(1), DXNum(2)], line=ctx.line_number())

    def visitQRange(self, ctx: Programmer.QXQRange):
        if ctx.range().right() == DXBin('+', ctx.range().left(), QXNum(1)):
            return QXQIndex(ctx.location(), ctx.range().left(), line=ctx.line_number()).accept(self)

        v = QXBind(ctx.location(), line=ctx.line_number()).accept(self)
        return v
    
    def visitVarState(self, ctx):
        pass
