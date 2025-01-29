import Programmer
from ProgramVisitor import ProgramVisitor
from CollectKind import *

from TypeChecker import *

from antlr4.tree.Tree import TerminalNodeImpl

def compareComp(t1: QXComp, t2: QXComp):
    return t1.op() == t2.op() and compareAExp(t1.left(),t2.left()) and compareAExp(t1.right(),t2.right())


def addElem(a:QXComp, l:[QXComp]):
    v = False
    for elem in l:
        if compareComp(a,elem):
            v = True
    if v:
        return l
    return l.append(a)

def isARange(env: [([QXQRange], QXQTy)], se:[str]):
    tmp = []
    for elem,ty in env:
        for v in elem:
            if not v.ID() in se:
                tmp += [v.ID()]
    return tmp

def merge_two_dicts(x, y):
    z = x.copy()   # start with keys and values of x
    z.update(y)    # modifies z with keys and values of y
    return z

def findQAVars(v: QXAExp, vars: [str]):

    if isinstance(v, QXUni):
        return findQAVars(v.next(), vars)

    if isinstance(v, QXBin):
        return (findQAVars(v.left(),vars) + findQAVars(v.right(),vars))

    if isinstance(v, QXNum):
        return []

    if isinstance(v, QXBind):
        if v.ID() in vars:
            return [v.ID()]
        else:
            return []

def findQVars(v: QXBool, vars: [str]):

    if isinstance(v, QXLogic):
        return (findQVars(v.left(),vars) + findQVars(v.right(),vars))

    if isinstance(v, QXCNot):
        return findQVars(v.next(), vars)

    if isinstance(v, QXComp):
        return (findQAVars(v.left(),vars) + findQAVars(v.right(),vars))

def subStrs(a: [str], b:[str]):
    for elem in a:
        if not elem in b:
            return False
    return True

# collect the types of the quantum array (nor, Had, EN types)
# For each method, we have a map from the function name to three things
# We first have a initial type env, grouping loci together
# This is based on analysis of requires
# Then, we will have a endinng type env, grouping another possible loci
# This is to analyze the ensures
# The third field is a predicate. Each predicate collect the implicit
# rules in the locus type
# for example, if we have a range x[i,j) in a locus, obviously, i <= j
class TypeCollector(ProgramVisitor):

    def __init__(self, kenv: dict):
        # need st --> state we are dealing with
        self.kenv = kenv
        self.env = dict()
        self.tenv = [] # temp pre-env for a method
        self.mkenv = [] # temp post-env for a method
        self.pred = [] # temp predicates
        self.fkenv = None
        self.fvar = ""

    def createRequireVars(self, l:[QXCond]):

        tmp = []
        for var , kty in self.fkenv[0].items():
            if isinstance(kty, TyQ):
                tmp += [var]

        vars = tmp.copy()

        for elem in l:
            if isinstance(elem, QXRequires):
                if isinstance(elem.spec(), QXQSpec):
                    for ran in elem.spec().locus():
                        if ran.ID() in tmp:
                            tmp.remove(ran.ID())
                        else:
                            return None
                if isinstance(elem.spec(), QXBool):
                    tmvars = findQVars(elem.spec(), vars)
                    if not subStrs(tmvars, tmp):
                        for tmpelem in tmvars:
                            tmp.remove(tmpelem)

        result = []

        for var in tmp:
            v = self.fkenv[0].get(var).flag()
            locus = [QXQRange(var, QXCRange(QXNum(0), v))]
            ty = TyEn(QXNum(1))
            result += [(locus, ty)]

        return result

    def createEnsureVars(self, l:[QXCond]):

        tmp = []
        for var , kty in self.fkenv[0].items():
            if isinstance(kty, TyQ):
                tmp += [var]

        vars = tmp.copy()

        for elem in l:
            if isinstance(elem, QXEnsures):
                if isinstance(elem.spec(), QXQSpec):
                    for ran in elem.spec().locus():
                        if ran.ID() in tmp:
                            tmp.remove(ran.ID())
                        else:
                            return None
                if isinstance(elem.spec(), QXBool):
                    tmvars = findQVars(elem.spec(), vars)
                    if not subStrs(tmvars, tmp):
                        for tmpelem in tmvars:
                            tmp.remove(tmpelem)

        result = []

        for var in tmp:
            v = self.fkenv[0].get(var).flag()
            locus = [QXQRange(var, QXCRange(QXNum(0), v))]
            ty = TyEn(QXNum(1))
            result += [(locus, ty)]

        return result


    def findLocus(self, locus: [QXQRange]):

        for elem, qty in self.tenv:
            vs = compareLocus(locus, elem)
            if vs == []:
                return qty
        return None

    def visitMethod(self, ctx: Programmer.QXMethod):
        self.fvar = str(ctx.ID())
        self.tenv = []
        self.mkenv = []
        self.pred = []
        self.fkenv = self.kenv.get(self.fvar)

        self.tenv += self.createRequireVars(ctx.conds())
        self.mkenv += self.createEnsureVars(ctx.conds())

        for condelem in ctx.conds():
            v = condelem.accept(self)
            if not v:
                return False

        self.env.update({self.fvar:(self.tenv,self.mkenv,self.pred)})

        return True

    def visitProgram(self, ctx: Programmer.QXProgram):
        for elem in ctx.method():
            v = elem.accept(self)
            if not v:
                return False

        return True

    def visitRequires(self, ctx: Programmer.QXRequires):
        if isinstance(ctx.spec(), QXQSpec):
            for elem in ctx.spec().locus():
                x = str(elem.ID())
                kty = self.fkenv[0].get(x)
                if not isinstance(kty, TyQ):
                    return False

                left = elem.crange().left()
                right = elem.crange().right()

                if not left.accept(self) or not right.accept(self):
                    return False

                if not compareAExp(left, QXNum(0)):
                    addElem((QXComp("<=",QXNum(0),left)), self.pred)
                if not compareAExp(right, kty.flag()):
                    addElem((QXComp("<=",right,kty.flag())), self.pred)

            self.tenv.append((ctx.spec().locus(), ctx.spec().qty()))

        if isinstance(ctx.spec(), QXComp):
            left = ctx.spec().left().ID()
            right = ctx.spec().right().ID()

            tyx = self.fkenv[0].get(left)
            tyy = self.fkenv[0].get(right)

            if isinstance(tyx, TyQ) and isinstance(tyy, TyQ):
                xT = self.findLocus([QXQRange(left, QXCRange(QXNum(0),tyx.flag()))])
                yT = self.findLocus([QXQRange(right, QXCRange(QXNum(0),tyy.flag()))])

                if xT is None and yT is None:
                    return False
                elif xT is None:
                    self.tenv.append([QXQRange(left, QXCRange(QXNum(0),tyx.flag()))], yT)
                elif yT is None:
                    self.tenv.append([QXQRange(left, QXCRange(QXNum(0), tyx.flag()))], xT)
                else:
                    re = compareType(xT, yT)
                    if re is None:
                        return False
        return True


    def visitEnsures(self, ctx: Programmer.QXEnsures):
        if isinstance(ctx.spec(), QXQSpec):
            for elem in ctx.spec().locus():
                x = str(elem.ID())
                kty = self.fkenv[0].get(x)
                if not isinstance(kty, TyQ):
                    return False

                left = elem.crange().left()
                right = elem.crange().right()

                if not left.accept(self) or not right.accept(self):
                    return False

                if not compareAExp(left, QXNum(0)):
                    addElem((QXComp("<=",QXNum(0),left)), self.pred)
                if not compareAExp(right, kty.flag()):
                    addElem((QXComp("<=",right,kty.flag())), self.pred)

            self.tenv.append((ctx.spec().locus(), ctx.spec().qty()))

        if isinstance(ctx.spec(), QXComp):
            left = ctx.spec().left().ID()
            right = ctx.spec().right().ID()

            tyx = self.fkenv[0].get(left)
            tyy = self.fkenv[0].get(right)

            if isinstance(tyx, TyQ) and isinstance(tyy, TyQ):
                xT = self.findLocus([QXQRange(left, QXCRange(QXNum(0),tyx.flag()))])
                yT = self.findLocus([QXQRange(right, QXCRange(QXNum(0),tyy.flag()))])

                if xT is None and yT is None:
                    return False
                elif xT is None:
                    self.tenv.append([QXQRange(left, QXCRange(QXNum(0),tyx.flag()))], yT)
                elif yT is None:
                    self.tenv.append([QXQRange(left, QXCRange(QXNum(0), tyx.flag()))], xT)
                else:
                    re = compareType(xT, yT)
                    if re is None:
                        return False
        return True

    def visitUni(self, ctx: Programmer.QXUni):
        return ctx.next().accept(self)

    def visitBin(self, ctx: Programmer.QXBin):
        return ctx.left().accept(self) and ctx.right().accept(self)

    def visitBind(self, ctx: Programmer.QXBind):
        ty = self.fkenv[0].get(ctx.ID())
        if isinstance(ty, TySingle):
            return ty.type() == "nat"

    def visitNum(self, ctx: Programmer.QXNum):
        return True

    def get_env(self):
        return self.env

    def get_tenv(self, method_name: str | TerminalNodeImpl) -> [([QXQRange], QXQTy, int)]:
        """Returns the requires type environment associated with a particular method name (either str or antlr4.tree.Tree.TerminalNodeImpl)"""
        if isinstance(method_name, TerminalNodeImpl):
            method_name = str(method_name)

        return self.env[method_name][0]

    def get_mkenv(self, method_name: str | TerminalNodeImpl) -> [([QXQRange], QXQTy, int)]:
        """Returns the ensures type environment associated with a particular method name (either str or antlr4.tree.Tree.TerminalNodeImpl)"""
        if isinstance(method_name, TerminalNodeImpl):
            method_name = str(method_name)

        return self.env[method_name][1]