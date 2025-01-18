import Programmer
from ProgramVisitor import ProgramVisitor
from CollectKind import *

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



# collect the types of the quantum array (nor, Had, EN types)
class TypeCollector(ProgramVisitor):

    def __init__(self, kenv: dict):
        # need st --> state we are dealing with
        self.kenv = kenv
        self.env = dict()
        self.tenv = []
        self.mkenv = []
        self.pred = []
        self.fkenv = None
        self.fvar = ""


    def visitMethod(self, ctx: Programmer.QXMethod):
        self.fvar = str(ctx.ID())
        self.tenv = []
        self.mkenv = []
        self.pred = []
        self.fkenv = self.kenv.get(self.fvar)

        for condelem in ctx.conds():
            v = condelem.accept(self)
            if not v:
                return False


        tmptv = []
        vars = isARange(self.tenv, self.fkenv[0])
        for var in vars:
            if isinstance(self.fkenv[0].get(var),TyQ):
                v = self.fkenv.get(var).flag()
                locus = [QXQRange(var, QXCRange(QXNum(0), v))]
                ty = TyEn(QXNum(1))
                tmptv += [(locus,ty)]

        tmpfv = []
        fkenvMerge = merge_two_dicts(self.fkenv[0], self.fkenv[1])
        vars = isARange(self.mkenv, fkenvMerge.keys())
        for var in vars:
            if isinstance(fkenvMerge.get(var),TyQ):
                v = self.fkenv.get(var).flag()
                locus = [QXQRange(var, QXCRange(QXNum(0), v))]
                ty = TyEn(QXNum(1))
                tmpfv += [(locus,ty)]

        self.tenv += tmptv
        self.mkenv += tmpfv

        self.env.update({self.fvar:(self.tenv,self.mkenv,self.pred)})


        return True

    def visitProgram(self, ctx: Programmer.QXProgram):
        for elem in ctx.method():
            v = elem.accept(self)
            if not v:
                return False

        return True

    def visitRequires(self, ctx: Programmer.QXRequires):
        if not isinstance(ctx.spec(), QXQSpec):
            return True
        for elem in ctx.spec().locus():
            x = str(elem.ID())
            left = elem.crange().left()
            right = elem.crange().right()
            v = self.kenv.get(self.fvar)[0].get(x)
            if isinstance(v, TyQ):
                if not compareAExp(left, QXNum(0)):
                    addElem((QXComp("<=",QXNum(0),left)), self.pred)
                if not compareAExp(right, v.flag()):
                    addElem((QXComp("<=",right,v.flag())), self.pred)
            else:
                return False

        self.tenv.append((ctx.spec().locus(), ctx.spec().qty()))
        return True


    def visitEnsures(self, ctx: Programmer.QXEnsures):
        if not isinstance(ctx.spec(), QXQSpec):
            return True
        for elem in ctx.spec().locus():
            x = str(elem.ID())
            left = elem.crange().left()
            right = elem.crange().right()
            v = self.kenv.get(self.fvar)[0].get(x)
            if isinstance(v, TyQ):
                if not compareAExp(left, QXNum(0)):
                    addElem((QXComp("<=",QXNum(0),left)), self.pred)
                if not compareAExp(right, v.flag()):
                    addElem((QXComp("<=",right,v.flag())), self.pred)
            else:
                return False

        self.mkenv.append((ctx.spec().locus(),ctx.spec().qty()))
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