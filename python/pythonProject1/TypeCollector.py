import Programmer
from ProgramVisitor import ProgramVisitor
from CollectKind import *

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

# check the types of the quantum array (nor, Had, EN types)
class TypeCollector(ProgramVisitor):

    def __init__(self, kenv: dict):
        # need st --> state we are deling with
        self.kenv = kenv
        self.env = dict()
        self.tenv = []
        self.mkenv = []
        self.pred = []
        self.fvar = ""


    def visitMethod(self, ctx: Programmer.QXMethod):
        self.fvar = ctx.ID()
        self.tenv = []
        self.mkenv = []
        self.pred = []

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
        for elem in ctx.spec().locus():
            x = elem.ID().ID()
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

        self.tenv.append((ctx.spec().locus(),ctx.spec().qty()))
        return True


    def visitEnsures(self, ctx: Programmer.QXEnsures):
        for elem in ctx.spec().locus():
            x = elem.ID().ID()
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