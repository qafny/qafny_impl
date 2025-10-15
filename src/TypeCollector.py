import Programmer
from ProgramVisitor import ProgramVisitor
from CollectKind import *

from TypeChecker import *

from antlr4.tree.Tree import TerminalNodeImpl

def compareComp(t1: QXComp, t2: QXComp):
    return t1.op() == t2.op() and compareAExp(t1.left(),t2.left()) and compareAExp(t1.right(),t2.right())


# def addElem(a:QXComp, l:[QXComp], isRequires:bool):
#     v = False
#     for elem in l:
#         if compareComp(a,elem.spec()):
#             v = True
#     if v:
#         return l
#     a = QXRequires(a) if isRequires else QXEnsures(a)
#     return l.append(a)

def addElem(a: QXComp, l: [QXComp], wrapper_class: QXCond):
    """
    Adds a predicate to a list, avoiding duplicates.
    Wraps the predicate in the provided wrapper_class (e.g., QXRequires).
    """
    is_duplicate = any(compareComp(a, elem.spec()) for elem in l if isinstance(elem.spec(), QXComp))
    if not is_duplicate:
        # Instantiate the provided class directly with the predicate
        wrapped_pred = wrapper_class(a)
        l.append(wrapped_pred)
    return l

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

def findQAVars(v: QXAExp, vars: [str]) -> list[str]:

    if isinstance(v, QXUni):
        return findQAVars(v.next(), vars)

    elif isinstance(v, QXBin):
        print(f'\n v: {v}')
        left_vars = findQAVars(v.left(), vars)
        right_vars = findQAVars(v.right(), vars)
        return left_vars + right_vars


    elif isinstance(v, QXNum):
        return []

    elif isinstance(v, QXBind):
        if v.ID() in vars:
            return [v.ID()]
        else:
            return []
    elif isinstance(v, QXCall):
        return v.exps()
    
    
    elif isinstance(v, QXLogic) or isinstance(v, QXCNot) or isinstance(v, QXComp):
        return findQVars(v, vars)
    else:
        return []

def findQVars(v: QXBool, vars: [str]):

    if isinstance(v, QXLogic):
        return (findQVars(v.left(),vars) + findQVars(v.right(),vars))

    if isinstance(v, QXCNot):
        return findQVars(v.next(), vars)

    if isinstance(v, QXComp):
        print(f"\n findQVars: {v} in vars {vars}")
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
        self.kenv = kenv # mapping from function names -> two items,
        #the two items are both maps from variables to kinds
        self.env = dict()
        # mapping from function names -> three items, types
        # TyEn, TyNor, TyHad, TyAA
        # the first two items mapping from loci -> types
        # the third item is a list of predicates
        self.tenv = [] # temp pre-env for a method
        self.tmpenv = []
        self.mkenv = [] # temp post-env for a method
        self.pred = [] # temp predicates
        self.fkenv = None
        self.fvar = ""
    
    def _create_default_vars(self, conditions: list, condition_type):
        """Helper for createRequireVars and createEnsureVars"""
        # Start with all quantum variables
        quantum_vars = {var for var, kty in self.fkenv[0].items() 
                    if isinstance(kty, TyQ)}
        specified_vars = set()
        
        for elem in conditions:
            if not isinstance(elem, condition_type):
                continue
    #        print(f"\n elem in _create_default_vars: {elem} with condition_type {condition_type}")
            if isinstance(elem.spec(), QXQSpec):
                # Mark variables that have explicit loci
                for ran in elem.spec().locus():
                    loc = ran.location()
                    if loc in quantum_vars:
                        specified_vars.add(loc)
                    # Don't fail if loc not in quantum_vars - might be classical
                        
            elif isinstance(elem.spec(), QXBool):
                # Boolean constraints might implicitly specify some variables
                # This logic needs clarification of the intended semantics
                referenced_vars = findQVars(elem.spec(), list(quantum_vars))
                specified_vars.update(referenced_vars)
        
        # Create default loci for unspecified quantum variables
        result = []
        if condition_type == QXInvariant:     
            for var in quantum_vars - specified_vars:
                flag = self.fkenv[0][var].flag()
                locus = [QXQRange(var, crange=QXCRange(QXNum(0), flag))]
                ty = TyEn(QXNum(1))
                result.append((locus, ty))
        
        return result

    def createRequireVars(self, l: list):
        return self._create_default_vars(l, QXRequires)

    def createEnsureVars(self, l: list):
        return self._create_default_vars(l, QXEnsures)
    
    def createInvVars(self, l:list):
        return self._create_default_vars(l, QXInvariant)

    def findLocus(self, locus: [QXQRange]):

        for elem, qty in self.tenv:
            vs = compareLocus(locus, elem)
            if vs == []:
                return qty
        return None
    
    def _visit_spec(self, ctx, spec_type: QXCond):
        """Shared logic for visitRequires visitInvariants, and visitEnsures"""
        wrapper_map = {
        "requires": QXRequires,
        "ensures": QXEnsures,
        "invariant": QXInvariant
        }
        wrapper_class = wrapper_map.get(spec_type)
        if not wrapper_class:
            raise ValueError(f"Unknown specification type: {spec_type}")

        if spec_type == "requires":
            target_env = self.tenv
        elif spec_type == "ensures":
            target_env = self.mkenv
        elif spec_type == "invariant":
            target_env = self.tmpenv # *** THE FIX: Use self.tmpenv ***
        else:
            return False # Should not happen

#        target_env = self.tenv if is_requires else self.mkenv
        
        if isinstance(ctx.spec(), QXQSpec):
            for elem in ctx.spec().locus():
                x = str(elem.location())
                kty = self.fkenv[0].get(x)
                if not isinstance(kty, TyQ):
                    return False

                left = elem.crange().left()
                right = elem.crange().right()

                if not left.accept(self) or not right.accept(self):
                    return False
                    
                # Add range predicates
                if not compareAExp(left, QXNum(0)):
                    addElem(QXComp("<=", QXNum(0), left), self.pred, wrapper_class)
                if not compareAExp(right, kty.flag()):
                    addElem(QXComp("<=", right, kty.flag()), self.pred, wrapper_class)
                    
            target_env.append((ctx.spec().locus(), ctx.spec().qty()))
            return True

        elif isinstance(ctx.spec(), QXQComp):
            # Safe ID extraction (same for both)
            left = ctx.spec().left().ID() if isinstance(ctx.spec().left(), QXBind) else ctx.spec().left()
            right = ctx.spec().right().ID() if isinstance(ctx.spec().right(), QXBind) else ctx.spec().right()

            tyx = self.fkenv[0].get(left)
            tyy = self.fkenv[0].get(right)
            
            if tyx is None or tyy is None:
                return False

            if isinstance(tyx, TyQ) and isinstance(tyy, TyQ):
                xT = self.findLocus([QXQRange(left, crange=QXCRange(QXNum(0), tyx.flag()))])
                yT = self.findLocus([QXQRange(right, crange=QXCRange(QXNum(0), tyy.flag()))])

                if xT is None and yT is None:
                    return False
                elif xT is None:
                    target_env.append(([QXQRange(left, crange=QXCRange(QXNum(0), tyx.flag()))], yT))
                elif yT is None:
                    target_env.append(([QXQRange(right, crange=QXCRange(QXNum(0), tyy.flag()))], xT))
                else:
                    re = compareType(xT, yT)
                    if re is None:
                        return False
            return True
        
        elif isinstance(ctx.spec(), QXComp):
            addElem(ctx.spec(), self.pred, wrapper_class)
            return True


    def visitMethod(self, ctx: Programmer.QXMethod):
        self.fvar = str(ctx.ID())
        self.tenv = []
        self.mkenv = []
        self.pred = []
        self.fkenv = self.kenv.get(self.fvar)

        if self.fkenv is None:
            print(f"Error: Kind environment not found for method '{self.fvar}'")
            return False

        self.tenv += self.createRequireVars(ctx.conds())
        self.mkenv += self.createEnsureVars(ctx.conds())

        for condelem in ctx.conds():
            v = condelem.accept(self)
            if not v:
                return False

        self.env[self.fvar] = (self.tenv, self.mkenv, self.pred)

        return True

    def visitProgram(self, ctx: Programmer.QXProgram):
        for elem in ctx.topLevelStmts():
            v = elem.accept(self)
            if not v:
                return False

        return True
    
    def visitRequires(self, ctx: Programmer.QXRequires):
        return self._visit_spec(ctx, spec_type='requires')

    def visitEnsures(self, ctx: Programmer.QXEnsures):
        return self._visit_spec(ctx, spec_type='ensures')
    
    def visitInvariant(self, ctx):
        return self._visit_spec(ctx, spec_type='invariant')

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
    
    def visitFun(self, ctx: Programmer.QXCall):
        for elem in ctx.exps():
    #        print(f"\n elem {elem}")
            return elem.accept(self)

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
    
    def get_tmpenv(self):
        return self.tmpenv
    
    def pop_tmpenv(self):
        self.tmpenv = []