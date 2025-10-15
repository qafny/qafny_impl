# tools/pattern_subst_daexp.py
from TargetProgramVisitor import TargetProgramVisitor
from TargetProgrammer import *
from AbstractTargetVisitor import AbstractTargetVisitor
from TypeChecker import *
from dataclasses import dataclass
from typing import Callable, List, Optional, Any

# ---------- metadata helpers  ----------


# ---------- light-weight shape predicates ----------
def is_num(node: DXAExp, value: float) -> bool:
    if not isinstance(node, DXNum): return False
    try:
        # DXNum may store string/decimal; normalize
        v = node.num() if hasattr(node, "num") else node.val()
        v = float(v)
    except Exception:
        return False
    return v == float(value)

def is_call(node: DXAExp, name: str, arity: Optional[int] = None) -> bool:
    if not isinstance(node, DXCall): return False
    if node.ID() != name: return False
    return True if arity is None else len(node.exps()) == arity

def is_cast_to_real(node: DXAExp) -> bool:
    if not isinstance(node, DXCast): return False
    t = node.type()
    return str(t).lower() == "real"


def rule_invpow2(ctx: DXAExp) -> Optional[DXAExp]:
    """
    1.0 / sqrt(pow2(i) as real)  =>  invPow2(i)
    Also matches without the cast-to-real wrapper.
    """
    if not isinstance(ctx, DXBin) or ctx.op() != "/": return None
    if not is_num(ctx.left(), 1.0): return None
    right = ctx.right()
    if not is_call(right, "sqrt", 1): return None
    (arg,) = right.exps()
    inner = arg
    if not is_call(inner, "pow2", 1): return None
    (i_expr,) = inner.exps()
    return DXCall("invPow2", [i_expr], False, line=ctx.line())

# ---------- generic pattern rewriter ----------
RuleFn = Callable[[DXAExp], Optional[DXAExp]]

class PatternSubstDAExp(TargetProgramVisitor):
    """
    Pattern-based expression substitution.
    
    """

    def __init__(self, rules: Optional[List[RuleFn]] = None):
        self._rules = list(rules) if rules else [rule_invpow2]

    def _apply(self, expr: DXAExp) -> DXAExp:
        for rule in self._rules:
            repl = rule(expr)
            if repl is not None:
                return repl
        return expr

    # ===== Expressions =====
    def visitBin(self, ctx: DXBin):
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return self._apply(DXBin(ctx.op(), l, r, line=ctx.line()))

    def visitUni(self, ctx: DXUni):
        n = ctx.next().accept(self)
        return self._apply(DXUni(ctx.op(), n, line=ctx.line()))

    def visitBind(self, ctx: DXBind): return ctx
    def visitNum(self, ctx: DXNum):   return ctx
    def visitReal(self, ctx: DXReal): return ctx

    def visitIndex(self, ctx: DXIndex):
        b = ctx.bind().accept(self)
        i = ctx.index().accept(self)
        return self._apply(DXIndex(b, i, line=ctx.line()))

    def visitCall(self, ctx: DXCall):
    #    print(f'ctx in PS {ctx}')
        args = [e.accept(self) for e in ctx.exps()]
        return self._apply(DXCall(ctx.ID(), args, ctx.end(), line=ctx.line()))

    def visitAll(self, ctx: DXAll):
        b = ctx.bind().accept(self)
        n = ctx.next().accept(self)
        return self._apply(DXAll(b, n, line=ctx.line()))

    def visitComp(self, ctx: DXComp):
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return self._apply(DXComp(ctx.op(), l, r, line=ctx.line()))

    def visitLogic(self, ctx: DXLogic):
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return self._apply(DXLogic(ctx.op(), l, r, line=ctx.line()))

    def visitNot(self, ctx: DXNot):
        n = ctx.next().accept(self)
        return self._apply(DXNot(n, line=ctx.line()))

    def visitInRange(self, ctx: DXInRange):
        b = ctx.bind().accept(self)
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return self._apply(DXInRange(b, l, r, line=ctx.line()))

    def visitIfExp(self, ctx: DXIfExp):
        b = ctx.bexp().accept(self)
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return self._apply(DXIfExp(b, l, r, line=ctx.line()))

    def visitCast(self, ctx: DXCast):
        n = ctx.next().accept(self)
        return self._apply(DXCast(ctx.type(), n, line=ctx.line()))

    def visitLength(self, ctx: DXLength):
        v = ctx.var().accept(self)
        return self._apply(DXLength(v, line=ctx.line()))

    # ===== Statements / Specs =====
    def visitAssert(self, ctx: DXAssert):
        s = ctx.spec().accept(self)
        return DXAssert(s, line=ctx.line())

    def visitAssign(self, ctx: DXAssign):
        ids = [i.accept(self) for i in ctx.ids()]
        exp = ctx.exp().accept(self)
        return DXAssign(ids, exp, ctx.init(), line=ctx.line())

    def visitEnsures(self, ctx: DXEnsures):
        s = ctx.spec().accept(self)
        return DXEnsures(s, line=ctx.line())

    def visitIf(self, ctx: DXIf):
        c = ctx.cond().accept(self)
        l = [s.accept(self) for s in ctx.left()]
        r = [s.accept(self) for s in ctx.right()]
        return DXIf(c, l, r, line=ctx.line())

    def visitInit(self, ctx: DXInit):
        b = ctx.binding().accept(self)
        e = ctx.exp().accept(self) if ctx.exp() else None
        return DXInit(b, e, line=ctx.line())

    def visitWhile(self, ctx: DXWhile):
        c = ctx.cond().accept(self)
        s = [st.accept(self) for st in ctx.stmts()]
        i = [inv.accept(self) for inv in ctx.inv()]
        return DXWhile(c, s, i, line=ctx.line())

    # ===== Types / Program =====
    def visitFunType(self, ctx: FunType):   return ctx
    def visitSeqType(self, ctx: SeqType):   return ctx
    def visitSType(self, ctx: SType):       return ctx
    def visitMethod(self, ctx: DXMethod):   return ctx
    def visitProgram(self, ctx: DXProgram): return ctx
