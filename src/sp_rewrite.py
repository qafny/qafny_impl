from __future__ import annotations
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

from sp_terms import IApply, IIter, ILoopSummary, IStep, ICtrlGate, ITensorProd, IPostSelect
from sp_pretty import pp
from SubstAExp import SubstAExp
from ProgramVisitor import ProgramVisitor
# -------------------------
# Tiny duck-typing helpers
# -------------------------

def _cn(x: Any) -> str:
    return getattr(x, "__class__", type(x)).__name__

def _call0(obj: Any, name: str, default=None):
    f = getattr(obj, name, None)
    if callable(f):
        try:
            return f()
        except Exception:
            return default
    return getattr(obj, name, default)

def _as_tuple(xs: Any) -> Tuple[Any, ...]:
    if xs is None:
        return ()
    if isinstance(xs, tuple):
        return xs
    if isinstance(xs, list):
        return tuple(xs)
    return (xs,)

def _same_targets(t1: Tuple[Any, ...], t2: Tuple[Any, ...]) -> bool:
    if len(t1) != len(t2):
        return False
    return all(repr(a) == repr(b) for a, b in zip(t1, t2))

def _op_name(op: Any) -> Optional[str]:
    if _cn(op) == "QXSingle":
        return str(_call0(op, "op"))
    return None

def _is_ket_val(term: Any, val: int|str) -> bool:
    if _cn(term) == "QXTensor":
        ks = _call0(term, "kets", None)
        if not isinstance(ks, (list, tuple)) or len(ks) != 1: return False
        term = ks[0]
    if _cn(term) == "QXSKet":
        v = _call0(term, "vector", None)
        try:
            return _cn(v) == "QXNum" and int(_call0(v, "num", -1)) == val
        except: return False
    return False

def _is_ket0(term: Any) -> bool:
    return _is_ket_val(term, 0)

def _is_ket1(term: Any) -> bool:
    return _is_ket_val(term, 1)

def _is_ket_minus(term: Any) -> bool:
    # Check for Apply(H, |1>) or equivalent
    if _cn(term) == "IApply" and _is_H(term.op):
        return _is_ket_val(term.term, 1)
    
    if _cn(term) == "QXTensor":
        ks = _call0(term, "kets", None)
        if not isinstance(ks, (list, tuple)) or len(ks) != 1: return False
        term = ks[0]
    if _cn(term) == "QXSKet":
        v = _call0(term, "vector", None)
        if _cn(v) == "QXHad":
             state = _call0(v, "state", "")
             return state == "-"
    return False

def _is_H(op: Any) -> bool:
    return _op_name(op) == "H"

def _is_increment_expr(expr: Any) -> bool:
    """Checks if expr is (x + 1) or (1 + x)."""
    if _cn(expr) != "QXBin":
        return False
    op = str(_call0(expr, "op"))
    if op != "+":
        return False
    l = _call0(expr, "left")
    r = _call0(expr, "right")
    try:
        # Check left + 1
        if _cn(r) == "QXNum" and int(_call0(r, "num", -1)) == 1:
            return True
        # Check 1 + right
        if _cn(l) == "QXNum" and int(_call0(l, "num", -1)) == 1:
            return True
    except:
        return False
    return False

# -------------------------
# AST constructors (robust)
# -------------------------

def _mk_bind(name: str):
    from Programmer import QXBind
    return QXBind(id=name)

def _mk_num(i: int):
    from Programmer import QXNum
    return QXNum(num=i)

def _mk_bin(op: str, left: Any, right: Any):
    from Programmer import QXBin
    return QXBin(op=op, left=left, right=right)

def _mk_uni(op: str, nxt: Any):
    from Programmer import QXUni
    return QXUni(op=op, next=nxt)

def _mk_crange(lo: Any, hi: Any):
    from Programmer import QXCRange
    return QXCRange(left=lo, right=hi)

def _mk_con(id_: str, cr: Any, cond: Any = None):
    from Programmer import QXCon
    try:
        return QXCon(id=id_, crange=cr, condtion=cond)
    except TypeError:
        return QXCon(id=id_, crange=cr, condition=cond)

def _mk_sket(v: Any):
    from Programmer import QXSKet
    return QXSKet(vector=v)

def _mk_tensor(kets: List[Any]):
    from Programmer import QXTensor
    return QXTensor(kets=kets, id='None', crange=None)

def _mk_sum(cons: List[Any], amp: Any, tensor: Any):
    from Programmer import QXSum
    return QXSum(sums=cons, amp=amp, tensor=tensor)


# -------------------------
# Alpha-canonicalization for QXSum
# -------------------------

def _subst_bind(expr: Any, mapping: Dict[str, str]) -> Any:
    """
    Rename QXBind occurrences inside an expression/kets tree using SubstAExp.
    """
    if expr is None: return None
    
    res = expr
    for src, tgt in mapping.items():
        from Programmer import QXBind, QXNum
        print(f"scr, tgt {src}, {tgt}")
        if isinstance(tgt, int):
            node = QXNum(num=int(tgt))
        elif isinstance(tgt, str):
            node = QXBind(id=tgt)
        else:
            node = tgt
        visitor = SubstAExp(src, node)
        
        if hasattr(res, "accept"):
            res = res.accept(visitor)
    return res

def canon_sum(term: Any) -> Any:
    if _cn(term) != "QXSum":
        return term

    cons = list(_call0(term, "sums", []) or [])
 
    if len(cons) != 1 or _cn(cons[0]) != "QXCon":
        return term

    old = str(_call0(cons[0], "ID"))
    new = "k#0"
    if old == new:
        return term

    mapping = {old: new}

    cr = _call0(cons[0], "crange")
    cond = _call0(cons[0], "condtion", None)
    con2 = _mk_con(new, cr, _subst_bind(cond, mapping) if cond is not None else None)

    amp2 = _subst_bind(_call0(term, "amp"), mapping)
    kets2 = _subst_bind(_call0(term, "kets"), mapping)

    return _mk_sum([con2], amp2, kets2)


# -------------------------
# Local rewrite rules
# -------------------------

def _rewrite_apply_cancel(op: Any, tgt: Tuple[Any, ...], inner: Any) -> Optional[Any]:
    if not _is_H(op):
        return None
    if not isinstance(inner, IApply):
        return None
    if not _is_H(inner.op):
        return None
    if not _same_targets(tgt, inner.target):
        return None
    return inner.term

def _rewrite_apply_expand_H0(op: Any, tgt: Tuple[Any, ...], inner: Any) -> Optional[Any]:
    if not _is_H(op):
        return None
    if not _is_ket0(inner) and not not _is_ket0(inner):
        return None
    if len(tgt) != 1 or _cn(tgt[0]) != "QXQRange":
        return None

    qrange = tgt[0]
    try:
        cr = _call0(qrange, "crange")
        lo = _call0(cr, "left")
        hi = _call0(cr, "right")
    except Exception:
        return None

    width = hi
    if _cn(lo) == "QXNum" and int(_call0(lo, "num", -1)) == 0:
        width = hi
    else:
        width = _mk_bin("-", hi, lo)

    if _is_ket1(inner):
        from Programmer import QXHad
        vis = SubstAExp("1", QXHad("-"))
        ket = vis.visit(inner)
        return ket

    two_pow = _mk_bin("^", _mk_num(2), width)
    amp = _mk_bin("/", _mk_num(1), _mk_uni("sqrt", two_pow))
    con = _mk_con("k#0", _mk_crange(_mk_num(0), two_pow), None)
    ket = _mk_tensor([_mk_sket(_mk_bind("k#0"))])
    return _mk_sum([con], amp, ket)

def _rewrite_tensor_sum_with_ket0(term: Any) -> Optional[Any]:
    """
    Rewrite ITensorProd(Sum, Ket) -> Sum(|k>|0>)
    """
    if not isinstance(term, ITensorProd):
        return None
    print(f"\n term: {term}")
    fs = term.factors
    if len(fs) != 2:
        return None
    left, right = fs
    
    # Try normalizing left side to Sum if it's IApply(H)
    if _cn(left) == "IApply" and _is_H(left.op):
        left_expanded = _rewrite_apply_expand_H0(left.op, left.target, left.term)
        if left_expanded:
            left = left_expanded

    if _cn(left) != "QXSum": return None
    
    if not _is_ket0(right) and (not _is_ket1(right)): return None

    left = canon_sum(left)

    cons = list(_call0(left, "sums", []) or [])
    if len(cons) != 1 or _cn(cons[0]) != "QXCon":
        return None

    amp = _call0(left, "amp")
    kets = _call0(left, "kets")
    if _cn(kets) != "QXTensor": return None
    ks = list(_call0(kets, "kets", []) or [])
    if len(ks) != 1: return None
    if _is_ket0(right): joint = _mk_tensor([ks[0], _mk_sket(_mk_num(0))])
    elif _is_ket1(right): joint = _mk_tensor([ks[0], _mk_sket(_mk_num(1))])
    return _mk_sum(cons, amp, joint)

def _rewrite_apply_collapse_sum(op: Any, tgt: Tuple[Any, ...], inner: Any) -> Optional[Any]:
    """
    Inverse of expand_H0: Apply[H]( Sum_k 1/sqrt(N) |k> ) ==> |0>
    Used for Deutsch-Jozsa verification (constant case).
    """
    if not _is_H(op): return None
    if _cn(inner) != "QXSum": return None
    cons = list(_call0(inner, "sums", []) or [])
    if len(cons) != 1: return None
    amp = _call0(inner, "amp")
    # Check for 1/sqrt...
    if _cn(amp) != "QXBin" or str(_call0(amp, "op")) != "/": return None
    left = _call0(amp, "left")
    if _cn(left) != "QXNum" or int(_call0(left, "num", -1)) != 1: return None
    # We assume if it's a normalized sum of |k>, H collapses it to |0>.
    return _mk_tensor([_mk_sket(_mk_num(0))])

def _rewrite_apply_oracle_on_sum(op: Any, tgt: Tuple[Any, ...], inner: Any) -> Optional[Any]:
    if _cn(op) != "QXOracle": return None
    
    sum_term = None
    target_term = None
    
    #safe check
    if isinstance(inner, ITensorProd) and len(inner.factors) == 2:
        left, right = inner.factors
        if _cn(left) == "QXSum":
            sum_term = left
            target_term = right
        elif _cn(left) == "IApply" and _is_H(left.op):
             sum_term = _rewrite_apply_expand_H0(left.op, left.target, left.term)
             target_term = right
             
        if not sum_term:
             if _cn(right) == "QXSum":
                 sum_term = right
                 target_term = left
             elif _cn(right) == "IApply" and _is_H(right.op):
                 sum_term = _rewrite_apply_expand_H0(right.op, right.target, right.term)
                 target_term = left

    elif _cn(inner) == "QXSum":
        sum_term = inner
        kets_obj = _call0(sum_term, "kets")
        ks = list(_call0(kets_obj, "kets", []) or [])
        if len(ks) >= 2: target_term = ks[1] 

    if not sum_term: return None
    sum_term = canon_sum(sum_term)

    cons = list(_call0(sum_term, "sums", []) or [])
    amp = _call0(sum_term, "amp")
    kets_obj = _call0(sum_term, "kets")
    ks = list(_call0(kets_obj, "kets", []) or [])
    if len(ks) < 1: return None
    x_val = _call0(ks[0], "vector")

    oracle_phase = _call0(op, "phase", None)
    
    # Check for trivial phase omega(0, 1) -> 1
    print(f"\n oracle_phase : {oracle_phase} \n target_term: {target_term}")
    if oracle_phase and _cn(oracle_phase) == "QXCall":
        fid = str(_call0(oracle_phase, "ID"))
        if fid == "omega":
            exps = list(_call0(oracle_phase, "exps", []) or [])
            if len(exps) >= 1:
                arg0 = exps[0]
                #default case
                if _cn(arg0) == "QXNum" and int(_call0(arg0, "num", -1)) == 0:
                    oracle_phase = None

    oracle_kets = list(_call0(op, "kets", []) or [])

    is_kickback = False
    if target_term and _is_ket_minus(target_term):
        is_kickback = True

    if oracle_phase or is_kickback:
        bindings = list(_call0(op, "bindings", []) or [])
        mapping = {}
        if bindings:
             bvar = str(_call0(bindings[0], "ID"))
             mapping[bvar] = x_val
        
        new_phase = None
        if oracle_phase:
             new_phase = _subst_bind(oracle_phase, mapping)
        elif is_kickback and len(oracle_kets) >= 2:
             vec_expr = _call0(oracle_kets[1], "vector") 
             if len(bindings) >= 2:
                 y_var = str(_call0(bindings[1], "ID"))
                 mapping[y_var] = _mk_num(0)
             f_x = _subst_bind(vec_expr, mapping)
             from Programmer import QXCall
             new_phase = QXCall(id='omega', exps=[f_x, _mk_num(1)], inverse=False)
        
        if new_phase:
            from Programmer import QXBin
            new_amp = QXBin(op='*', left=amp, right=new_phase)
            if target_term and isinstance(inner, ITensorProd):
                 new_sum = _mk_sum(cons, new_amp, _mk_tensor([ks[0]]))
                 return ITensorProd(factors=(new_sum, target_term))
            else:
                 new_sum = _mk_sum(cons, new_amp, kets_obj)
                 return new_sum

    # Case 2: Ket Function Oracle (Bit-Flip)
    if len(oracle_kets) >= 2:
        bindings = list(_call0(op, "bindings", []) or [])
        mapping = {}
        if len(bindings) >= 1: mapping[str(_call0(bindings[0], "ID"))] = x_val
        
        y_val = None
        if target_term and _cn(target_term) == "QXSKet":
             y_val = _call0(target_term, "vector")
        elif len(ks) >= 2:
             y_val = _call0(ks[1], "vector")
             
        if y_val and len(bindings) >= 2: mapping[str(_call0(bindings[1], "ID"))] = y_val

        new_ks = []
        for ok in oracle_kets:
            ov = _call0(ok, "vector")
            nv = _subst_bind(ov, mapping)
            new_ks.append(_mk_sket(nv))
        new_tensor = _mk_tensor(new_ks)
        return _mk_sum(cons, amp, new_tensor)

    return None


def _rewrite_oracle_iter(term: Any) -> Optional[Any]:

    if not isinstance(term, IIter):
        return None

    summ = term.body_summary
    if not isinstance(summ, ILoopSummary):
        return None

    steps = list(summ.steps or ())
    if len(steps) != 1 or not isinstance(steps[0], IStep):
        return None

    gate = steps[0].op
    if not isinstance(gate, ICtrlGate):
        return None

    ctrls = tuple(gate.controls or ())
    tgts  = tuple(gate.targets or ())
    if len(ctrls) != 1 or len(tgts) != 1:
        return None
    
    inner = term.term

    if isinstance(inner, ITensorProd):
        normalized_inner = _rewrite_tensor_sum_with_ket0(inner)
        print(f"\n norm_inner: \n {normalized_inner}")
        if normalized_inner:
            inner = normalized_inner

    if _cn(inner) != "QXSum":
        return None

    inner = canon_sum(inner)
    cons = list(_call0(inner, "sums", []) or [])
    if len(cons) != 1 or _cn(cons[0]) != "QXCon":
        return None

    k_id = str(_call0(cons[0], "ID"))
    if not k_id:
        return None

    kets = _call0(inner, "kets")
    if _cn(kets) != "QXTensor":
        return None
    #the kets
    ks = list(_call0(kets, "kets", []) or [])
    if len(ks) != 2:
        return None
        
    if _cn(ks[0]) != "QXSKet": return None
    vec0 = _call0(ks[0], "vector")
    if _cn(vec0) != "QXBind" or str(_call0(vec0, "ID")) != k_id: return None
    
    if _cn(ks[1]) != "QXSKet" or _cn(_call0(ks[1], "vector")) != "QXNum":
        return None

    oracle_op = gate.op
    if _cn(oracle_op) != "QXOracle":
        return None
       
    oracle_kets = list(_call0(oracle_op, "kets", []) or [])
    
    new_vec = None
    
    # Check what kind of oracle it is
    print(f"\n oracle_kets: \n {oracle_kets} \n or {oracle_op}")
    # Try to extract logic from oracle kets
    if len(oracle_kets) >= 1:
        oracle_vec = _call0(oracle_kets[0], "vector")
        loop_var = term.binder
        
        # Check if vector depends on loop variable i
        checker = HasLoopVarVisitor(loop_var)
        oracle_vec.accept(checker)
        print(f"checker {checker.found}")
        
        if not checker.found:
            if _is_increment_expr(oracle_vec):
                from Programmer import QXCall
                new_vec = QXCall(id='countN', exps=[_mk_bind(k_id)], inverse=False)
            
        else:
            if _cn(oracle_vec) == "QXBin":
                shor_vis = ShorVisitor(sum_binder=k_id, loop_var=loop_var)
                vec_with_k = oracle_vec.accept(shor_vis)
            elif _cn(oracle_vec) == "QXCall":
                vec_with_k = oracle_vec

            #x
            oracle_bindings = list(_call0(oracle_op, "bindings", []) or [])
            mapping = {}
            if oracle_bindings:
                #using the first binding here, could be multiple tho
                bound_var = str(_call0(oracle_bindings[0], "ID"))
                print(f"\n ks, {ks[0]} {ks[1]}")
                x0 = _call0(ks[1], "vector") 
                x0_str = _call0(x0, "num")
                print(f"\n x0", {x0})
                mapping[bound_var] = x0_str
            new_vec = _subst_bind(vec_with_k, mapping)

            new_vec = new_vec.accept(shor_vis)

    ket_k = _mk_sket(_call0(ks[0], "vector"))
    
    # New ket is |Oracle(k)> 
    ket_oracle = _mk_sket(new_vec)  
    new_tensor = _mk_tensor([ket_k, ket_oracle])
    return _mk_sum(cons, _call0(inner, "amp"), new_tensor)


# -------------------------
# Main normalizer
# -------------------------

def rewrite_term(term: Any, st: Any) -> Any:
 #   print(f"\n term: \n {term}")
    def rec(x: Any) -> Any:
        if isinstance(x, IIter):
            innerN = rec(x.term)
            y = _rewrite_oracle_iter(IIter(
                binder=x.binder, lo=x.lo, hi=x.hi,
                body_summary=x.body_summary, term=innerN
            ))
            if y is not None:
                return canon_sum(y)
            return IIter(binder=x.binder, lo=x.lo, hi=x.hi, body_summary=x.body_summary, term=innerN)
        
        if isinstance(x, IApply):
            innerN = rec(x.term)
            tgt = tuple(x.target)
            y = _rewrite_apply_cancel(x.op, tgt, innerN)
            if y is not None: return rec(y)
            y = _rewrite_apply_expand_H0(x.op, tgt, innerN)
            if y is not None: return canon_sum(y)
            y = _rewrite_apply_oracle_on_sum(x.op, tgt, innerN) # Added for Simon
            if y is not None: return y
            y = _rewrite_apply_collapse_sum(x.op, tgt, innerN) # DJ Constant
            if y is not None: return y
            # y = _rewrite_shor_rqft(x.op, tgt, innerN) # Shor
            # if y is not None: return y
            
            return IApply(op=x.op, target=tgt, term=innerN)

        if isinstance(x, ITensorProd):
            fsN = tuple(rec(f) for f in x.factors)
            y = _rewrite_tensor_sum_with_ket0(ITensorProd(fsN))
            if y is not None:
                return canon_sum(y)
            return ITensorProd(fsN)

        
        # if isinstance(x, IPostSelect):
        #     preN = rec(x.pre_term)
        #     y = _rewrite_postselect_hamming(IPostSelect(
        #         pre_term=preN,
        #         meas_locus=x.meas_locus,
        #         meas_value=x.meas_value,
        #         prob_sym=x.prob_sym
        #     ))
        #     if y is not None:
        #         return canon_sum(y) if _cn(y) == "QXSum" else y
        #     return IPostSelect(pre_term=preN, meas_locus=x.meas_locus, meas_value=x.meas_value, prob_sym=x.prob_sym)

        if _cn(x) == "QXSum":
            return canon_sum(x)

        return x

    out = term
    from sp_pretty import pp
    for _ in range(10):
        print(f"\n out{_}: {pp(out)}")
        nxt = rec(out)
        print(f"\n nxt{_}: {pp(nxt)}")
        if repr(nxt) == repr(out):
            break
        out = nxt
    return out

class ShorVisitor(SubstAExp): 
    def __init__(self, sum_binder: str, loop_var: Any):
        # No super().__init__ needed if ProgramVisitor doesn't require args, 
        # or call it correctly if it does.
        super().__init__("DUMMY_VAR", None) 
        self.sum_binder = sum_binder
        self.loop_var = loop_var

    def visitBind(self, ctx:Any):
        return ctx

    def visitBin(self, ctx):
        op = str(_call0(ctx, "op"))
        if op == "^":
            l = _call0(ctx, "left")
            # Ensure safe access to node properties
            if _cn(l) == "QXNum" and int(_call0(l, "num", -1)) == 2:
                return _mk_bind(self.sum_binder)
            
        if op == "*":
            r = _call0(ctx, "right")
            if _cn(r) == "QXNum" and _call0(r, "num") == 1:
                return ctx.left().accept(self)
        
        # Call the generic visitBin or visitQXBin depending on base class definition
        return super().visitBin(ctx)
    
class HasLoopVarVisitor(SubstAExp):
    def __init__(self, target_id: str):
        self.target_id = target_id
        self.found = False

    def visitBind(self, ctx):
        if str(_call0(ctx, "ID")) == self.target_id:
            self.found = True
        return ctx