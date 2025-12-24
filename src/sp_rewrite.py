from __future__ import annotations
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

from sp_terms import IApply, IIter, ILoopSummary, IStep, ICtrlGate, ITensorProd, IPostSelect
from sp_pretty import pp
from SubstAExp import SubstAExp

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

def _is_ket0(term: Any) -> bool:
    if _cn(term) != "QXTensor":
        return False
    ks = _call0(term, "kets", None)
    if not isinstance(ks, (list, tuple)) or len(ks) != 1:
        return False
    if _cn(ks[0]) != "QXSKet":
        return False
    v = _call0(ks[0], "vector", None)
    return _cn(v) == "QXNum" and int(_call0(v, "num", -1)) == 0

def _is_H(op: Any) -> bool:
    return _op_name(op) == "H"

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
        from Programmer import QXBind
        tgt_bind = QXBind(id=tgt)
        
        visitor = SubstAExp(src, tgt_bind)
        
        if hasattr(res, "accept"):
            res = res.accept(visitor)
        else:
            pass
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
    if not _is_ket0(inner):
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

    two_pow = _mk_bin("^", _mk_num(2), width)
    amp = _mk_bin("/", _mk_num(1), _mk_uni("sqrt", two_pow))
    con = _mk_con("k#0", _mk_crange(_mk_num(0), two_pow), None)
    ket = _mk_tensor([_mk_sket(_mk_bind("k#0"))])
    return _mk_sum([con], amp, ket)

def _rewrite_tensor_sum_with_ket0(term: Any) -> Optional[Any]:
    if not isinstance(term, ITensorProd):
        return None
    fs = term.factors
    if len(fs) != 2:
        return None
    left, right = fs
    
    # Try normalizing left side to Sum if it's IApply(H)
    if _cn(left) == "IApply" and _is_H(left.op):
        left_expanded = _rewrite_apply_expand_H0(left.op, left.target, left.term)
        if left_expanded:
            left = left_expanded

    if _cn(left) != "QXSum":
        return None
    if not _is_ket0(right):
        return None

    left = canon_sum(left)

    cons = list(_call0(left, "sums", []) or [])
    if len(cons) != 1 or _cn(cons[0]) != "QXCon":
        return None

    amp = _call0(left, "amp")
    kets = _call0(left, "kets")
    if _cn(kets) != "QXTensor":
        return None
    ks = list(_call0(kets, "kets", []) or [])
    if len(ks) != 1:
        return None

    joint = _mk_tensor([ks[0], _mk_sket(_mk_num(0))])
    return _mk_sum(cons, amp, joint)

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
    ks = list(_call0(kets, "kets", []) or [])
    if len(ks) != 2:
        return None
        
    if _cn(ks[0]) != "QXSKet": return None
    vec0 = _call0(ks[0], "vector")
    if _cn(vec0) != "QXBind" or str(_call0(vec0, "ID")) != k_id: return None
    
    if _cn(ks[1]) != "QXSKet" or _cn(_call0(ks[1], "vector")) != "QXNum" or int(_call0(_call0(ks[1], "vector"), "num", -1)) != 0:
        return None

    oracle_op = gate.op
    if _cn(oracle_op) != "QXOracle":
        return None
        
    oracle_kets = list(_call0(oracle_op, "kets", []) or [])
    
    new_vec = None
    
    # Check what kind of oracle it is
    if len(oracle_kets) >= 1:
        oracle_vec = _call0(oracle_kets[0], "vector")
        oracle_bindings = list(_call0(oracle_op, "bindings", []) or [])
        
        mapping = {}
        if oracle_bindings:
            bound_var = str(_call0(oracle_bindings[0], "ID"))
            mapping[bound_var] = k_id
        
        new_vec = _subst_bind(oracle_vec, mapping)
        
    else: #hard coded for now
        from Programmer import QXCall
        new_vec = QXCall(id='countN', exps=[_mk_bind(k_id)], inverse=False)

    ket_k = _mk_sket(_call0(ks[0], "vector"))
    
    # New ket is |Oracle(k)> 
    ket_oracle = _mk_sket(new_vec)
    
    new_tensor = _mk_tensor([ket_k, ket_oracle])

    return _mk_sum(cons, _call0(inner, "amp"), new_tensor)


# -------------------------
# Main normalizer
# -------------------------

def rewrite_term(term: Any, st: Any) -> Any:
    print(f"\n term: \n {term}")
    def rec(x: Any) -> Any:
        if isinstance(x, IApply):
            innerN = rec(x.term)
            tgt = tuple(x.target)
            y = _rewrite_apply_cancel(x.op, tgt, innerN)
            if y is not None:
                return rec(y)
            y = _rewrite_apply_expand_H0(x.op, tgt, innerN)
            if y is not None:
                return canon_sum(y)
            return IApply(op=x.op, target=tgt, term=innerN)

        if isinstance(x, ITensorProd):
            fsN = tuple(rec(f) for f in x.factors)
            y = _rewrite_tensor_sum_with_ket0(ITensorProd(fsN))
            if y is not None:
                return canon_sum(y)
            return ITensorProd(fsN)

        if isinstance(x, IIter):
            innerN = rec(x.term)
            y = _rewrite_oracle_iter(IIter(
                binder=x.binder, lo=x.lo, hi=x.hi,
                body_summary=x.body_summary, term=innerN
            ))
            if y is not None:
                return canon_sum(y)
            return IIter(binder=x.binder, lo=x.lo, hi=x.hi, body_summary=x.body_summary, term=innerN)

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