from __future__ import annotations
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

from sp_terms import IApply, IIter, ILoopSummary, IStep, ICtrlGate, ITensorProd, IPostSelect

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
    # QXTensor(kets=[QXSKet(vector=QXNum(0))])
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

def _is_increment_oracle(op: Any) -> bool:
    # Your HW oracle encoding: QXOracle(... kets=[|x+1|] ...)
    if _cn(op) != "QXOracle":
        return False
    kets = _call0(op, "kets", None) or []
    if not isinstance(kets, (list, tuple)) or len(kets) != 1:
        return False
    if _cn(kets[0]) != "QXSKet":
        return False
    v = _call0(kets[0], "vector", None)
    return _cn(v) == "QXBin" and str(_call0(v, "op")) == "+" and _cn(_call0(v, "right")) == "QXNum" and int(_call0(_call0(v, "right"), "num", -1)) == 1


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
    # Your class uses "condtion" (typo) in earlier AST prints.
    from Programmer import QXCon
    try:
        return QXCon(id=id_, crange=cr, condtion=cond)
    except TypeError:
        # fallback if someone fixed spelling
        return QXCon(id=id_, crange=cr, condition=cond)

def _mk_sket(v: Any):
    from Programmer import QXSKet
    return QXSKet(vector=v)

def _mk_tensor(kets: List[Any]):
    from Programmer import QXTensor
    return QXTensor(kets=kets, id='None', crange=None)

def _mk_sum(cons: List[Any], amp: Any, tensor: Any):
    from Programmer import QXSum
    # Your AST shows QXSum(..., kets=QXTensor(...))
    return QXSum(sums=cons, amp=amp, tensor=tensor)


# -------------------------
# Alpha-canonicalization for QXSum
# -------------------------

def _subst_bind(expr: Any, mapping: Dict[str, str]) -> Any:
    """
    Rename QXBind occurrences inside an expression/kets tree.
    Keep this minimal: QXBind/QXBin/QXUni/QXComp/QXTensor/QXSKet/QXCall/QXSum.
    Extend as needed.
    """
    cn = _cn(expr)
    if cn == "QXBind":
        nm = str(_call0(expr, "ID"))
        if nm in mapping:
            return _mk_bind(mapping[nm])
        return expr

    if cn == "QXBin":
        return _mk_bin(str(_call0(expr, "op")),
                       _subst_bind(_call0(expr, "left"), mapping),
                       _subst_bind(_call0(expr, "right"), mapping))

    if cn == "QXUni":
        return _mk_uni(str(_call0(expr, "op")),
                       _subst_bind(_call0(expr, "next"), mapping))

    if cn == "QXComp":
        from Programmer import QXComp
        return QXComp(op=str(_call0(expr, "op")),
                      left=_subst_bind(_call0(expr, "left"), mapping),
                      right=_subst_bind(_call0(expr, "right"), mapping))

    if cn == "QXCall":
        from Programmer import QXCall
        exps = list(_call0(expr, "exps", []) or [])
        return QXCall(id=str(_call0(expr, "ID")),
                      exps=[_subst_bind(e, mapping) for e in exps],
                      inverse=bool(_call0(expr, "inverse", False)))

    if cn == "QXSKet":
        return _mk_sket(_subst_bind(_call0(expr, "vector"), mapping))

    if cn == "QXTensor":
        ks = list(_call0(expr, "kets", []) or [])
        return _mk_tensor([_subst_bind(k, mapping) for k in ks])

    if cn == "QXSum":
        # recursive canonicalization handled elsewhere
        return expr

    return expr

def canon_sum(term: Any) -> Any:
    """
    Deterministically rename the first comprehension binder in a QXSum to k#0.
    This makes repr-based comparisons resilient to user binder choices.
    """
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

    # rebuild con with canonical id
    cr = _call0(cons[0], "crange")
    cond = None
    # your class exposes condtion()
    cond = _call0(cons[0], "condtion", None)
    con2 = _mk_con(new, cr, _subst_bind(cond, mapping) if cond is not None else None)

    amp2 = _subst_bind(_call0(term, "amp"), mapping)
    kets2 = _subst_bind(_call0(term, "kets"), mapping)

    return _mk_sum([con2], amp2, kets2)


# -------------------------
# Local rewrite rules
# -------------------------

def _rewrite_apply_cancel(op: Any, tgt: Tuple[Any, ...], inner: Any) -> Optional[Any]:
    # H(H(x)) -> x on same target
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
    """
    Apply[H @ q[lo,hi)](|0⟩) ==> ∑k∈[0,2^(hi-lo)). 1/sqrt(2^(hi-lo)) · |k⟩
    This produces the state of *that register only*.
    """
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
    """
    ITensorProd( Sum_k |k>, |0> )  ==>  Sum_k (|k> ⊗ |0>)
    This is needed so Hamming rules can see a joint |k>⊗|0> sum.
    """
    if not isinstance(term, ITensorProd):
        return None
    fs = term.factors
    if len(fs) != 2:
        return None
    left, right = fs
    if _cn(left) != "QXSum":
        return None
    if not _is_ket0(right):
        return None

    # left must be canonized already, but be defensive
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

    # build tensor |k> ⊗ |0>
    joint = _mk_tensor([ks[0], _mk_sket(_mk_num(0))])
    return _mk_sum(cons, amp, joint)

def _rewrite_hamming_iter(term: Any) -> Optional[Any]:
    """
    IIter( Summary[Ctrl[q[i]](increment-oracle @ p)],  ∑k |k>⊗|0> )
      ==> ∑k |k>⊗|countN(k)>
    """
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
    if str(_call0(ctrls[0], "location")) != "q":
        return None
    if str(_call0(tgts[0], "location")) != "p":
        return None
    if not _is_increment_oracle(gate.op):
        return None

    inner = term.term
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
    # second must be |0>
    if _cn(ks[1]) != "QXSKet" or _cn(_call0(ks[1], "vector")) != "QXNum" or int(_call0(_call0(ks[1], "vector"), "num", -1)) != 0:
        return None

    from Programmer import QXCall
    count = QXCall(id="countN", exps=[_mk_bind(k_id)], inverse=False)
    new_tensor = _mk_tensor([ks[0], _mk_sket(count)])

    return _mk_sum(cons, _call0(inner, "amp"), new_tensor)


# -------------------------
# Main normalizer: bottom-up + fixpoint
# -------------------------

def rewrite_term(term: Any, st: Any) -> Any:
    """
    Bottom-up, bounded fixpoint normalization for term DAG nodes.
    Keep it terminating: apply local rules, repeat a few rounds.
    """
    def rec(x: Any) -> Any:
        if isinstance(x, IApply):
            innerN = rec(x.term)
            tgt = tuple(x.target)
            # local cancels/expansions
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
            y = _rewrite_hamming_iter(IIter(
                binder=x.binder, lo=x.lo, hi=x.hi,
                body_summary=x.body_summary, term=innerN
            ))
            if y is not None:
                return canon_sum(y)
            return IIter(binder=x.binder, lo=x.lo, hi=x.hi, body_summary=x.body_summary, term=innerN)

        if isinstance(x, IPostSelect):
            preN = rec(x.pre_term)
            # optionally, apply a postselect-specific rewrite after normalizing pre_term
            y = _rewrite_postselect_hamming(IPostSelect(
                pre_term=preN,
                meas_locus=x.meas_locus,
                meas_value=x.meas_value,
                prob_sym=x.prob_sym
            ))
            if y is not None:
                return canon_sum(y) if _cn(y) == "QXSum" else y
            return IPostSelect(pre_term=preN, meas_locus=x.meas_locus, meas_value=x.meas_value, prob_sym=x.prob_sym)

        # If it's a QXSum, canonicalize binders
        if _cn(x) == "QXSum":
            return canon_sum(x)

        return x

    out = term
    # bounded fixpoint is fine for your current rules
    for _ in range(6):
        nxt = rec(out)
        if repr(nxt) == repr(out):
            break
        out = nxt
    return out


def _rewrite_postselect_hamming(ps: Any) -> Optional[Any]:
    """
    PostSelect[p=v] ( ∑k 1/sqrt(2^n) |k> ⊗ |countN(k)> )
      ==> ∑k (1/sqrt(choose(n,v))) |k>   with condition countN(k)==v
    """
    from Programmer import QXCall

    if not isinstance(ps, IPostSelect):
        return None

    pre = ps.pre_term
    if _cn(pre) != "QXSum":
        return None

    pre = canon_sum(pre)

    cons = list(_call0(pre, "sums", []) or [])
    if len(cons) != 1 or _cn(cons[0]) != "QXCon":
        return None

    k_id = str(_call0(cons[0], "ID"))
    if not k_id:
        return None

    kets = _call0(pre, "kets")
    if _cn(kets) != "QXTensor":
        return None
    ks = list(_call0(kets, "kets", []) or [])
    if len(ks) != 2:
        return None

    # second ket must be |countN(k)|
    if _cn(ks[1]) != "QXSKet":
        return None
    v2 = _call0(ks[1], "vector")
    if _cn(v2) != "QXCall" or str(_call0(v2, "ID")) != "countN":
        return None
    exps = list(_call0(v2, "exps", []) or [])
    if len(exps) != 1 or _cn(exps[0]) != "QXBind" or str(_call0(exps[0], "ID")) != k_id:
        return None

    # Build condition countN(k)==v
    from Programmer import QXComp
    cond = QXComp(op="==", left=v2, right=ps.meas_value)

    # Need n for choose(n,v): infer from the QXCon range right side, which is 2^n
    cr = _call0(cons[0], "crange")
    hi = _call0(cr, "right")
    # expect hi = (2 ^ n) or (2 ^ width)
    n_expr = None
    if _cn(hi) == "QXBin" and str(_call0(hi, "op")) == "^":
        # hi = 2 ^ n
        right = _call0(hi, "right")
        n_expr = right

    if n_expr is None:
        return None

    # amp = 1/sqrt(choose(n,v))
    choose = QXCall(id="choose", exps=[n_expr, ps.meas_value], inverse=False)
    amp = _mk_bin("/", _mk_num(1), _mk_uni("sqrt", choose))

    # new sum: same binder, but with condition; ket is |k> only
    con2 = _mk_con("k#0", _call0(cons[0], "crange"), cond)  # binder already canonized to k#0
    ket1 = _mk_tensor([ks[0]])  # keep first ket only

    return _mk_sum([con2], amp, ket1)

