# sp_terms.py
from __future__ import annotations
from dataclasses import dataclass
from typing import Any, List, Sequence, Tuple

from sp_qtys import degrade_qty

# -----------------------------
# Internal term nodes
# -----------------------------

@dataclass(frozen=True)
class IApply:
    """Deferred operator application."""
    op: Any
    target: Tuple[Any, ...]   # tuple[QXQRange,...] (symbolic, AST-based)
    term: Any                 # a TERM (never QXQSpec, never list)

@dataclass(frozen=True)
class ITensorProd:
    """Internal tensor product (rarely needed once you stop DSU-merging by atoms)."""
    factors: Tuple[Any, ...]

@dataclass(frozen=True)
class ILoopSummary:
    """Compact symbolic summary of one loop iteration (parameterized by binder)."""
    steps: Tuple[Any, ...]    # tuple[IStep,...]

@dataclass(frozen=True)
class IStep:
    op: Any                   # QXSingle/QXOracle/ICtrlGate/...
    target: Tuple[Any, ...]   # tuple[QXQRange,...]
    guard: Any | None = None  # optional (classical) guard

@dataclass(frozen=True)
class IIter:
    """Internal loop summary node."""
    binder: str
    lo: Any
    hi: Any
    body_summary: ILoopSummary
    term: Any

@dataclass(frozen=True)
class ICtrlGate:
    """Controlled operator with symbolic control/target loci."""
    controls: Tuple[Any, ...] # tuple[QXQRange,...]
    op: Any                   # QXSingle/QXOracle/...
    targets: Tuple[Any, ...]  # tuple[QXQRange,...]

@dataclass(frozen=True)
class ICtrlBody:
    controls: Tuple[Any, ...]
    op: Any
    targets: Tuple[Any, ...]

@dataclass(frozen=True)
class IMeasure:
    """Deferred measurement of a locus; outcome stored in classical ids."""
    ids: Tuple[str, ...]          # e.g. ("w","v","s")
    locus: Tuple[Any, ...]        # list/tuple of QXQRange
    term: Any                     # the pre-measure joint term

@dataclass(frozen=True)
class IPostSelect:
    """
    Represents the post-measurement residual state on `residual_locus`,
    obtained by measuring `meas_locus` and observing `meas_value`.
    """
    pre_term: Any                 # the joint term BEFORE measurement (may mention q and p)
    meas_locus: Tuple[Any, ...]   # e.g. (QXQRange(p, [0,n)),)
    meas_value: Any               # e.g. QXBind('v')
    prob_sym: Any | None = None   # optional: QXBind('prob') or similar



def apply_op_to_term(op: Any, target_locus: Sequence[Any], term: Any) -> Any:
    """
    Always defer application: return IApply(op, target_locus, term).
    Later you can add rewrite rules here.
    """
    # Normalize representation: always tuple
    tgt = tuple(target_locus)
    return IApply(op=op, target=tgt, term=term)


def apply_op_to_qspec(qspec: Any, op: Any, target_locus: Sequence[Any], st: Any) -> Any:
    """
    Apply op to each top-level term in qspec.states (treated as linear sum).
    - Does not expand IApply.
    - Updates qty eagerly via degrade_qty.
    """
    # 1) read header
    try:
        locus = qspec.locus()
    except Exception:
        locus = getattr(qspec, "locus", None)

    try:
        qty = qspec.qty()
    except Exception:
        qty = getattr(qspec, "qty", None)

    # 2) read states (must become list of TERMS)
    try:
        states = list(qspec.states())
    except Exception:
        states = list(getattr(qspec, "states", []))

    # Defensive: if a spec accidentally stores a single term, normalize to [term]
    if not isinstance(states, list):
        states = [states]

    new_states: List[Any] = []
    for t in states:
        # Never allow list-valued terms
        if isinstance(t, list):
            raise TypeError("qspec.states contains a Python list; expected term node")
        new_states.append(apply_op_to_term(op, target_locus, t))

    # 3) eagerly degrade qty
    fresh_flag = st.fresh.fresh_en_flag()
    new_qty = degrade_qty(qty, op, fresh_flag=fresh_flag)

    # 4) rebuild spec
    try:
        from Programmer import QXQSpec
        return QXQSpec(locus=locus, qty=new_qty, states=new_states)
    except Exception:
        # Fallback: return a lightweight tuple (debug only)
        return (locus, new_qty, new_states)

