# sp_tags.py
from __future__ import annotations
from typing import Any

# These functions define the abstract domain behavior for qty:
# - join_qty: used when merging components
# - degrade_qty: used when applying ops
# - leq_qty: used for safe strengthening/weakenings

def is_nor(q: Any) -> bool:
    try:
        from Programmer import TyNor
        return isinstance(q, TyNor)
    except Exception:
        return q.__class__.__name__ == "TyNor"

def is_had(q: Any) -> bool:
    try:
        from Programmer import TyHad
        return isinstance(q, TyHad)
    except Exception:
        return q.__class__.__name__ == "TyHad"

def is_en(q: Any) -> bool:
    try:
        from Programmer import TyEn
        return isinstance(q, TyEn)
    except Exception:
        return q.__class__.__name__ == "TyEn"

def _fresh_en(fresh_flag: Any) -> Any:
    from Programmer import TyEn
    return TyEn(flag=fresh_flag)

def join_qty(q1: Any, q2: Any, *, fresh_flag: Any) -> Any:
    """
    Conservative tag join.
    - Nor and Had are structured and incomparable; their join is En.
    - En dominates everything (keeps En).
    """
    if type(q1) is type(q2):
        return q1  # keep identical tag; flag unification is optional

    if is_en(q1):
        return q1
    if is_en(q2):
        return q2

    if (is_nor(q1) and is_had(q2)) or (is_had(q1) and is_nor(q2)):
        return _fresh_en(fresh_flag)

    # Default conservative
    return _fresh_en(fresh_flag)

def degrade_qty(qty: Any, op: Any, *, fresh_flag: Any) -> Any:
    """
    Monotone weakening of structure after applying an op.
    For now: any non-trivial op on structured tags degrades to En.
    You can later add preservation rules here.
    """
    if is_en(qty):
        return qty

    # OPTIONAL: you may whitelist trivial "structure-preserving" operations later.
    # Right now, stay sound and simple:
    return _fresh_en(fresh_flag)

def leq_qty(q1: Any, q2: Any) -> bool:
    """
    q1 <= q2 in the tag order (safe strengthening check).
    Nor <= En, Had <= En, identical types are comparable.
    """
    if type(q1) is type(q2):
        return True
    if (is_nor(q1) or is_had(q1)) and is_en(q2):
        return True
    return False
