from __future__ import annotations
from typing import Any, List

from sp_rewrite import rewrite_term  # single source of truth

def _call_list(obj: Any, name: str, default=None) -> list:
    f = getattr(obj, name, None)
    if callable(f):
        try:
            return list(f() or [])
        except Exception:
            return list(default or [])
    return list(getattr(obj, name, default or []) or [])

def normalize_qspec(spec: Any, st: Any) -> Any:
    """
    Normalize only the state terms (and canonicalize alpha binders).
    Keep locus/qty as-is.
    """
    locus = _call_list(spec, "locus")
    qty   = getattr(spec, "qty", None)
    if callable(getattr(spec, "qty", None)):
        try: qty = spec.qty()
        except Exception: pass

    states = _call_list(spec, "states")

    new_states = [rewrite_term(t, st) for t in states]
    print(f"\n new_states {new_states}")

    from Programmer import QXQSpec
    return QXQSpec(locus=locus, qty=qty, states=new_states)
