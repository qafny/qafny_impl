from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Optional, Tuple, Union

@dataclass(frozen=True)
class SymScalar:
    """Opaque classical scalar symbol (e.g., measurement result)."""
    name: str

def _as_cstore(env: Any) -> Optional[dict[str, Any]]:
    # Accept dict[str,Any] or ExecState-like objects (with .cstore)
    if env is None:
        return None
    if isinstance(env, dict):
        return env
    return getattr(env, "cstore", None)

def try_eval_int(aexp: Any, env: Any = None) -> Optional[int]:
    """Best-effort evaluator for integer arithmetic expressions.

    env may be:
      - dict[str,Any] (cstore)
      - ExecState (has .cstore)
    """
    cstore = _as_cstore(env)

    if aexp is None:
        return None
    
    if isinstance(aexp, SymScalar):
        return None

    try:
        from Programmer import QXNum, QXBind, QXBin
    except Exception:
        QXNum = QXBind = QXBin = ()  # type: ignore

    if isinstance(aexp, QXNum):
        v = aexp.num()
        return int(v) if isinstance(v, int) else None

    if isinstance(aexp, QXBind):
        name = aexp.ID()
        if cstore and name in cstore:
            return try_eval_int(cstore[name], cstore)
        return None

    if isinstance(aexp, QXBin):
        op = aexp.op()
        l = try_eval_int(aexp.left(), cstore)
        r = try_eval_int(aexp.right(), cstore)
        if l is None or r is None:
            return None
        try:
            if op == '+':  return l + r
            if op == '-':  return l - r
            if op == '*':  return l * r
            if op == '//': return l // r
        except Exception:
            return None

    return None

def as_bool_not(bexp: Any) -> Any:
    """Build a syntactic negation node for path conditions."""
    try:
        from Programmer import QXUni
        return QXUni('!', bexp)
    except Exception:
        # fallback: tuple marker
        return ('not', bexp)
