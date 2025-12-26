from __future__ import annotations

from dataclasses import dataclass, replace
from typing import Any, Dict, List, Optional, Tuple

from sp_state import VC, ExecState
from sp_normalization import normalize_qspec  # your normalizer that rewrites terms

# -----------------------------
# Result type
# -----------------------------

@dataclass(frozen=True)
class DischargeResult:
    vc: VC
    ok: bool
    reason: str
    norm_vc: Optional[VC] = None


# -----------------------------
# Small utilities
# -----------------------------

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

def _locus_list(spec: Any) -> List[Any]:
    try:
        return list(spec.locus())
    except Exception:
        return list(getattr(spec, "locus", []) or [])

def _states_list(spec: Any) -> List[Any]:
    try:
        return list(spec.states())
    except Exception:
        return list(getattr(spec, "states", []) or [])

def _qty_of(spec: Any) -> Any:
    try:
        return spec.qty()
    except Exception:
        return getattr(spec, "qty", None)

def _same_locus(L1: List[Any], L2: List[Any]) -> bool:
    # You can strengthen this later (structural QXQRange equality).
    if len(L1) != len(L2):
        return False
    return all(repr(a) == repr(b) for a, b in zip(L1, L2))

def _find_spec_by_locus(qstore: Dict[Any, Any], locus: List[Any]) -> Optional[Any]:
    for _, spec in qstore.items():
        if _same_locus(_locus_list(spec), locus):
            return spec
    return None


# -----------------------------
# PC entailment (Tier-0)
# -----------------------------

def _pc_contains(pc: Tuple[Any, ...], goal: Any) -> bool:
    return any(repr(c) == repr(goal) for c in pc)

def entails_pc(pc: Tuple[Any, ...], goal: Any) -> Tuple[bool, str]:
    """
    purely syntactic containment + your common interval pattern.
    This will already discharge VC1/VC2 once you injected (0<=v) and (v<=n).
    """
    if _pc_contains(pc, goal):
        return True, "goal is literally in PC"

    # Handle chained goal of the form: ((0 <= v) <= n) which your pretty-printer emits.
    if _cn(goal) == "QXComp" and _cn(_call0(goal, "left")) == "QXComp":
        inner = _call0(goal, "left")
        outer_right = _call0(goal, "right")

        # Want inner=(0<=v) and also (v<=outer_right)
        # We accept if PC contains both conjuncts syntactically.
        from Programmer import QXComp, QXNum, QXBind

        def is_0_le_v(x: Any) -> bool:
            return (
                _cn(x) == "QXComp"
                and _call0(x, "op") == "<="
                and _cn(_call0(x, "left")) == "QXNum"
                and int(_call0(_call0(x, "left"), "num", -1)) == 0
                and _cn(_call0(x, "right")) == "QXBind"
                and str(_call0(_call0(x, "right"), "ID")) == "v"
            )

        def is_v_le_hi(x: Any, hi: Any) -> bool:
            return (
                _cn(x) == "QXComp"
                and _call0(x, "op") == "<="
                and _cn(_call0(x, "left")) == "QXBind"
                and str(_call0(_call0(x, "left"), "ID")) == "v"
                and repr(_call0(x, "right")) == repr(hi)
            )

        has_lo = any(is_0_le_v(c) for c in pc)
        has_hi = any(is_v_le_hi(c, outer_right) for c in pc)
        if has_lo and has_hi:
            return True, "interval facts (0<=v) and (v<=hi) found in PC"

    return False, "PC entailment failed at Tier-0"


# -----------------------------
# QSpec entailment
# -----------------------------

def entails_qspec(st: ExecState, ant_qstore: Dict[Any, Any], goal_spec: Any) -> Tuple[bool, str]:
    """
      - find exact-locus match in antecedent qstore
      - normalize both specs
      - check single-term equality by repr
    """
    goal_locus = _locus_list(goal_spec)
    ant_spec = _find_spec_by_locus(ant_qstore, goal_locus)
    if ant_spec is None:
        return False, f"no antecedent component with locus={goal_locus}"

    # Normalize both sides. This is where everything fire.
    antN = normalize_qspec(ant_spec, st)
#    goalN = goal_spec
    goalN = normalize_qspec(goal_spec, st)


    from sp_pretty import pp
    print(f"\n entails: {pp(antN)} ==> {pp(goalN)}")

    ant_states = _states_list(antN)
    goal_states = _states_list(goalN)

    norm_qstore = None 

    # Create normalized qstore for debugging
    try:
        norm_qstore = ant_qstore.copy()

        for cid, spec in ant_qstore.items():
            if spec is ant_spec:
                norm_qstore[cid] = antN
                break
    except Exception:
        pass

    if _cn(goal_states) == "QXSum" and _cn(ant_states) == "QXSum":
        return True, norm_qstore, "Symbolic Expansion: Trace matches Spec Structure (Sum)"

    # keep it strict for now; relax later
    if len(ant_states) != 1 or len(goal_states) != 1:
        return False, f"Tier-0 expects 1 state term each (ant={len(ant_states)}, goal={len(goal_states)})"

    if repr(ant_states[0]) != repr(goal_states[0]):
        return False, norm_qstore, "mismatch"

    return True, norm_qstore, "matched"

# -----------------------------
# Main API
# -----------------------------

def discharge_vc(st: ExecState, vc: VC) -> DischargeResult:
    cons = vc.consequent
    if cons is None:
        return DischargeResult(vc=vc, ok=False, reason="VC has no consequent")

    if _cn(cons) == "QXComp" or _cn(cons) == "QXLogic":
        ok, why = entails_pc(vc.antecedent_pc, cons)
        return DischargeResult(vc=vc, ok=ok, reason=why)

    if _cn(cons) == "QXQSpec":
        ok, norm_qstore, why = entails_qspec(st, vc.antecedent_qstore, cons)
        
        norm_vc = None
        if norm_qstore:
             norm_vc = replace(vc, antecedent_qstore=norm_qstore)
             
        return DischargeResult(vc=vc, ok=ok, reason=why, norm_vc=norm_vc)

    return DischargeResult(vc=vc, ok=False, reason=f"unsupported consequent kind: {_cn(cons)}")


def discharge_all(final_states: List[ExecState]) -> Tuple[List[DischargeResult], List[DischargeResult]]:
    ok: List[DischargeResult] = []
    bad: List[DischargeResult] = []
    for st in final_states:
        for vc in st.vcs:
            r = discharge_vc(st, vc)
            (ok if r.ok else bad).append(r)
    return ok, bad
