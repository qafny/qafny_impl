from __future__ import annotations
from typing import Any, Dict, List, Optional, Tuple

from sp_qtys import join_qty
from sp_terms import ITensorProd


#handle loci and qty
try:
    from ProgramTransformer import compareLocus, subLocus  
except Exception:
    compareLocus = None
    subLocus = None


def _regs_of_locus(locus: List[Any]) -> set[str]:
    regs: set[str] = set()
    for r in locus:
        try:
            regs.add(str(r.location()))
        except Exception:
            regs.add(str(getattr(r, "location", None)))
    return regs


def candidates_for_locus(qstore: Dict[int, Any], pi: Any, target_locus: List[Any]) -> List[int]:
    """
    Conservative candidate selection:
    - If pi.reg_index exists, use it.
    - Otherwise scan all components.
    """
    regs = _regs_of_locus(target_locus)

    reg_index = getattr(pi, "reg_index", None)
    if isinstance(reg_index, dict) and reg_index:
        cands: set[int] = set()
        for r in regs:
            cands |= reg_index.get(r, set())
        return sorted(cands)

    return sorted(list(qstore.keys()))


def owner_component(st: Any, target_locus: List[Any]) -> Optional[int]:
    """
    Find a component whose locus contains target_locus using subLocus().
    Returns cid or None.
    """
    if subLocus is None:
        return None

    qs: List[Tuple[List[Any], Any, int]] = []
    for cid in candidates_for_locus(st.qstore, st.pi, target_locus):
        spec = st.qstore.get(cid)
        if spec is None:
            continue
        qs.append((list(spec.locus()), spec.qty(), cid))

    hit = subLocus(target_locus, qs)
    if hit is None:
        return None
    _, _, cid = hit
    return cid


def touches_components(st: Any, target_locus: List[Any]) -> List[int]:
    """
    Conservative "may overlap" check:
    - If containment proven via compareLocus, include.
    - Otherwise, if same register name, include (sound over-approx).
    """
    hits: set[int] = set()

    for cid in candidates_for_locus(st.qstore, st.pi, target_locus):
        spec = st.qstore.get(cid)
        if spec is None:
            continue
        comp_locus = list(spec.locus())

        # If we have compareLocus, use it
        if compareLocus is not None:
            if compareLocus(target_locus, comp_locus) is not None:
                hits.add(cid)
                continue
            if compareLocus(comp_locus, target_locus) is not None:
                hits.add(cid)
                continue

        # Fallback: same register => may overlap
        if _regs_of_locus(target_locus) & _regs_of_locus(comp_locus):
            hits.add(cid)

    return sorted(hits)

def _tensor_product_states(s1: List[Any], s2: List[Any]) -> List[Any]:
    """
    Correct component merge:
      states := { ITensorProd(t, u) | t in s1, u in s2 }
    Treat each top-level term as atomic; do NOT distribute QXSum here.
    """
    if not s1:
        return list(s2)
    if not s2:
        return list(s1)

    out: List[Any] = []
    for t in s1:
        for u in s2:
            out.append(ITensorProd(factors=(t, u)))
    return out


def _union_locus(loci: List[List[Any]]) -> List[Any]:
    """
    Very conservative locus union:
    - concatenate and keep as a list
    Later you can canonicalize/merge adjacent ranges.
    """
    out: List[Any] = []
    for L in loci:
        out.extend(list(L))
    return out


def merge_components(st: Any, cids: List[int]) -> int:
    """
    Merge multiple components into the first cid.

    Correct semantics:
    - locus := union (conservative)
    - qty   := join_qty (abstract tag join)
    - states := tensor-product of superpositions (NOT concatenation)
    """
    cids = sorted(set(cids))
    if not cids:
        raise ValueError("merge_components called with empty list")
    if len(cids) == 1:
        return cids[0]

    base = cids[0]
    base_spec = st.qstore[base]

    loci: List[List[Any]] = [list(base_spec.locus())]
    try:
        states: List[Any] = list(base_spec.states())
    except Exception:
        states = list(getattr(base_spec, "states", []))
    qty = base_spec.qty()

    # Merge remaining components into base using tensor-product
    for cid in cids[1:]:
        s = st.qstore[cid]

        loci.append(list(s.locus()))
        try:
            s_states = list(s.states())
        except Exception:
            s_states = list(getattr(s, "states", []))

        # KEY FIX: tensor-product merge, not concatenation
        states = _tensor_product_states(states, s_states)

        qty = join_qty(qty, s.qty(), fresh_flag=st.fresh.fresh_en_flag())

        # delete old component
        del st.qstore[cid]
        st.pi.drop_component(cid)

    merged_locus = _union_locus(loci)

    from Programmer import QXQSpec
    merged = QXQSpec(locus=merged_locus, qty=qty, states=states)

    st.qstore[base] = merged

    # re-index base
    st.pi.drop_component(base)
    st.pi.index_component(base, list(merged.locus()))
    return base



def ensure_component_for(st: Any, target_locus: List[Any]) -> int:
    """
    Ensure there is a single component to which this target applies:
    1) If target is contained in an existing component -> return it.
    2) Else merge any components that may overlap -> return merged.
    3) Else create a new component covering target_locus -> return new cid.
    """
    own = owner_component(st, target_locus)
    if own is not None:
        return own

    hits = touches_components(st, target_locus)
    if hits:
        return merge_components(st, hits)

    # create new component
    cid = st.pi.new_cid()
    from Programmer import QXQSpec, TyEn
    spec = QXQSpec(locus=target_locus, qty=TyEn(flag=st.fresh.fresh_en_flag()), states=[])
    st.qstore[cid] = spec
    st.pi.index_component(cid, target_locus)
    return cid

def split_component_by_regs(st: Any, cid: int, keep_regs: set[str]) -> tuple[int, int] | None:
    """
    Split qstore[cid] into:
      - cid_keep: locus over keep_regs
      - cid_rest: locus over the other regs
    Returns (cid_keep, cid_rest) or None if not splittable.
    """
    spec = st.qstore.get(cid)
    if spec is None:
        return None

    locus = list(spec.locus())
    def reg_of(qr): 
        try: return str(qr.location())
        except: return str(getattr(qr, "location", None))

    keep = [qr for qr in locus if reg_of(qr) in keep_regs]
    rest = [qr for qr in locus if reg_of(qr) not in keep_regs]

    if not keep or not rest:
        return None

    # Create new cid for keep part, reuse old cid for rest
    cid_keep = st.pi.new_cid()

    from Programmer import QXQSpec
    st.qstore[cid_keep] = QXQSpec(locus=keep, qty=spec.qty(), states=list(spec.states()))
    st.qstore[cid]      = QXQSpec(locus=rest, qty=spec.qty(), states=list(spec.states()))

    # re-index
    st.pi.drop_component(cid)
    st.pi.index_component(cid, rest)
    st.pi.index_component(cid_keep, keep)

    return cid_keep, cid

