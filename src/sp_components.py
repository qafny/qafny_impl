from __future__ import annotations
from typing import Any, Dict, List, Optional, Tuple, Set

from sp_state import ExecState, get_regs
from sp_qtys import join_qty
from sp_terms import ITensorProd


# Handle loci comparison if available
try:
    from ProgramTransformer import compareLocus, subLocus  
except Exception:
    compareLocus = None
    subLocus = None


def owner_component(st: ExecState, target_locus: List[Any]) -> Optional[int]:
    """
    Find a component whose locus contains target_locus using subLocus().
    Returns cid if found, else None.
    """
    # Use PiManager to filter candidates efficiently
    candidates = st.pi.get_candidates(target_locus, st.qstore.keys())

    # Check candidates
    for cid in candidates:
        spec = st.qstore.get(cid)
        if spec is None: 
            continue
        
        # Use ProgramTransformer logic if available
        if subLocus:
            if subLocus(target_locus, spec.locus()):
                return cid
        else:
            # Fallback: simple register name overlap check
            spec_regs = get_regs(spec.locus())
            tgt_regs = get_regs(target_locus)
            if tgt_regs.issubset(spec_regs):
                return cid
                
    return None


def touches_components(st: ExecState, target_locus: List[Any]) -> Set[int]:
    """
    Return set of CIDs that overlap with target_locus.
    Used for detecting merge conflicts.
    """
    candidates = st.pi.get_candidates(target_locus, st.qstore.keys())
    hits = set()
    
    tgt_regs = get_regs(target_locus)
    
    for cid in candidates:
        spec = st.qstore.get(cid)
        if spec is None: continue
        
        spec_regs = get_regs(spec.locus())
        # Intersection check
        if not tgt_regs.isdisjoint(spec_regs):
            hits.add(cid)
            
    return hits


def merge_components(st: ExecState, hits: Set[int]) -> int:
    """
    Merge multiple components into one (Survivor Strategy).
    - Keeps the smallest CID (survivor).
    - Tensors states.
    - Updates Index via PiManager.
    """
    sorted_hits = sorted(list(hits))

    survivor = sorted_hits[0]
    victims = sorted_hits[1:]

    survivor_spec = st.qstore[survivor]
    
    # Accumulate state
    new_locus = list(survivor_spec.locus())
    new_qty = survivor_spec.qty()
    
    # We collect terms for the tensor product.
    def get_term(spec):
        states = getattr(spec, "states", [])
        if callable(states): states = states()
        return list(states)[0] # Taking primary term, need to be fixed for state with multiple terms
        
    term_factors = [get_term(survivor_spec)]

    for vic_cid in victims:
        vic_spec = st.qstore[vic_cid]
        
        # Merge Locus (Naive list extend; ideally check duplicates or use Set)
        new_locus.extend(vic_spec.locus())
        
        # Merge Qty
        new_qty = join_qty(new_qty, vic_spec.qty(), fresh_flag=st.fresh.fresh_en_flag())
        
        # Merge Term
        term_factors.append(get_term(vic_spec))
        
        # Delete Victim, de-index before deleting to clean up map
        st.pi.deindex_component(vic_cid, vic_spec.locus())
        del st.qstore[vic_cid]

    # Create new combined term
    new_term = ITensorProd(factors=tuple(term_factors))
    
    # Update Survivor
    from Programmer import QXQSpec
    new_spec = QXQSpec(locus=new_locus, qty=new_qty, states=[new_term])
    st.qstore[survivor] = new_spec
    
    # Re-index survivor (it grew)
    st.pi.index_component(survivor, new_locus)
    
    return survivor


def ensure_component_for(st: ExecState, target_locus: List[Any]) -> int:
    """
    Ensure there is exactly one component covering target_locus.
    - If spread across multiple, merge them.
    - If none, create new.
    - If one, return it.
    """
    hits = touches_components(st, target_locus)
    
    if len(hits) == 1:
        # Check if it fully covers logic (owner_component logic)
        cid = list(hits)[0]
        # In a robust system we check subLocus here again, 
        # but for now assume touch + 1 hit = owner or partial owner.
        return cid
        
    if len(hits) > 1:
        return merge_components(st, hits)

    # Create new component
    cid = st.pi.new_cid()
    
    try:
        from Programmer import QXQSpec, TyEn, TyNor
        # Default to TyNor if just allocated? Or TyEn? 
        # Use context or default. Here we assume generic allocation is En 
        # or rely on caller to refine. Let's use En for safety.
        # Actually, standard allocation |0> is Nor. 
        # But if we are ensuring for a Gate application, we might not be allocating.
        # This branch implies 'touching nothing', meaning these registers were unused.
        # Ideally, we start them as |0> (Nor).
        spec = QXQSpec(locus=target_locus, qty=TyNor(), states=[]) # Empty states? 
    except ImportError:
        # Fallback for mocks
        spec = None
        
    st.qstore[cid] = spec
    st.pi.index_component(cid, target_locus)
    return cid

def split_component_by_regs(st: ExecState, cid: int, keep_regs: Set[str]) -> Optional[Tuple[int, int]]:
    """
    Split qstore[cid] into:
      - cid_keep: locus over keep_regs
      - cid_rest: locus over the other regs
    Returns (cid_keep, cid_rest) or None if not splittable.
    """
    spec = st.qstore.get(cid)
    if spec is None:
        return None

    locus = list(spec.locus()) if hasattr(spec, "locus") else []
    
    # Use helper from sp_state
    keep_locus = []
    rest_locus = []
    
    for r in locus:
        # We need per-range reg check
        # This assumes range doesn't span boundaries we care about
        rs = get_regs([r])
        if rs.issubset(keep_regs):
            keep_locus.append(r)
        else:
            rest_locus.append(r)

    if not keep_locus or not rest_locus:
        return None

    # Logic to split the TERM inside spec is complex (Tensor factorization).
    # For now, we assume we can only split ITensorProd terms.
    states = getattr(spec, "states", [])
    if callable(states): states = states()
    if not states: return None
    
    term = list(states)[0]
    if getattr(term, "__class__", None).__name__ != "ITensorProd":
        # Cannot split atomic term (entangled)
        return None
        
    # Naive split of factors (assuming factors align with locus)
    # This is a simplification. In real impl, we map factors to regs.
    factors = term.factors
    if len(factors) != 2: 
        return None # Simplest case only
        
    # Assume factor 0 -> keep, factor 1 -> rest (or vice versa) based on simple count?
    # Without tracking which factor belongs to which register, this is unsafe.
    # ABORT split if we can't be sure.
    return None 
    
    # If successful:
    # 1. Update old cid to have rest_locus and factor 1
    # 2. Create new cid to have keep_locus and factor 0
    # 3. Update index