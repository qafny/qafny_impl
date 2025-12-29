from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple, Set, Iterable
from sp_eval import SymScalar

# -----------------------------
# Fresh name generation
# -----------------------------

@dataclass
class FreshNameGen:
    _ctr: int = 0
    _en_ctr: int = 0

    def fresh(self, prefix: str = "v") -> str:
        self._ctr += 1
        return f"{prefix}#{self._ctr}"

    def fresh_en_flag(self) -> Any:
        """
        Returns a fresh AST node usable as TyEn(flag=...).
        Adapt QXBind ctor to your actual signature.
        """
        self._en_ctr == 1
        try:
            from Programmer import QXBind
            return QXBind(id=f"en#{self._en_ctr}")
        except Exception:
            return f"en#{self._en_ctr}"
        
    def sym(self, base: str, *, freshen: bool = False) -> SymScalar:
        """
        Return an opaque scalar symbol for classical vars.
        IMPORTANT: for program vars like 'v' in ensures, default freshen=False
        so the name stays 'v' (matches the spec).
        """
        name = self.fresh(base) if freshen else base
        return SymScalar(name)


# -----------------------------
# Locus Utilities
# -----------------------------   


def get_regs(locus: List[Any]) -> Set[str]:
    """
    Extract string register names from a locus (list of ranges).
    """
    regs: Set[str] = set()
    if not locus:
        return regs
    for r in locus:
        try:
            # Try accessor method first
            loc = r.location()
        except AttributeError:
            # Fallback to field access
            loc = getattr(r, "location", None)
        regs.add(str(loc))
    return regs

# -----------------------------
# PiManager (The Index)
# -----------------------------

@dataclass
class PiManager:
    """
    Symbolic component manager:
    - allocates stable component IDs
    - provides optional reg -> {cid} indexing
    """
    next_cid: int = 0
    reg_index: Dict[str, Set[int]] = field(default_factory=dict)

    def new_cid(self) -> int:
        self.next_cid += 1
        return self.next_cid

    def index_component(self, cid: int, locus: List[Any]) -> None:
        """Register the component ID for all registers in the locus."""
        regs = get_regs(locus)
        for r in regs:
            if r not in self.reg_index:
                self.reg_index[r] = set()
            self.reg_index[r].add(cid)

    def deindex_component(self, cid: int, locus: List[Any]) -> None:
        """Remove the component ID from the index for registers in the locus."""
        regs = get_regs(locus)
        for r in regs:
            if r in self.reg_index:
                self.reg_index[r].discard(cid)
                if not self.reg_index[r]:
                    del self.reg_index[r]

    def get_candidates(self, target_locus: List[Any], fallback_cids: Iterable[int]) -> List[int]:
        """
        Find potential CIDs that might own the target locus.
        - Uses the index if available and relevant.
        - Falls back to returning all CIDs if index is empty/misses.
        """
        regs = get_regs(target_locus)
        
        # If we have a useful index, use it
        if self.reg_index:
            cands: Set[int] = set()
            found_any = False
            for r in regs:
                if r in self.reg_index:
                    cands |= self.reg_index[r]
                    found_any = True
            
            # If the registers are in the index, trust the index.
            # If they are NOT in the index, they might be new or unindexed, 
            # so strictly speaking we might return empty or fallback.
            # Assuming 'Conservative' strategy: return what we found.
            return sorted(cands)

        # Fallback: scan everything
        return sorted(list(fallback_cids))

# -----------------------------
# VC + Trace types
# -----------------------------

@dataclass(frozen=True)
class VC:
    antecedent_qstore: Dict[Any, Any] # cid -> QXQSpec snapshot
    antecedent_pc: Tuple[Any, ...] # boolean AST nodes snapshot
    consequent: Any #QXQSpec or boolean
    source_line: Optional[int]
    origin: str

@dataclass(frozen=True)
class TraceStep:
    line: Optional[int]
    stmt: Any
    qstore_snapshot: Dict[Any, Any]
    pc_snapshot: Tuple[Any, ...]

# -----------------------------
# ExecState
# -----------------------------

@dataclass
class ExecState:
    qstore: Dict[Any, Any] = field(default_factory=dict)   # cid -> QXQSpec (states include internal terms)
    pi: PiManager = field(default_factory=PiManager)
    cstore: Dict[str, Any] = field(default_factory=dict)   # var -> QX* expr AST
    pc: List[Any] = field(default_factory=list)            # boolean AST nodes
    binders: List[Dict[str, str]] = field(default_factory=list)  # source-id -> alpha-id
    vcs: List[VC] = field(default_factory=list)
    fresh: FreshNameGen = field(default_factory=FreshNameGen)

    def clone(self, *, copy_pi: bool = False) -> "ExecState":
        """
        - Sequential execution: copy_pi=False (share pi; cheap).
        - Classical branching: copy_pi=True (pi + qstore are isolated across paths).
        """
        if copy_pi:
            pi_copy = PiManager(
                next_cid=self.pi.next_cid,
                reg_index={k: set(v) for k, v in self.pi.reg_index.items()},
            )
        else:
            pi_copy = self.pi

        return ExecState(
            qstore=dict(self.qstore),
            pi=pi_copy,
            cstore=dict(self.cstore),
            pc=list(self.pc),
            binders=[dict(fr) for fr in self.binders],
            vcs=list(self.vcs),
            fresh=self.fresh,  # shared to keep names globally unique across paths
        )

    def push_binder(self, src: str, alpha: str) -> None:
        if not self.binders:
            self.binders.append({})
        self.binders[-1][src] = alpha

    def enter_scope(self) -> None:
        self.binders.append({})

    def exit_scope(self) -> None:
        if self.binders:
            self.binders.pop()

    def alpha_of(self, src: str) -> Optional[str]:
        for fr in reversed(self.binders):
            if src in fr:
                return fr[src]
        return None
