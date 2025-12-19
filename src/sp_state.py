from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple
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
        self._en_ctr += 1
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
    
@dataclass
class PiManager:
    """
    Symbolic component manager:
    - allocates stable component IDs
    - provides optional reg -> {cid} indexing (not authoritative)
    """
    next_cid: int = 0
    reg_index: Dict[str, set[int]] = field(default_factory=dict)

    def new_cid(self) -> int:
        cid = self.next_cid
        self.next_cid += 1
        return cid

    def _regs_of_locus(self, locus: List[Any]) -> set[str]:
        regs: set[str] = set()
        for r in locus:
            try:
                regs.add(str(r.location()))
            except Exception:
                regs.add(str(getattr(r, "location", None)))
        return regs

    def index_component(self, cid: int, locus: List[Any]) -> None:
        for reg in self._regs_of_locus(locus):
            self.reg_index.setdefault(reg, set()).add(cid)

    def drop_component(self, cid: int) -> None:
        for reg in list(self.reg_index.keys()):
            s = self.reg_index.get(reg)
            if s is None:
                continue
            if cid in s:
                s.remove(cid)
            if not s:
                self.reg_index.pop(reg, None)


# -----------------------------
# Simple union-find for Π
# -----------------------------

# @dataclass
# class UnionFind:
#     parent: Dict[LocusAtom, LocusAtom] = field(default_factory=dict)
#     rank: Dict[LocusAtom, int] = field(default_factory=dict)

#     def add(self, x: LocusAtom) -> None:
#         if x not in self.parent:
#             self.parent[x] = x
#             self.rank[x] = 0

#     def find(self, x: LocusAtom) -> LocusAtom:
#         self.add(x)
#         p = self.parent[x]
#         if p != x:
#             self.parent[x] = self.find(p)
#         return self.parent[x]

#     def union(self, a: LocusAtom, b: LocusAtom) -> LocusAtom:
#         ra, rb = self.find(a), self.find(b)
#         if ra == rb:
#             return ra
#         if self.rank[ra] < self.rank[rb]:
#             ra, rb = rb, ra
#         self.parent[rb] = ra
#         if self.rank[ra] == self.rank[rb]:
#             self.rank[ra] += 1
#         return ra

#     def union_all(self, xs: Sequence[LocusAtom]) -> Optional[LocusAtom]:
#         if not xs:
#             return None
#         r = self.find(xs[0])
#         for x in xs[1:]:
#             r = self.union(r, x)
#         return r

#     def roots(self, xs: Iterable[LocusAtom]) -> List[LocusAtom]:
#         return sorted({self.find(x) for x in xs})

#     def component_atoms(self, root: LocusAtom) -> List[LocusAtom]:
#         rr = self.find(root)
#         return sorted([a for a in self.parent.keys() if self.find(a) == rr])

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
    qstore: Dict[Any, Any] = field(default_factory=dict)   # cid -> QXQSpec
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
