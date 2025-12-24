from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Sequence, Tuple

from sp_state import ExecState, TraceStep, VC
from sp_eval import try_eval_int, as_bool_not

from sp_terms import ICtrlGate, IIter, ILoopSummary, IStep, IPostSelect, IMeasure, apply_op_to_qspec
from sp_components import ensure_component_for
from sp_normalization import normalize_qspec
from sp_discharge import DischargeResult, discharge_all
from SubstAExp import SubstAExp



@dataclass
class SPResult:
    finals: List[ExecState]
    vcs: List[VC]
    ok_vcs: List[DischargeResult]
    bad_vcs: List[DischargeResult]
    trace: Optional[List[TraceStep]] = None


def compute_sp(ast, arg_values: Optional[Dict[str, Any]] = None, want_trace: bool = False, want_discharge: bool = True) -> SPResult:
    """Compute strongest postcondition(s) for a Qafny program.
    It is path-aware for classical if; quantum-if is summarized without path splitting.
    """
    v = SPVisitor(arg_values=arg_values or {}, want_trace=want_trace, want_discharge=want_discharge)

    try:
        ast.accept(v)
    except Exception:
        raise
    finals = v.finals
    vcs = v.all_vcs()

    ok_vcs, bad_vcs = ([], [])
    if want_discharge:
        ok_vcs, bad_vcs = discharge_all(finals)

    print('\n ok: ', ok_vcs)
    print('\n bad: ', bad_vcs)

    return SPResult(
        finals=finals,
        vcs=vcs,
        ok_vcs=ok_vcs,
        bad_vcs=bad_vcs,
        trace=v.trace if want_trace else None,
    )



# -----------------------------
# Visitor
# -----------------------------

class SPVisitor:
    """
    Key: visitor methods mutate self.paths (PathSet). 

    """

    def __init__(self, *, arg_values: Dict[str, Any], want_trace: bool = False, want_discharge: bool = False):
        self.arg_values = dict(arg_values)
        self.want_trace = want_trace
        self.want_discharge = want_discharge
        self.trace: Optional[List[TraceStep]] = [] if want_trace else None

        self.paths: List[ExecState] = []
        self._ensures: List[Any] = []  # list of QXSpec from QXEnsures
        self.finals: List[ExecState] = []

    def visitProgram(self, prog: Any) -> List[ExecState]:
        """
        Handle QXProgram roots.
        If multiple methods exist, we compute SP for each and keep the finals for the last one.
        VCs are accumulated in each path state.
        """

        def _clsname(x: Any) -> str:
            try:
                return x.__class__.__name__
            except Exception:
                return ""

        # Get program expressions (methods/functions). Support both exps() and _exps.
        exps: list = []
        try:
            f = getattr(prog, "exps", None)
            if callable(f):
                exps = list(f() or [])
            else:
                exps = list(getattr(prog, "_exps", []) or [])
        except Exception:
            exps = []

        methods = [e for e in exps if _clsname(e) == "QXMethod"]
        if not methods and _clsname(prog) == "QXMethod":
            methods = [prog]

        finals: List[ExecState] = []
        for m in methods:
            finals = self.visitMethod(m)
        return finals

    # ---- utilities ----

    def all_vcs(self) -> List[VC]:
        vcs: List[VC] = []
        for st in self.finals:
            vcs.extend(st.vcs)
        return vcs

    def _trace_pre(self, stmt: Any) -> None:
        if self.trace is None:
            return
        for st in self.paths:
            self.trace.append(TraceStep(
                line=_line_of(stmt),
                stmt=stmt,
                qstore_snapshot=dict(st.qstore),
                pc_snapshot=tuple(st.pc),
            ))

    def _run_stmt_on_paths(self, stmt: Any, paths: List[ExecState]) -> List[ExecState]:
        """Temporarily run a statement on a provided path set (used by unrolling)."""
        old = self.paths
        self.paths = paths
        stmt.accept(self)
        out = self.paths
        self.paths = old
        return out

    def _run_block_on_paths(self, stmts: Sequence[Any], paths: List[ExecState]) -> List[ExecState]:
        out = paths
        for s in stmts:
            out = self._run_stmt_on_paths(s, out)
        return out

    # ---- AST visitor surface ----

    def visitMethod(self, method: Any) -> List[ExecState]:
        # Fresh initial state
        st = ExecState()

        # Install provided argument values into cstore (best-effort; exact types handled by caller)
        for k, v in (self.arg_values or {}).items():
            st.cstore[k] = v

        # Also install binding defaults if bindings are QXBind nodes with initializers (ignored if absent)
        try:
            bindings = method.bindings() or []
        except Exception:
            bindings = []
        for b in bindings:
            try:
                name = b.ID()
                init = getattr(b, "init", lambda: None)()
                if init is not None and name not in st.cstore:
                    st.cstore[name] = init
            except Exception:
                pass

        # Process method conditions: requires into pc/qstore; collect ensures specs.
        # duck-typing + class-name checks.
        self._ensures = []

        def _clsname(x: Any) -> str:
            try:
                return x.__class__.__name__
            except Exception:
                return ""

        def _get_list(obj: Any, meth: str, attr: str) -> list:
            """Return obj.meth() if callable, else obj.attr if present, else []."""
            try:
                f = getattr(obj, meth, None)
                if callable(f):
                    v = f()
                    return list(v) if v is not None else []
            except Exception:
                pass
            try:
                v = getattr(obj, attr, None)
                return list(v) if v is not None else []
            except Exception:
                return []

        def _get_spec(cond: Any) -> Any:
            """Return cond.spec() if available, else cond._spec, else None."""
            try:
                f = getattr(cond, "spec", None)
                if callable(f):
                    return f()
            except Exception:
                pass
            return getattr(cond, "_spec", None)

        def _is_qspec(x: Any) -> bool:
            # QXQSpec has locus(), qty()/ty(), states() in your AST.
            return hasattr(x, "locus") and hasattr(x, "states")

        conds = _get_list(method, "conds", "_conds")

        for c in conds:
            name = _clsname(c)
            spec = _get_spec(c)
            if name == "QXRequires":
                if spec is None:
                    continue
                if _is_qspec(spec):
                    _install_qspec_into_state(st, spec)
                else:
                    st.pc.append(spec)
            elif name == "QXEnsures":
                if spec is not None:
                    self._ensures.append(spec)

        # Initialize path set
        self.paths = [st]

        # Execute statements sequentially using visitor dispatch
        stmts = _get_list(method, "stmts", "_stmts")
        for s in stmts:
            s.accept(self)

        # Emit ensures VCs for each final path
        for st2 in self.paths:
            for ens in self._ensures:
                st2.vcs.append(VC(
                    antecedent_qstore=dict(st2.qstore), 
                    antecedent_pc=list(st2.pc),
                    consequent=ens,
                    source_line=getattr(ens, "line_number", lambda: None)(),
                    origin="user-ensures",
                ))

        self.finals = self.paths
        return self.paths

    def visitQAssign(self, stmt: Any) -> List[ExecState]:
        self._trace_pre(stmt)
        self.paths = [_sp_qassign(stmt, st) for st in self.paths]
        return self.paths

    def visitMeasure(self, stmt: Any) -> List[ExecState]:
        self._trace_pre(stmt)
        self.paths = [_sp_measure(stmt, st) for st in self.paths]
        return self.paths

    def visitAssert(self, stmt: Any) -> List[ExecState]:
        self._trace_pre(stmt)
        out: List[ExecState] = []
        for st in self.paths:
            _sp_assert(stmt, st)
            out.append(st)
        self.paths = out
        return self.paths

    def visitIf(self, stmt: Any) -> List[ExecState]:
        self._trace_pre(stmt)

        def _clsname(x: Any) -> str:
            try:
                return x.__class__.__name__
            except Exception:
                return ""

        bexp = stmt.bexp()
        # Quantum-if detection must be robust to multiple module loads.
        if _clsname(bexp) == "QXQRange":
            # quantum-if: no path split
            self.paths = [_sp_quantum_if(stmt, st) for st in self.paths]
            return self.paths

        # classical-if: split and run blocks
        then_stmts = stmt.stmts() or []
        else_stmts = stmt.else_stmts() or []

        out: List[ExecState] = []
        for st in self.paths:
            # then
            t = st.clone(copy_pi=True)
            t.pc.append(bexp)
            out.extend(self._run_block_on_paths(then_stmts, [t]))

            # else
            e = st.clone(copy_pi=True)
            e.pc.append(as_bool_not(bexp))
            out.extend(self._run_block_on_paths(else_stmts, [e]))

        self.paths = out
        return self.paths

    def visitFor(self, stmt: Any) -> List[ExecState]:
        self._trace_pre(stmt)

        # unrolling and loop summary
        out: List[ExecState] = []
        for st in self.paths:
            out.extend(_sp_for(stmt, st, self))
        self.paths = out
        return self.paths


# -----------------------------
# Core transfer functions (single-path)
# -----------------------------

def _sp_qassign(stmt: Any, st: ExecState) -> ExecState:
    st = st.clone(copy_pi=False)

    op = stmt.exp()

    # location() is already a list[QXQRange] in your AST
    target_locus = list(stmt.location() or [])
    if not target_locus:
        return st

    cid = ensure_component_for(st, target_locus)
    qspec = st.qstore.get(cid)
    if qspec is None:
        # Conservative: nothing to apply to
        return st

    st.qstore[cid] = apply_op_to_qspec(qspec, op=op, target_locus=target_locus, st=st)
    return st



def _sp_quantum_if(stmt: Any, st: ExecState) -> ExecState:
    st = st.clone(copy_pi=False)

    guard = stmt.bexp()  # QXQRange
    controls_locus = [guard]

    body_stmts = stmt.stmts() or []
    if not body_stmts:
        return st

    # After your AST normalization, body should be a single QXQAssign
    if len(body_stmts) != 1 or body_stmts[0].__class__.__name__ != "QXQAssign":
        # Opaque controlled body fallback: still supported, but keep it symbolic
        ctrl_op = ICtrlGate(controls=tuple(controls_locus), op=body_stmts, targets=tuple())
        touched_locus = list(controls_locus)
        cid = ensure_component_for(st, touched_locus)
        qspec = st.qstore.get(cid)
        if qspec is None:
            return st
        st.qstore[cid] = apply_op_to_qspec(qspec, op=ctrl_op, target_locus=touched_locus, st=st)
        return st

    s = body_stmts[0]
    exp = s.exp()
    targets_locus = list(s.location() or [])

    touched_locus = list(controls_locus) + list(targets_locus)

    ctrl_op = ICtrlGate(
        controls=tuple(controls_locus),
        op=exp,
        targets=tuple(targets_locus),
    )

    cid = ensure_component_for(st, touched_locus)
    qspec = st.qstore.get(cid)
    if qspec is None:
        return st

    st.qstore[cid] = apply_op_to_qspec(qspec, op=ctrl_op, target_locus=touched_locus, st=st)
    return st


@dataclass(frozen=True)
class BodyStep:
    target_atoms: list  # resolved locus atoms
    op_expr: object     # QXSingle or QXOracle (opaque)

def _sp_measure(stmt: Any, st: "ExecState") -> "ExecState":
    """
    Measurement (Milestone: split):
      - bind measurement ids into cstore as QXBind(name)
      - optional solver symbols in st.symtab
      - split the owning component:
          residual: wrap terms with IPostSelect(...)
          measured: locus : Nor ↦ |v⟩
      - add conservative PC constraints about v (and prob if present)
    """
    st = st.clone(copy_pi=False)

    from Programmer import QXBind, QXComp, QXNum, QXTensor, QXSKet, QXQSpec, TyNor
    from sp_components import ensure_component_for
    try:
        from sp_components import owner_component  # if you have it
    except Exception:
        owner_component = None

    try:
        from sp_terms import IPostSelect
    except Exception:
        IPostSelect = None

    # -----------------------
    # Helpers
    # -----------------------
    def _id_name(b: Any) -> str | None:
        if b is None:
            return None
        try:
            nm = b.ID() if callable(getattr(b, "ID", None)) else None
            if isinstance(nm, str) and nm:
                return nm
        except Exception:
            pass
        try:
            s = str(b)
            return s if s else None
        except Exception:
            return None

    def _same_qrange(a: Any, b: Any) -> bool:
        try:
            if str(a.location()) != str(b.location()):
                return False
        except Exception:
            pass
        try:
            return repr(a.crange()) == repr(b.crange())
        except Exception:
            return repr(a) == repr(b)

    # Ensure optional solver symbol table exists
    if not hasattr(st, "symtab") or getattr(st, "symtab") is None:
        st.symtab = {}

    # -----------------------
    # Measurement locus
    # -----------------------
    meas_locus = tuple(stmt.locus() or [])
    if not meas_locus:
        return st

    # -----------------------
    # Bind ids into cstore (AST) and symtab (optional)
    # -----------------------
    ids = list(stmt.ids() or [])

    # Create binds for all ids
    for b in ids:
        nm = _id_name(b)
        if not nm:
            continue
        st.cstore[nm] = QXBind(id=nm)
        if hasattr(st, "fresh") and hasattr(st.fresh, "sym"):
            try:
                st.symtab[nm] = st.fresh.sym(nm, freshen=False)
            except Exception:
                pass

    # Identify measured value var (prefer literal "v")
    v_name = next(( _id_name(b) for b in ids if _id_name(b) == "v" ), None)
    if v_name is None:
        v_name = _id_name(ids[1]) if len(ids) >= 2 else (_id_name(ids[0]) if ids else "v")
    if not v_name:
        v_name = "v"

    v_sym = QXBind(id=v_name)
    st.cstore[v_name] = v_sym

    # Identify probability var
    # - prefer explicit "prob"
    # - else if there are 3 ids, commonly w,v,s and s is prob-like
    prob_name = next(( _id_name(b) for b in ids if _id_name(b) and "prob" in _id_name(b).lower() ), None)
    if prob_name is None and len(ids) >= 3:
        cand = _id_name(ids[2])
        if cand:
            prob_name = cand

    prob_sym = None
    if prob_name:
        prob_sym = QXBind(id=prob_name)
        st.cstore[prob_name] = prob_sym

    # -----------------------
    # Find the owning component (do NOT create a fresh one unless needed)
    # -----------------------
    cid_joint = None
    if owner_component is not None:
        cid_joint = owner_component(st, list(meas_locus))

    if cid_joint is None:
        # fallback: ensure; this may merge or create if needed
        cid_joint = ensure_component_for(st, list(meas_locus))

    joint = st.qstore.get(cid_joint)
    if joint is None:
        return st


    # -----------------------
    # Create measured component: meas_locus : Nor ↦ |v⟩
    # -----------------------
    meas_term = QXTensor(kets=[QXSKet(vector=v_sym)], id=None, crange=None)
    meas_spec = QXQSpec(locus=list(meas_locus), qty=TyNor(), states=[meas_term])

    cid_meas = st.pi.new_cid()
    st.qstore[cid_meas] = meas_spec
    st.pi.index_component(cid_meas, list(meas_locus))

    # -----------------------
    # Residual component: try to subtract meas_locus from joint.locus
    # If subtraction fails, KEEP the joint locus (sound), but still wrap with PostSelect.
    # -----------------------
    old_locus = list(joint.locus()) if callable(getattr(joint, "locus", None)) else list(getattr(joint, "locus", []))
    residual_locus: list[Any] = []
    removed_any = False
    for r in old_locus:
        kill = any(_same_qrange(r, mr) for mr in meas_locus)
        if kill:
            removed_any = True
        else:
            residual_locus.append(r)

    # If we cannot subtract structurally, keep old locus (sound)
    if not removed_any:
        residual_locus = old_locus

    try:
        old_terms = list(joint.states()) if callable(getattr(joint, "states", None)) else list(getattr(joint, "states", []))
    except Exception:
        old_terms = []

    if IPostSelect is not None:
        new_terms = [
            IPostSelect(
                pre_term=t,
                meas_locus=meas_locus,
                meas_value=v_sym,
                prob_sym=prob_sym,
            )
            for t in old_terms
        ]
    else:
        new_terms = old_terms

    try:
        qty = joint.qty()
    except Exception:
        qty = getattr(joint, "qty", None)

    old_spec = st.qstore[cid_joint]

    st.qstore[cid_joint] = QXQSpec(locus=residual_locus, qty=qty, states=new_terms)

    st.pi.deindex_component(cid_joint, old_spec.locus())
    st.pi.index_component(cid_joint, residual_locus)

    # -----------------------
    # PC facts: 0 <= v <= hi, and 0 <= prob <= 1 if present
    # -----------------------
    st.pc.append(QXComp(op="<=", left=QXNum(num=0), right=v_sym))

    # v <= hi (best effort)
    try:
        mr0 = meas_locus[0]
        hi = mr0.crange().right()
        st.pc.append(QXComp(op="<=", left=v_sym, right=hi))
    except Exception:
        pass

    if prob_sym is not None:
        st.pc.append(QXComp(op="<=", left=QXNum(num=0), right=prob_sym))
        st.pc.append(QXComp(op="<=", left=prob_sym, right=QXNum(num=1)))

    return st





def _sp_assert(stmt: Any, st: ExecState) -> None:
    spec = stmt.spec()
    vc = VC(
        antecedent_qstore=dict(st.qstore),
        antecedent_pc=list(st.pc),
        consequent=spec,
        source_line=_line_of(stmt),
        origin="user-assert",
    )

    from sp_discharge import discharge_vc
    r = discharge_vc(st, vc)
    if not r.ok:
        from sp_pretty import pp_vc
        raise AssertionError(f"Assertion failed: {r.reason}\n{pp_vc(vc)}")
    
    st.vcs.append(vc)


def _sp_for(stmt: Any, st: ExecState, v: SPVisitor) -> List[ExecState]:
    """
    Handles loops using SubstAExp for alpha-renaming.
    """

    # --------- helpers ---------

    def _clsname(x: Any) -> str:
        try: return x.__class__.__name__
        except Exception: return ""

    def _get(maybe_obj: Any, meth: str, default=None):
        try:
            f = getattr(maybe_obj, meth, None)
            if callable(f): return f()
        except Exception: pass
        return default

    def _loop_var_name(s: Any) -> str:
        x = _get(s, "ID", None) or _get(s, "id", None)
        if isinstance(x, str): return x
        try: return str(getattr(s, "_id"))
        except Exception: return "i"

    # --------- Alpha Renaming Helpers using Visitor ---------

    def _subst_range(qr: Any, visitor: SubstAExp) -> Any:
        if _clsname(qr) != "QXQRange": return qr
        try:
            from Programmer import QXQRange, QXCRange
            cr = qr.crange()
            # Apply visitor to left and right bounds
            new_cr = QXCRange(left=visitor.visit(cr.left()), right=visitor.visit(cr.right()))
            return QXQRange(location=qr.location(), crange=new_cr)
        except Exception: return qr

    def _subst_gate(op: Any, visitor: SubstAExp) -> Any:
        if op is None: return op
        n = _clsname(op)
        if n == "QXSingle": return op
        if n == "QXOracle":
            try:
                from Programmer import QXOracle, QXSKet, QXCall
                phase = op.phase()
                # Phase is expression or call
                phase2 = visitor.visit(phase)
                
                # Kets vectors are expressions
                kets = op.kets() if callable(getattr(op, "kets", None)) else getattr(op, "kets", [])
                kets2 = []
                for k in (kets or []):
                    if _clsname(k) == "QXSKet":
                        vec2 = visitor.visit(k.vector())
                        kets2.append(QXSKet(vector=vec2))
                    else: kets2.append(k)
                return QXOracle(bindings=list(op.bindings() or []), phase=phase2, kets=kets2, inverse=getattr(op, "inverse", lambda: False)())
            except Exception: return op
        return op

    # --------- extract loop header ---------

    loop_var = _loop_var_name(stmt)
    cr = _get(stmt, "crange", None)
    if cr is None: return [st]

    lo = _get(cr, "left", None)
    hi = _get(cr, "right", None)
    body = _get(stmt, "stmts", []) or []

    # --------- Tier A: unroll ---------

    L = try_eval_int(lo, st.cstore)
    R = try_eval_int(hi, st.cstore)

    UNROLL_LIMIT = 8
    if L is not None and R is not None and 0 <= (R - L) <= UNROLL_LIMIT:
        cur: List[ExecState] = [st]
        saved_paths = v.paths
        try:
            for k in range(L, R):
                nxt: List[ExecState] = []
                for s0 in cur:
                    s1 = s0.clone(copy_pi=False)
                    try:
                        from Programmer import QXNum
                        s1.cstore[loop_var] = QXNum(num=k)
                    except Exception:
                        s1.cstore[loop_var] = k

                    v.paths = [s1]
                    for b in body:
                        b.accept(v)
                    nxt.extend(v.paths)
                cur = nxt
            return cur
        finally:
            v.paths = saved_paths

    # --------- Tier B: symbolic summary ---------

    from sp_components import ensure_component_for
    from sp_qtys import degrade_qty  
    from Programmer import QXBind, QXQSpec

    st2 = st.clone(copy_pi=False)
    st2.enter_scope()

    alpha = st2.fresh.fresh(loop_var)   # e.g. "i#3"
    st2.push_binder(loop_var, alpha)
    
    # Initialize the Substitution Visitor
    # Target is the fresh binder AST node
    alpha_bind = QXBind(id=alpha)
    subst_vis = SubstAExp(loop_var, alpha_bind)

    steps: List[IStep] = []

    for b in body:
        bn = _clsname(b)

        if bn == "QXQAssign":
            tgt0 = tuple(b.location() or [])
            tgt = tuple(_subst_range(r, subst_vis) for r in tgt0)
            op = _subst_gate(b.exp(), subst_vis)
            steps.append(IStep(op=op, target=tgt))

        elif bn == "QXIf" and _clsname(b.bexp()) == "QXQRange":
            guard0 = b.bexp()
            guard = _subst_range(guard0, subst_vis)

            inner = b.stmts() or []
            if len(inner) == 1 and _clsname(inner[0]) == "QXQAssign":
                s = inner[0]
                tgt0 = tuple(s.location() or [])
                tgt = tuple(_subst_range(r, subst_vis) for r in tgt0)
                op = _subst_gate(s.exp(), subst_vis)

                ctrl = ICtrlGate(controls=(guard,), op=op, targets=tgt)
                touched = (guard,) + tgt
                steps.append(IStep(op=ctrl, target=touched))
            else:
                steps.append(IStep(op=b, target=(guard,)))

        else:
            steps.append(IStep(op=b, target=tuple()))

    summary = ILoopSummary(steps=tuple(steps))

    touched_locus: List[Any] = []
    for s in steps:
        for r in s.target:
            if _clsname(r) == "QXQRange":
                touched_locus.append(r)

    if not touched_locus:
        st2.exit_scope()
        return [st2]

    cid = ensure_component_for(st2, touched_locus)
    qspec = st2.qstore.get(cid)
    if qspec is None:
        st2.exit_scope()
        return [st2]

    old_states = list(qspec.states()) if callable(getattr(qspec, "states", None)) else list(getattr(qspec, "states", []))
    new_states = [IIter(binder=alpha, lo=lo, hi=hi, body_summary=summary, term=t) for t in old_states]

    new_qty = degrade_qty(qspec.qty(), op=summary, fresh_flag=st2.fresh.fresh_en_flag())

    st2.qstore[cid] = QXQSpec(locus=list(qspec.locus()), qty=new_qty, states=new_states)

    st2.exit_scope()
    return [st2]

# -----------------------------
# Helpers
# -----------------------------

def _install_qspec_into_state(st: ExecState, qspec: Any) -> None:
    """Install a user qspec into qstore (cid-keyed) and index by registers."""
    cid = st.pi.new_cid()
    st.qstore[cid] = qspec
    try:
        st.pi.index_component(cid, list(qspec.locus()))
    except Exception:
        # if locus() accessor differs, best-effort
        st.pi.index_component(cid, list(getattr(qspec, "_locus", [])) or [])



def _line_of(node: Any) -> Optional[int]:
    try:
        return node.line_number()
    except Exception:
        return getattr(node, "_line_number", None)
