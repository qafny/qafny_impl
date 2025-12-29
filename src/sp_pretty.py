from __future__ import annotations
from typing import Any, Iterable

# -------------------------
# Core pretty-printer
# -------------------------

def pp(obj: Any) -> str:
    if obj is None:
        return "∅"

    cn = obj.__class__.__name__

    # ---- internal terms ----
    if cn == "IApply":
        # Avoid redundant "@ target" when op already carries targets (e.g., ICtrlGate)
        op_cn = obj.op.__class__.__name__
        if op_cn in {"ICtrlGate", "ICtrlBody"}:
            return f"{pp(obj.op)}({pp(obj.term)})"

        # Generic op: print Apply[op @ targets](term)
        try:
            tgt = ", ".join(pp(x) for x in obj.target)
            return f"Apply[{pp(obj.op)} @ {tgt}]({pp(obj.term)})"
        except Exception:
            return f"{pp(obj.op)}({pp(obj.term)})"



    if cn == "ICtrlGate":
        ctrls = ", ".join(pp(x) for x in obj.controls)
        tgts  = ", ".join(pp(x) for x in obj.targets)
        return f"Ctrl[{ctrls}]({pp(obj.op)} @ {tgts})"

    if cn == "ICtrlBody":
        ctrls = ", ".join(pp(x) for x in obj.controls)
        return f"CtrlBody[{ctrls}](...)"  # intentionally compact

    if cn == "IIter":
            return (f"Iter[{obj.binder} in {pp(obj.lo)}..{pp(obj.hi)}]"
            f"({pp(obj.body_summary)} {pp(obj.term)})")

    if cn == "ILoopSummary":
        steps = ", ".join(pp(s) for s in obj.steps)
        return f"Summary[{steps}]"


    if cn == "IStep":
        op_cn = obj.op.__class__.__name__
        if op_cn in {"ICtrlGate", "ICtrlBody"}:
            return pp(obj.op)
        tgt = getattr(obj, "target", ())
        return f"Apply[{pp(obj.op)} @ {', '.join(pp(x) for x in tgt)}]"


    if cn == "ITensorProd":
        return " ⊗ ".join(pp(x) for x in obj.factors)
    
    if cn == "IPostSelect":
        # show the key info without exploding the term
        try:
            locus = ", ".join(pp(x) for x in obj.meas_locus)
            v = pp(obj.meas_value)
            prob = pp(obj.prob_sym) if getattr(obj, "prob_sym", None) is not None else "∅"
            return f"PostSelect[{locus}={v}, prob={prob}]⟪{pp(obj.pre_term)}⟫"
        except Exception:
            return "PostSelect[...]"


    # ---- QSpec / locus ----
    if cn == "QXQSpec":
        locus = ", ".join(pp(x) for x in obj.locus())
        qty   = pp(obj.qty()) if hasattr(obj, "qty") else "?"
        try:
            states = list(obj.states())
        except Exception:
            states = list(getattr(obj, "states", []) or [])
        rhs = " + ".join(pp(t) for t in states) if states else "<?>"
        return f"{{ {locus} : {qty} ↦ {rhs} }}"

    if cn == "QXQRange":
        loc = str(obj.location())
        cr  = obj.crange()
        return f"{loc}[{pp(cr.left())},{pp(cr.right())})"
    
    if cn == "QXCRange":
        try:
            return f"[{pp(obj.left())},{pp(obj.right())})"
        except Exception:
            return "Range(...)"
        


    # ---- gates / expr ----
    if cn == "QXSingle":
        return str(obj.op())

    if cn == "QXOracle":
        try:
            bind_str = ""
            if obj.bindings():
                bind_str = ", ".join(pp(b) for b in obj.bindings()) + " -> "                
            parts = []
            if obj.phase():
                parts.append(f"phase={pp(obj.phase())}")            
            kets = obj.kets()
            if kets:
                k_str = ", ".join(pp(k) for k in kets)
                parts.append(f"kets=[{k_str}]")                
            body = ", ".join(parts)
            return f"Oracle({bind_str}{body})"
        except Exception:
            return "Oracle(?)"

    # ---- types ----
    if cn == "TyNor":
        return "Nor"
    if cn == "TyHad":
        return "Had"
    if cn == "TyEn":
        return f"En({pp(obj.flag())})"

    # ---- booleans / arithmetic ----
    if cn == "QXComp":
        return f"({pp(obj.left())} {obj.op()} {pp(obj.right())})"

    if cn == "QXLogic":
        return f"({pp(obj.left())} {obj.op()} {pp(obj.right())})"

    if cn == "QXBind":
        return str(obj.ID())

    if cn == "QXNum":
        return str(obj.num())
    
    if cn == "QXHad":
        return str(obj.state())

    if cn == "QXBin":
        return f"({pp(obj.left())} {obj.op()} {pp(obj.right())})"

    if cn == "QXUni":
        return f"{obj.op()}({pp(obj.next())})"

    # ---- tensor / ket ----
    if cn == "QXTensor":
        try:
            binder = obj.ID() if callable(getattr(obj, "ID", None)) else None
            cr = obj.range() if callable(getattr(obj, "range", None)) else None
            ks = obj.kets() if callable(getattr(obj, "kets", None)) else None

            # normalize ks into a list for printing
            if ks is None:
                klist = []
            elif isinstance(ks, (list, tuple)):
                klist = list(ks)
            else:
                klist = [ks]

            # Comprehension tensor: ⊗ i∈[lo,hi) <ket>
            if binder is not None and cr is not None:
                lo = cr.left() if callable(getattr(cr, "left", None)) else getattr(cr, "_left", None)
                hi = cr.right() if callable(getattr(cr, "right", None)) else getattr(cr, "_right", None)
                inner = pp(klist[0]) if klist else "<?>"
                return f"⊗ {binder}∈[{pp(lo)},{pp(hi)}) {inner}"

            # Plain tensor product of listed kets
            if not klist:
                return "⊗()"
            return " ⊗ ".join(pp(k) for k in klist)

        except Exception:
            return "Tensor(...)"


    if cn == "QXSKet":
        return f"|{pp(obj.vector())}⟩"

    if cn == "QXSum":
        # QXSum(sums=[QXCon(...)], amp=..., kets=...)
        try:
            cons = obj.sums() if callable(getattr(obj, "sums", None)) else getattr(obj, "sums", [])
            amp  = obj.amp()  if callable(getattr(obj, "amp", None))  else getattr(obj, "amp", None)
            kets = obj.kets() if callable(getattr(obj, "kets", None)) else getattr(obj, "kets", None)

            # binder part
            if cons:
                bind = ", ".join(pp(c) for c in cons)
                return f"∑ {bind}. {pp(amp)} · {pp(kets)}"
            else:
                return f"∑. {pp(amp)} · {pp(kets)}"
        except Exception:
            return "Sum(...)"

    if cn == "QXCon":
        try:
            var = obj.ID() if callable(getattr(obj, "ID", None)) else getattr(obj, "id", "?")
            cr  = obj.crange() if callable(getattr(obj, "crange", None)) else getattr(obj, "crange", None)
            cond = obj.condtion() if callable(getattr(obj, "condtion", None)) else getattr(obj, "condtion", None)

            if cr is None:
                core = str(var)
            else:
                core = f"{var}∈[{pp(cr.left())},{pp(cr.right())})"

            if cond is None:
                return core
            return f"{core} | {pp(cond)}"
        except Exception:
            return "Con(...)"
        
    if cn == 'QXCall':
        try:
            fid = obj.ID() if callable(getattr(obj, "ID", None)) else getattr(obj, "id", "?")
            args= obj.exps() if callable(getattr(obj, "exps", None)) else getattr(obj, "exps", None)
            return f"{fid}({', '.join(pp(arg) for arg in args)})" if args else f"{fid}()"

        except Exception:
            return "Function(...)"
        
    if cn == "VC":
        return pp_vc(obj)
    
    if cn == "DischargeResult":
        return pp_discharge_result(obj)


    # fallback
    return cn


# -------------------------
# VC formatting
# -------------------------

def pp_pc(pc: Iterable[Any]) -> str:
    pc = list(pc or [])
    if not pc:
        return "True"
    return " ∧ ".join(pp(c) for c in pc)

def pp_qstore(qstore: dict) -> str:
    if not qstore:
        return "{}"
    parts = []
    for cid, spec in qstore.items():
        parts.append(f"C{cid}: {pp(spec)}")
    return " ∧ ".join(parts)

def pp_vc(vc: Any) -> str:
    """Format one VC as:  [origin@line]  (pc ∧ qstore) ⇒ consequent"""
    origin = getattr(vc, "origin", "vc")
    line   = getattr(vc, "source_line", None)
    header = f"[{origin}@{line}]" if line is not None else f"[{origin}]"

    ant_pc = getattr(vc, "antecedent_pc", [])
    ant_qs = getattr(vc, "antecedent_qstore", {})
    consq  = getattr(vc, "consequent", None)

    lhs = f"({pp_pc(ant_pc)} ∧ {pp_qstore(ant_qs)})"
    rhs = pp(consq)
    return f"{header} {lhs} ⇒ {rhs}"

def pp_discharge_result(res: Any) -> str:
    status = "OK" if res.ok else "FAIL"
    reason = res.reason
    vc_str = pp_vc(res.norm_vc)
    # Indent the VC for better readability
    indented_vc = vc_str.replace('\n', '\n    ')
    return (f"  Result: [{status}]\n"
            f"  Reason: {reason}\n" 
            f"  {indented_vc}") 
