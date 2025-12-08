from dataclasses import dataclass
from typing import List, Dict, Union, Any

# ────────── Symbolic Expressions ──────────
@dataclass
class SymExpr: 
    def __repr__(self): return self.__class__.__name__

# 1. State Transformers
@dataclass
class SymInit(SymExpr):
    def __repr__(self): return "InitState"

@dataclass
class SymGate(SymExpr):
    op_name: str
    target_reg: str
    prev_state: SymExpr
    def __repr__(self): return f"Apply({self.op_name} on {self.target_reg}, after={self.prev_state})"

@dataclass
class SymOracle(SymExpr):
    op_name: str
    target_reg: str
    prev_state: SymExpr
    def __repr__(self): return f"Oracle({self.op_name} on {self.target_reg}, after={self.prev_state})"

@dataclass
class SymPhase(SymExpr):
    target_reg: str
    bound_var: str        
    phase_expr: SymExpr   
    prev_state: SymExpr
    def __repr__(self): return f"Phase(lambda {self.bound_var}: {self.phase_expr} on {self.target_reg}, after={self.prev_state})"

# --- Loop Sandbox Nodes ---
@dataclass
class SymLoopStart(SymExpr):
    """Marker for the beginning of a loop iteration context."""
    def __repr__(self): return "LoopStart"

@dataclass
class SymFold(SymExpr):
    iterator: str
    start: SymExpr
    end: SymExpr
    body_expr: SymExpr 
    prev_state: SymExpr
    def __repr__(self): return f"Fold(iter={self.iterator}, range=[{self.start}:{self.end}], after={self.prev_state})"

@dataclass
class SymMeasureOp(SymExpr):
    target_reg: str
    out_vars: List[str]
    prev_state: SymExpr
    def __repr__(self): return f"Measure({self.target_reg}->{self.out_vars}, after={self.prev_state})"

@dataclass
class SymQuantumIf(SymExpr):
    control_reg: str        
    control_logic: SymExpr
    then_state: SymExpr
    else_state: SymExpr
    def __repr__(self): return f"QIf({self.control_reg} ? ...)"

@dataclass
class SymClassicalIf(SymExpr):
    condition: SymExpr
    then_state: SymExpr
    else_state: SymExpr
    def __repr__(self): return f"If({self.condition} ? ... : ...)"

@dataclass
class SymMasked(SymExpr): 
    control_qubit: SymExpr
    op: SymExpr
    def __repr__(self): return f"Mask({self.control_qubit} ? {self.op})"

# 2. Values & Logic
@dataclass
class SymVar(SymExpr): 
    name: str
    def __repr__(self): return self.name

@dataclass
class SymInt(SymExpr): 
    val: int
    def __repr__(self): return str(self.val)

@dataclass
class SymString(SymExpr):
    val: str
    def __repr__(self): return f"{self.val}"

@dataclass
class SymArg(SymExpr):
    index: int
    def __repr__(self): return f"Arg{self.index}"

@dataclass
class SymMeasurementResult(SymExpr):
    source_op: SymMeasureOp
    var_name: str
    def __repr__(self): return f"Result({self.var_name} from {self.source_op})"

@dataclass
class SymBinOp(SymExpr):
    op: str
    left: SymExpr
    right: SymExpr
    def __repr__(self): return f"({self.left} {self.op} {self.right})"

@dataclass
class SymUniOp(SymExpr):
    op: str
    expr: SymExpr
    def __repr__(self): return f"{self.op}({self.expr})"

@dataclass
class SymCall(SymExpr): 
    name: str
    args: List[SymExpr]
    def __repr__(self): return f"{self.name}({', '.join(map(str, self.args))})"

@dataclass
class SymMathSum(SymExpr):
    iterator: str
    start: SymExpr
    end: SymExpr
    expr: SymExpr
    def __repr__(self): return f"MathSum({self.iterator}={self.start}..{self.end}, {self.expr})"

@dataclass
class SymIte(SymExpr):
    cond: SymExpr
    then_expr: SymExpr
    else_expr: SymExpr
    def __repr__(self): return f"({self.cond} ? {self.then_expr} : {self.else_expr})"

# 3. Assertions
@dataclass
class SymAssert(SymExpr):
    cond: SymExpr
    state_context: SymExpr
    def __repr__(self): return f"Assert({self.cond})"

@dataclass
class SymCheckSum(SymExpr):
    registers: List[str]
    condition: SymExpr 
    amplitude: SymExpr
    ket_expr: SymExpr
    ket_var: str
    state_context: SymExpr
    def __repr__(self): return f"VerifySum(regs={self.registers}, ket=|{self.ket_expr}>)"

@dataclass
class SymCheckState(SymExpr):
    reg: str
    val_expr: SymExpr
    state_context: SymExpr
    def __repr__(self): return f"CheckState({self.reg} == |{self.val_expr}>)"


# ────────── The SP Visitor ──────────

class SPVisitor:
    def __init__(self):
        self.sym_store: Dict[str, SymExpr] = {}
        self.curr_state = SymInit()
        self.asserts: List[SymExpr] = []
        self.reg_specs: Dict[str, SymExpr] = {}

    def visit(self, node):
        if node is None: return
        method_name = 'visit' + node.__class__.__name__[2:]
        if hasattr(self, method_name):
            return getattr(self, method_name)(node)
        if hasattr(node, 'stmts'):
            for s in node.stmts(): s.accept(self)
        elif hasattr(node, 'topLevelStmts'):
            for s in node.topLevelStmts(): s.accept(self)

    def visitProgram(self, node):
        for stmt in node.topLevelStmts(): stmt.accept(self)

    def visitMethod(self, node):
        if hasattr(node, 'bindings'):
            for bind in node.bindings():
                bind_id = self._clean_id(bind.ID())
                self.sym_store[bind_id] = SymVar(bind_id)
                ty = bind.type()
                if ty.__class__.__name__ == 'TyQ':
                    size_expr = self.eval_expr(ty.flag())
                    self.reg_specs[bind_id] = size_expr

        for stmt in node.stmts(): stmt.accept(self)
        
        if hasattr(node, 'conds'):
            for cond in node.conds():
                if cond.__class__.__name__ == 'QXEnsures':
                    self.visit(cond)
                #    cond.accept(self)

    def _clean_id(self, raw_id):
        s = str(raw_id).strip()
        return s.replace("'", "").replace('"', '')

    def eval_expr(self, node, loop_context=None) -> SymExpr:
        if hasattr(node, 'num'): return SymInt(node.num())
        if node.__class__.__name__ == 'QXHad': return SymString(node.state())
        
        if hasattr(node, 'exps'):
            func_name = node.id() if hasattr(node, 'id') else node.ID()
            func_name = self._clean_id(func_name)
            args = [self.eval_expr(a, loop_context) for a in node.exps()]
            return SymCall(func_name, args)
            
        if hasattr(node, 'ID'): 
            name = self._clean_id(node.ID())
            if loop_context and name in loop_context: return SymVar(name)
            return self.sym_store.get(name, SymVar(name))
            
        if hasattr(node, 'op'):
            if hasattr(node, 'next'):
                return SymUniOp(node.op(), self.eval_expr(node.next(), loop_context))
            return SymBinOp(node.op(), self.eval_expr(node.left(), loop_context), self.eval_expr(node.right(), loop_context))
        return SymVar("Unknown")

    def _is_trivial_phase(self, node):
        if hasattr(node, 'num') and node.num() == 0: return True
        if hasattr(node, 'exps'): 
             name = node.id() if hasattr(node, 'id') else node.ID()
             name = self._clean_id(name)
             if name == 'omega':
                 args = node.exps()
                 if args and hasattr(args[0], 'num') and args[0].num() == 0:
                     return True
        return False

    def visitQAssign(self, node):
        loc_node = node.location()[0]
        reg_name = self._clean_id(loc_node.location())
        rhs = node.exp()
        
        if hasattr(rhs, 'op'):
            op_name = rhs.op()
            self.curr_state = SymGate(op_name, reg_name, self.curr_state)
            return

        if rhs.__class__.__name__ == 'QXOracle':
            bound_vars = [self._clean_id(b.ID()) for b in rhs.bindings()]
            bound_var = bound_vars[0] if bound_vars else 'x'

            has_nontrivial_phase = False
            if hasattr(rhs, 'phase') and rhs.phase():
                if not self._is_trivial_phase(rhs.phase()):
                    has_nontrivial_phase = True
            
            if has_nontrivial_phase:
                phase_ast = rhs.phase()
                loop_ctx_set = set(bound_vars)
                sym_phase_expr = self.eval_expr(phase_ast, loop_context=loop_ctx_set)
                
                self.curr_state = SymPhase(
                    target_reg=reg_name, 
                    bound_var=bound_var,
                    phase_expr=sym_phase_expr, 
                    prev_state=self.curr_state
                )
            else:
                self.curr_state = SymOracle("Increment", reg_name, self.curr_state)

    def visitFor(self, node):
        start = self.eval_expr(node.crange().left())
        end = self.eval_expr(node.crange().right())
        iterator = self._clean_id(node.ID())
        
        outer_state = self.curr_state
        self.curr_state = SymLoopStart()
        for s in node.stmts(): self.visit(s)
        body_effect_tree = self.curr_state
        self.curr_state = SymFold(iterator, start, end, body_effect_tree, outer_state)

    def visitMeasure(self, node):
        loc_node = node.locus()[0]
        reg_name = self._clean_id(loc_node.location())
        out_vars = [self._clean_id(b.ID()) for b in node.ids()]
        
        meas_op = SymMeasureOp(reg_name, out_vars, self.curr_state)
        self.curr_state = meas_op
        
        for v in out_vars:
            self.sym_store[v] = SymMeasurementResult(meas_op, v)

    def visitIf(self, node):
        bexp = node.bexp()
        if hasattr(bexp, 'location'): 
            reg = self._clean_id(bexp.location())
            idx_node = bexp.crange().left()
            idx_expr = self.eval_expr(idx_node)
            control_logic = SymCall("Access", [self.sym_store.get(reg, SymVar(reg)), idx_expr])
            
            start_state = self.curr_state
            for s in node.stmts(): self.visit(s)
            then_state = self.curr_state
            
            self.curr_state = start_state
            if hasattr(node, 'else_stmts') and node.else_stmts():
                 for s in node.else_stmts(): self.visit(s)
            else_state = self.curr_state
            
            self.curr_state = SymQuantumIf(reg, control_logic, then_state, else_state)
        else:
            cond_expr = self.eval_expr(bexp)
            start_state = self.curr_state
            for s in node.stmts(): self.visit(s)
            then_state = self.curr_state
            self.curr_state = start_state
            if hasattr(node, 'else_stmts') and node.else_stmts():
                for s in node.else_stmts(): self.visit(s)
            else_state = self.curr_state
            self.curr_state = SymClassicalIf(cond_expr, then_state, else_state)

    def visitEnsures(self, node):
        spec = node.spec()
        if hasattr(spec, 'op'):
            cond = self.eval_expr(spec)
            self.asserts.append(SymAssert(cond, self.curr_state))
            return

        if spec.__class__.__name__ == 'QXQSpec':
            states = spec.states()
            if states and states[0].__class__.__name__ == 'QXSum':
                qx_sum = states[0]
                sum_vars = [self._clean_id(s.ID()) for s in qx_sum.sums()]
                kets = qx_sum.kets()
                ket_vec_node = kets.kets()[0].vector()
                ket_var = self._clean_id(ket_vec_node.ID()) if hasattr(ket_vec_node, 'ID') else None
                sym_ket = self.parse_condition(ket_vec_node, ket_var)

                sum_con = qx_sum.sums()[0] 
                ast_cond = sum_con.condition()
                sym_cond = self.parse_condition(ast_cond, ket_var) if ast_cond else None
                
                raw_amp = self.eval_expr(qx_sum.amp(), loop_context=set(sum_vars))
                final_amp = raw_amp
                for s in reversed(qx_sum.sums()):
                    loop_var = self._clean_id(s.ID())
                    if loop_var != ket_var:
                        rng = s.range() if hasattr(s, 'range') else s.crange()
                        start = self.eval_expr(rng.left())
                        end = self.eval_expr(rng.right())
                        final_amp = SymMathSum(loop_var, start, end, final_amp)
                
                locus_reg = self._clean_id(spec.locus()[0].location())

                self.asserts.append(SymCheckSum(
                    registers=[locus_reg], 
                    condition=sym_cond,
                    amplitude=final_amp,
                    ket_expr=sym_ket,
                    ket_var=ket_var,
                    state_context=self.curr_state
                ))
            
            elif states and states[0].__class__.__name__ == 'QXTensor':
                kets = states[0].kets()
                if kets and hasattr(kets[0], 'vector'):
                    target_val = self.eval_expr(kets[0].vector())
                    reg_name = self._clean_id(spec.locus()[0].location())
                    self.asserts.append(SymCheckState(reg_name, target_val, self.curr_state))

    def parse_condition(self, ast_node, loop_var_name):
        if hasattr(ast_node, 'op'): 
            return SymBinOp(ast_node.op(), self.parse_condition(ast_node.left(), loop_var_name), self.parse_condition(ast_node.right(), loop_var_name))
        if hasattr(ast_node, 'id') or hasattr(ast_node, 'ID'):
            func_name = ast_node.id() if hasattr(ast_node, 'id') else ast_node.ID()
            func_name = self._clean_id(func_name)
            if func_name == 'countN':
                arg0 = self.parse_condition(ast_node.exps()[0], loop_var_name)
                return SymCall('PopCount', [arg0])
        if hasattr(ast_node, 'ID'):
            var_name = self._clean_id(ast_node.ID())
            if var_name == loop_var_name: return SymArg(0) 
            else: return SymVar(var_name)
        if hasattr(ast_node, 'num'): return SymInt(ast_node.num())
        return SymVar("UnknownCond")