import copy
import os
import time
from contextlib import contextmanager

from Programmer import *
from ProgramVisitor import ProgramVisitor
from TargetProgrammer import *
from CollectKind import *
from SubstDAExp import SubstDAExp
from BindCollector import BindCollector
from TypeChecker import TypeChecker, subLocusGen, compareType
from EqualityChecker import EqualityVisitor
from LocusCollector import LocusCollector   
from UpdateVars import UpdateVars
from TypeCollector import TypeCollector

class StackFactor:
    pass

class EnFactor(StackFactor):
    def __init__(self, condition: DXComp = None):
        self._cond = condition

    def cond(self):
        return self._cond
    
    def push(self, add):
        self._cond = add if self._cond is None else DXBin("&&", self._cond, add)

    def pop(self):
        if isinstance(self._cond, DXBin) and self._cond.op() == "&&":
            self._cond = self._cond.left()   # <-- only if your DXBin exposes .left()
        else:
            self._cond = None
        return self._cond


# ==============================================================================
# == Helper Functions for Locus and Type Manipulation
# ==============================================================================
def eqQRange(q1: QXQRange, q2: QXQRange):
    """Checks for exact equality between two quantum ranges."""
    return (q1.location() == q2.location() and compareAExp(q1.crange().left(),q2.crange().left())
            and compareAExp(q1.crange().right(),q2.crange().right()))

def compareQRange(q1: QXQRange, q2: QXQRange):
    """Checks if two quantum ranges refer to the same variable and start at the same index."""
    return str(q1.location()) == str(q2.location()) and compareAExp(q1.crange().left(),q2.crange().left())

def compareRangeLocus(q1: QXQRange, qs: [QXQRange]):
    """
    Checks if a quantum range q1 is a prefix of a locus qs.
    If it is, it returns the remainder of the locus.
    """
    vs = []
    for i in range(len(qs)):
        if compareQRange(q1,qs[i]):
            if compareAExp(q1.crange().right(), qs[i].crange().right()):
                return (vs + (qs[i+1:len(qs)]))
            else:
                return (vs + [QXQRange(q1.location(), qs[i].index(),
                                       QXCRange(qs[i].crange().left(),
                                                QXBin("+",q1.crange().right(), QXNum(1)),
                                                qs[i].crange().line_number()), qs[i].line_number())]
                        + (qs[i+1:len(qs)]))
        vs = vs + [qs[i]]
    return None

#compareLocus and return the reminder locus
def compareLocus(q1: [QXQRange], q2: [QXQRange]):
    """Checks if locus q1 is a sub-locus of q2 and returns the remainder."""
    vs = q2
    for elem in q1:
        vs = compareRangeLocus(elem, vs)
        if vs is None:
            return None
    return vs

#check if q2 is in the database, and then return locus,qty,var, where q2 is part of locus
def subLocus(q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):
    """Finds which locus in the current state `qs` contains the target locus `q2`."""
    vs = q2
    qsf = []
    for locus,qty,var in qs:
        vs = compareLocus(q2, locus)
        if vs is not None:
            return locus,qty,var
    return None

def sub_locus_from_env(q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):
    """
    Searches the program's current state ('qs', the environment) to find the
    state tuple that fully contains the required locus 'q2'.

    Returns: A tuple containing (found_tuple, remaining_env), or (None, None) if no match is found.
    """
    # Iterate with an index to make removing the found element easy.
    for i, state_tuple in enumerate(qs):
        existing_locus, _, _ = state_tuple
        print(f"\n q2 {q2}, \n existing_locus: {existing_locus}")
        
        # We want to see if the required locus 'q2' is a sub-part of an 'existing_locus'.
        if compareLocus(q2, existing_locus) is not None:
            # A match was found.
            found_tuple = state_tuple
            
            # The remaining environment is everything else in the list.
            remaining_env = qs[:i] + qs[i+1:]
            
            return found_tuple, remaining_env
            
    # If the loop completes without finding a containing locus, return None for both parts.
    return None, None



def makeIndex(id : DXBind, sums: [QXCon]):
    tmp = id
    for elem in sums:
        tmp = DXIndex(tmp, DXBind(elem.ID(),SType("nat")))
    return tmp

def findPos(q1: QXQRange, qs: [QXQRange]):
    for i in range(len(qs)):
        if compareQRange(q1,qs[i]):
            return i
    return -1

def meetType(t1: QXQTy, t2: QXQTy):
    # Fast path: identical types
    if type(t1) is type(t2):
        if isinstance(t1, TyEn):
            return t1 if t1.flag().num() >= t2.flag().num() else t2
        if isinstance(t1, TyAA):
            # choose the larger flag; keep AA structure (qrange/line) from either
            if t1.flag() >= t2.flag().num():
                return t1
            else:
                return t2
        # TyNor vs TyNor, TyHad vs TyHad
        return t1

    # If either is TyNor, the other dominates (unless you intend Nor to be top/bottom differently)
    if isinstance(t1, TyNor):
        return t2
    if isinstance(t2, TyNor):
        return t1

    # If either is TyHad, the non-Nor, non-Had type dominates
    # (i.e., Had yields to En/AA)
    if isinstance(t1, TyHad) and not isinstance(t2, TyNor):
        return t2
    if isinstance(t2, TyHad) and not isinstance(t1, TyNor):
        return t1

    # En vs En``
    if isinstance(t1, TyEn) and isinstance(t2, TyEn):
        return t1 if t1.flag() >= t2.flag() else t2

    # En vs AA — keep AA form, lift flag to the max
    if isinstance(t1, TyEn) and isinstance(t2, TyAA):
        return t2 if t1.flag().num() < t2.flag().num() else TyAA(t1.flag(), t2.qrange(), t2.line_number())
    if isinstance(t1, TyAA) and isinstance(t2, TyEn):
        return t1 if t1.flag().num() >= t2.flag().num() else TyAA(t2.flag(), t1.qrange(), t1.line_number())

    # AA vs AA (different shapes)
    if isinstance(t1, TyAA) and isinstance(t2, TyAA):
        return t1 if t1.flag().num() >= t2.flag().num() else t2

    # Fallback (shouldn’t be reached if all kinds are covered)
    return t1

def gen_nested_seq_type(n: int, t: DXType):
    """Generates a nested Dafny sequence type of depth n."""
    for _ in range(n):
        t = SeqType(t)
    return t

def make_dafny_vars_for_locus(locus: list[QXQRange], q_type: QXQTy, counter: int):
    """
    Creates the Dafny variable bindings (DXBind) for a given Qafny locus and type.
    This is a core function for mapping Qafny's type system to Dafny's.
    """
    var_map = {}
    if isinstance(q_type, TyNor):
        for elem in locus:
            var_map[elem.location()] = DXBind(elem.location(), SeqType(SType("bv1")), counter)
    elif isinstance(q_type, TyEn):
        num = q_type.flag().num()
        for elem in locus:
            var_map[elem.location()] = DXBind(elem.location(), gen_nested_seq_type(num, SeqType(SType("bv1"))), counter)
        # An entangled state always has a corresponding amplitude variable.
        var_map['amp'] = DXBind("amp", gen_nested_seq_type(num, SType("real")), counter)
    elif isinstance(q_type, TyHad):
        for elem in locus:
            var_map[elem.location()] = DXBind(elem.location(), SeqType(SType("real")), counter)
    return var_map

# ==============================================================================
# == Main Visitor Class for Qafny to Dafny Transformation
# ==============================================================================

class ProgramTransfer(ProgramVisitor):
    """
    Transforms a Qafny Abstract Syntax Tree (AST) into a Dafny AST.
    
    This visitor walks the Qafny program structure and generates corresponding
    Dafny code and logical predicates. It uses a predicate transformation
    approach, where the logical state of the quantum system is tracked and
    updated by each quantum operation.
    """

    def __init__(self, kenv: dict, tenv: dict, *, debug: bool | None = None):

        """Initializes the ProgramTransfer visitor."""
        
        # Environment management
        self.kenv = kenv  # Kind environment
        self.tenv = tenv  # Type environment
        self.fvar = ""    # Current function name

        # State management for transformation
        self.varnums = []            # List of (locus, type, var_map) for the current state
        self.outvarnums = []
        self.final_requires = []     # Stores original, unmodified requires clauses
#        self.working_predicate = []  # The current logical state, which gets transformed
        self.en_factor = EnFactor() # Current condition context (for if-statements)
        # Utility state
        self.counter = 0             # Global counter for unique variable names
        self.libFuns = set()         # Tracks required Dafny library functions
        self.addFuns = []          # List of generated ghost methods to add to the program
        self.current_line = 0        # Tracks the line number for generated AST nodes
        self.t_ensures = False  # Flag to indicate if we are in a postcondition context
        self.classical_args = [] # To store classical arguments of the current method
        

        # ---- Debug controls  ----
        # If not provided, respect env var QAFNY_DEBUG=1/0
        self.debug = bool(int(os.getenv("QAFNY_DEBUG", "0"))) if debug is None else bool(debug)
        self._indent = 0  # pretty nesting for logs

    # ==========================================================================
    # == Helpers for debugging
    # ==========================================================================

    # replace your current log() with this exact version
    def log(self, *msg):
        """Lightweight gated logger."""
        if not self.debug:
            return
        try:
            ts = time.strftime("%H:%M:%S")
            indent = "  " * self._indent
            # IMPORTANT: use print(), do NOT call self.log() inside log()
            print(f"\n[{ts}] {indent}" + " ".join(str(m) for m in msg))
        except Exception as e:
            # Last-resort guard: never recurse—still use print
            print(f"[log-error] {e}")

    def _push(self):  # increase visual nesting
        self._indent += 1

    def _pop(self):   # decrease visual nesting
        if self._indent > 0:
            self._indent -= 1

    @contextmanager
    def trace(self, section: str):
        """
        Context manager to log entry/exit and elapsed time for a block.
        Usage: with self.trace("Hadamard transform"): ...
        """
        if not self.debug:
            # Still yield without overhead
            yield
            return
        self.log(f"→ {section}")
        self._push()
        t0 = time.perf_counter()
        try:
            yield
        finally:
            dt_ms = (time.perf_counter() - t0) * 1000.0
            self._pop()
            self.log(f"← {section} ({dt_ms:.2f} ms)")

    # ==========================================================================
    # == Core Visitor Methods (Program and Method Orchestration)
    # ==========================================================================
    def visitProgram(self, ctx: Programmer.QXProgram):
        """Translates the entire Qafny program."""
        methods = [elem.accept(self) for elem in ctx.topLevelStmts()]
        # The main program now includes the main methods and any generated ghost methods.
#        self.log(f"\nGenerated library functions: {self.addFuns}")
        return DXProgram(methods, line=ctx.line_number())

    def visitMethod(self, ctx: Programmer.QXMethod):
        """
        Translates a single Qafny method into a Dafny method.
        This is the main orchestrator for the predicate transformation process.
        """
        """Translates a single Qafny method into a Dafny method."""
        # --- Initialize State ---
        self.fvar = str(ctx.ID())
#        self.log(f"\n self.fvar {self.fvar}")
        self.fkenv = self.kenv.get(self.fvar)
        self.current_line = ctx.line_number()
        self.varnums = []
        tenvp = self.tenv.get(self.fvar)[0]
        self.log(f"\n tenvp {tenvp}")
        for locus, q_type in tenvp:     
            var_map = make_dafny_vars_for_locus(locus, q_type, self.counter)
            self.counter += 1
            self.varnums.append((locus, q_type, var_map))

        # --- Translate Arguments ---
        self.classical_args = [b.accept(self) for b in ctx.bindings() if b.accept(self) is not None]
        dafny_args = self.classical_args[:]
#        self.log(f"\n dafny_args before varnums: {dafny_args}")
        for _, _, var_map in self.varnums:
            dafny_args.extend(sorted(var_map.values(), key=lambda v: v.ID()))

        # --- Translate Preconditions ---

        for cond in ctx.conds():
            if isinstance(cond, QXRequires):
                self.current_line = cond.line_number()
                preds = cond.accept(self)
                if preds:
                    preds_list = preds if isinstance(preds, list) else [preds]
                    self.final_requires.extend(DXRequires(p, line=self.current_line) for p in preds_list if p is not None)
#        self.log(f"\n final_requires: {final_requires}")
        # --- Prepare for Postcondition Translation ---
        out_tenv = self.tenv.get(self.fvar)[1]  
        self.outvarnums = []
        for locus, q_type in out_tenv:         
            var_map = make_dafny_vars_for_locus(locus, q_type, self.counter)
            self.counter += 1
            self.outvarnums.append((locus, q_type, var_map))

        # --- Translate Method Body ---
        dafny_body = []
        if not ctx.axiom():
            for stmt in ctx.stmts():
                self.current_line = stmt.line_number()
                stmts = stmt.accept(self)
                if stmts:
                    dafny_body.extend(stmts if isinstance(stmts, list) else [stmts])
            
            # --- Assign Final State to Return Variables ---
            final_assignments, _ = self._get_final_assignments_and_mapping(self.outvarnums)
            dafny_body.extend(final_assignments)
        
        # --- Translate Postconditions ---
        final_ensures = []
        self.t_ensures = True
        for cond in ctx.conds():
            if isinstance(cond, QXEnsures):
                res = cond.accept(self)
                if res:
                    preds_list = res if isinstance(res, list) else [res]
                    final_ensures.extend(DXEnsures(p, line=self.current_line) for p in preds_list if p is not None)
        self.t_ensures = False
         #-- translate Returns ---           
        
        dafny_returns = [r.accept(self) for r in ctx.returns() if r.accept(self) is not None]
        for _, _, var_map in self.outvarnums:            
            dafny_returns.extend(sorted(var_map.values(), key=lambda v: v.ID()))
        
        all_conditions = self.final_requires + final_ensures
#        self.log(f"\n specs: {all_conditions}")
        
        return DXMethod(self.fvar, ctx.axiom(), dafny_args, dafny_returns, all_conditions, dafny_body, False, line=ctx.line_number())

    # ==========================================================================
    # == Statement Visitors (Predicate Transformers)
    # ==========================================================================
    def visitQAssign(self, ctx: QXQAssign):
        """Transforms the state for a quantum assignment (Oracle or Single gate)."""
        loc_tuple = subLocus(ctx.locus(), self.varnums)
        if not loc_tuple:
            locus_str = ", ".join([str(l) for l in ctx.locus()])
            raise Exception(f"Locus '{locus_str}' for operation not found in current state.")
        
        loc, qty, old_vars = loc_tuple
        
        operation = ctx.exp()
        
        # Dispatch based on the operation type first, then state type.
        if isinstance(operation, QXOracle):
            # Oracles must operate on an entangled state. Autocast if needed.
            if isinstance(qty, TyHad):
                cast_stmts, casted_vars = self._gen_had_to_en_cast_stmts(old_vars, ctx.line_number())
                casted_qty = TyEn(QXNum(1))
                oracle_stmts, final_vars = self._sequence_comprehension_transform(ctx, loc, casted_qty, casted_vars)
                stmts = cast_stmts + oracle_stmts
                new_qty = casted_qty
            elif isinstance(qty, TyEn):
                stmts, final_vars = self._sequence_comprehension_transform(ctx, loc, qty, old_vars)
                new_qty = qty
            elif isinstance(qty, TyNor):
                stmts, final_vars = self._sequence_comprehension_transform(ctx, loc, qty, old_vars)
                new_qty = qty
            else:
                raise Exception(f"Oracle cannot be applied to a state of type {type(qty)}")

        elif isinstance(operation, QXSingle):
            if isinstance(qty, TyEn):
                stmts, new_qty, final_vars = self._hadamard_on_entangled_transform(ctx, loc, qty, old_vars)
            else:
                stmts, new_qty, final_vars = self._direct_call_transform(ctx, loc, qty, old_vars)
        else:
            return [] # Unsupported operation type

        # Update the main state
    #    self.log(f"\n varnum before updates {self.varnums}")
        self._update_state_for_locus(loc, new_qty, final_vars)
    #    self.log(f"\n varnum after updates {self.varnums}")
        return stmts

    def visitOracle(self, ctx: QXOracle):
        bindings=[]
        for binding in ctx.bindings():
            bindings.append(binding.accept(self))
        ket_vectors=[]
        for arg in ctx.vectors():
            ket_vectors.append(arg.accept(self))
        return bindings, ket_vectors
    
    def visitSingle(self, ctx: QXSingle):
        """
        Transforms the state for a single-locus operation like a Hadamard gate.
        """
        # This is a placeholder for the new predicate transformation logic.
        # As a starting point, it will just generate the Dafny call.
        
        loc, qty, old_vars = subLocus(ctx.locus(), self.varnums)
        if not loc:
            raise Exception(f"Error: Locus for operation {ctx.op()} not found in current state.")

        # --- Generate Dafny Code ---
        self.counter += 1
        new_vars = {k: v.newBind(self.counter) for k, v in old_vars.items()}
        
        # For an H gate on a TyNor state:
        if isinstance(qty, TyNor) and ctx.op() == "H":
            self.libFuns.add('hadNorHad')
            call = DXCall("hadNorHad", list(old_vars.values()), line=ctx.line_number())
            assign_stmt = DXAssign(list(new_vars.values()), call, line=ctx.line_number())
            
            # --- Transform Predicate (To be implemented) ---
            # 1. Define the inverse expression for the Hadamard gate.
            # 2. Create SubstDAExp visitors to replace old variables with the inverse expression.
            # 3. Apply visitors to self.working_predicate.
            # (For now, we will just pass the predicate through)

            # --- Update State ---
            new_qty = TyHad()
            self._update_state_for_locus(loc, new_qty, new_vars)
            
            return [DXInit(v, line=ctx.line_number()) for v in new_vars.values()] + [assign_stmt]
            
        # Other cases (H on TyEn, QFT, etc.) would follow a similar pattern.
        # Each would have its own transformation logic.
        return [] # Placeholder

    def visitIf(self, ctx: Programmer.QXIf):
        """
        Translates an if-then-else statement by dispatching to the appropriate
        handler for either a quantum or classical conditional.
        """
        # Case 1: Classical Conditional `if (bool_expr) { ... }`
        if isinstance(ctx.bexp(), QXBool):
            bex = ctx.bexp().accept(self)

            # Create a new visitor to handle the 'then' branch in isolation
            then_visitor = copy.deepcopy(self)
            then_stmts = []
            for stmt in ctx.stmts():
                res = stmt.accept(then_visitor)
                if res: then_stmts.extend(res if isinstance(res, list) else [res])
            
            # Create another visitor for the 'else' branch
            else_visitor = copy.deepcopy(self)
            else_stmts = []
            for stmt in ctx.else_stmts():
                res = stmt.accept(else_visitor)
                if res: else_stmts.extend(res if isinstance(res, list) else [res])
            
            return [DXIf(bex, then_stmts, else_stmts, line=ctx.line_number())]

        # Case 2: Quantum Conditional `if (q[i]) { ... }`
        # This generates an imperative while loop to construct the new state.
        all_stmts = []
        lc = LocusCollector()
        lc.visit(ctx.bexp())
        for stmt in ctx.stmts():
            lc.visit(stmt)
        
        # Unify the state, generating statements for the unification process.
        unification_stmts, unified_locus, unified_qty, current_vars, remaining_vars = self.includeIfLocus(lc.renv)

        self.log(f"\n unification_stmts: {unification_stmts}, \n unified_locus: {unified_locus}, \
              \n unified_qty: {unified_qty}, \n unified_vars: {current_vars}, \n remaining_vars: {remaining_vars}")
        
        all_stmts.extend(unification_stmts)
        # Dispatch to the correct transformation logic based on the operation inside the conditional
        # 3. Iterate through each statement in the block, applying the transformation sequentially.
        for stmt in ctx.stmts():
            if isinstance(stmt, QXQAssign):
                op = stmt.exp()
                if isinstance(op, QXOracle):
                    comprehension_stmts, new_vars = self._sequence_comprehension_transform(ctx, unified_locus, unified_qty, current_vars)
                    new_qty = unified_qty
                elif isinstance(op, QXSingle):
                    comprehension_stmts, new_qty, new_vars = self._controlled_single_gate_transform(ctx, unified_locus, unified_qty, current_vars)
                all_stmts.extend(comprehension_stmts)
                # Update the current state for the next iteration
                current_vars = new_vars
                self.varnums = [(unified_locus, new_qty, current_vars)] + remaining_vars
            else:
                stmts = stmt.accept(self)
                if stmts:
                    all_stmts.extend(stmts)

        #need to refine the check
        lemma_stmts = []
        if unification_stmts:
            lemma_map = {
                "pow": "powTimesMod",
        #        "sqrt": "triggerSqrtMul",
        #        "mergeAmpEn": "invPow2Step"
            }
            for lib_func, lemma_name in lemma_map.items():
                if lib_func in self.libFuns:
                    lemma_stmts.append(DXCall(lemma_name, [], True, line=self.current_line))
                    self.libFuns.add(lemma_name)
        all_stmts.extend(lemma_stmts)
        #pop the condition
        self.en_factor.pop()
        
        
        return all_stmts

    # ==========================================================================
    # == Core Logic for Oracle Transformation
    # ==========================================================================

    def _get_shape_preservation_predicates(self, old_vars, new_vars):
        """
        Returns a list of predicates for preserving the shape of all dimensions.
        """
        if not old_vars: return []
        
        predicates = []
        max_depth = max(self._get_total_nesting_depth(v.type()) for v in old_vars.values())
    
        for i in range(max_depth):
            iterators = [DXBind(f"k{j}", SType("nat")) for j in range(i)]
            length_comps = []
            
            for var_name, old_bind in old_vars.items():
                if self._get_total_nesting_depth(old_bind.type()) > i:
                    new_bind = new_vars[var_name]
                    old_indexed = self._create_indexed_var(old_bind, iterators)
                    new_indexed = self._create_indexed_var(new_bind, iterators)
                    comparison = DXComp("==", DXLength(new_indexed), DXLength(old_indexed), line=self.current_line)
                    length_comps.append(comparison)

            if not length_comps: continue

            body = length_comps[0]
            if len(length_comps) > 1:
                body = DXComp("==", length_comps[0], length_comps[1], line=self.current_line)
                for k in range(2, len(length_comps)):
                    body = DXComp("==", body, length_comps[k], line=self.current_line)
            
            final_predicate = body
            if iterators:
                representative_var = next(v for v in old_vars.values() if self._get_total_nesting_depth(v.type()) > i)
                final_predicate = self._wrap_in_forall(body, iterators, representative_var)
            
            predicates.append(final_predicate)
        return predicates

    def _get_transformation_predicate(self, old_vars, new_vars, rhs_generator):
        """
        A generic helper to build and return the main transformation predicate.
        """
        qubit_vars = {k: v for k, v in old_vars.items() if k != 'amp'}
        if not qubit_vars: return DXBoolValue(True)

        rep_type = next(iter(qubit_vars.values())).type()
        num_iterators = self._get_total_nesting_depth(rep_type) - 1
        iterators = [DXBind(f"k{i}", SType("nat")) for i in range(num_iterators)]

        all_equalities = []
        for var_name, old_bind in sorted(old_vars.items()):
            new_bind = new_vars[var_name]
            indexed_new = self._create_indexed_var(new_bind, iterators)
            
            rhs_expr = rhs_generator(var_name, old_bind, iterators)
            
            if rhs_expr:
                lhs_expr = indexed_new
                indexed_old = self._create_indexed_var(old_bind, iterators)
                if var_name != 'amp' and not EqualityVisitor().visit(rhs_expr, indexed_old):
                    lhs_expr = DXCall("castBVInt", [indexed_new])
                    self.libFuns.add('castBVInt')
                
                all_equalities.append(DXComp("==", lhs_expr, rhs_expr))

        if not all_equalities: return DXBoolValue(True)

        combined_body = self._join_predicates(all_equalities)
        return self._wrap_in_forall(combined_body, iterators, next(iter(qubit_vars.values())))

    def _join_predicates(self, predicates: list):
        """Joins a list of predicates with '&&'."""
        if not predicates:
            return None
        if len(predicates) == 1:
            return predicates[0]
        
        joined = DXLogic("&&", predicates[0], predicates[1])
        for i in range(2, len(predicates)):
            joined = DXLogic("&&", joined, predicates[i])
        return joined


    # ==========================================================================
    # == Helper Methods for State Management
    # ==========================================================================

    def _get_final_assignments_and_mapping(self, outvarnums):
        """
        Generates assignments from the final state variables to the return variables.
        This version is self-contained and does not require a separate helper.
        """
        assignments = []
        mapping = {}
        
        for locus, return_qty, return_vars_map in outvarnums:
            # Initialize variables to store the found state
            final_qty, final_vars_map = None, None
            
            # Get the set of required register names from the ensures locus.
            target_names = {part.location() for part in locus}

            # Iterate through each state in the final environment to find a match
            for current_locus, current_qty, current_vars_map in self.varnums:
                # Check 1: Do the types match?
                if compareType(current_qty, return_qty): # compareType is assumed to exist
                    # Check 2: Do the registers match?
                    current_names = {part.location() for part in current_locus}
                    if target_names == current_names:
                        # We found a direct match.
                        final_qty = current_qty
                        final_vars_map = current_vars_map
                        break # Stop searching once a match is found
            
            # If a matching state was found, create the assignment
            if final_vars_map:
                final_vars = sorted(final_vars_map.values(), key=lambda v: v.ID())
                return_vars = sorted(return_vars_map.values(), key=lambda v: v.ID())

                if final_vars and return_vars:
                    assignments.append(DXAssign(return_vars, final_vars, line=self.current_line))
                    for i in range(len(final_vars)):
                        mapping[final_vars[i].ID()] = return_vars[i]
                            
        return assignments, mapping

    def _update_state_for_locus(self, target_locus: list, new_qty: QXQTy, new_vars: dict):
        """Updates self.varnums by replacing an old locus with its new version."""
        new_varnums = []
        found = False
        for loc, qty, vars in self.varnums:
            # Use compareLocus to check for an exact match
            if compareLocus(target_locus, loc) == []:
                if not found:
                    new_varnums.append((target_locus, new_qty, new_vars))
                    found = True
            else:
                new_varnums.append((loc, qty, vars))
        self.varnums = new_varnums


    # ==========================================================================
    # == Specification and Expression Visitors
    # ==========================================================================

    def visitBind(self, ctx: QXBind):
        
        if isinstance(ctx.type(), TySingle):
            ty = ctx.type().accept(self)
            return DXBind(ctx.ID(), ty, None, line=ctx.line_number())
        if ctx.ID() and not ctx.type():
            return DXBind(ctx.ID(), None, line=ctx.line_number())
        return None

    def visitRequires(self, ctx: Programmer.QXRequires):
        return ctx.spec().accept(self)

    def visitEnsures(self, ctx: QXEnsures):
        # This is no longer the primary path for ensures clauses,
        # but can be kept for other uses.
        return ctx.spec().accept(self)

    def visitQSpec(self, ctx: QXQSpec):
        
        env = self.outvarnums if self.t_ensures else self.varnums
        self.log(f"\n env in QSpec: {env}")
        res = subLocus(ctx.locus(), env)
        self.log(f"\n res {res} in visitQSpec")
        if not res:
            return []
        loc,qty,varbind = res
        
        preds = []

        for st in ctx.states():
            if isinstance(st, QXSum):
                preds.extend(self.visitSum(st, loc, qty, varbind))
            elif isinstance(st, QXTensor):
                preds.extend(self.visitTensor(st, loc, qty, varbind))
        
        return preds
    
    def visitSum(self, ctx: QXSum, loc:[QXQRange], qty:TyEn, varbind:dict):
        """
        Translates a QXSum specification into a complete list of Dafny predicates,
        including both structural (shape) and logical (value) predicates.
        """

        tmp = []
        n = qty.flag().num()
        sum_vars = ctx.sums()
        print(f"\n sum_vars {sum_vars}")

        def _build_sum_forall(body, iterators):
            """Helper to wrap a pred in nested foralls."""
            nested_forall = body
            for i in range(len(iterators) - 1, -1, -1):
                drange = DXInRange(iterators[i], sum_vars[i].range().left().accept(self), sum_vars[i].range().right().accept(self), line=self.current_line)
                if sum_vars[i].condition():
                    drange = DXBin('&&', drange, sum_vars[i].condition().accept(self))
                nested_forall = DXAll(iterators[i], DXLogic("==>", drange, nested_forall, line=self.current_line), line=self.current_line)
            return nested_forall
        
        # Shape predicates
        for elem in loc:
            dvar = varbind.get(elem.location())
            if dvar: 
                left = elem.crange().left().accept(self)            
                qcount = elem.crange().right().accept(self)
                if not self.t_ensures:
                    len_pred = DXComp(">=", qcount, DXNum(0), line=ctx.line_number())
                    is_duplicate = any(EqualityVisitor().visit(len_pred, p) for p in tmp)
                    if not is_duplicate:
                        tmp.append(len_pred)
        
        for i in range(n + 1):
            iterators = [DXBind(sum_vars[j].ID(), SType("nat")) for j in range(i)]

            for var, dvar in varbind.items():
                is_amp = (var == 'amp')
                if is_amp and i >= n:
                    continue
                for it in iterators:
                    dvar = DXIndex(dvar, it)
                # Determine the correct bound for this dimension's length assertion.
                if i < n:
                    bound = sum_vars[i].range().right().accept(self)
                else: # Innermost dimension for basis vectors
                    qrange = next((r for r in loc if r.location() == var), None)
                    bound = qrange.crange().right().accept(self) if qrange else None
                if bound:
                    len_pred = DXComp("==", DXLength(dvar), bound)
                    tmp.append(_build_sum_forall(len_pred, iterators))


        # Amplitude predicate
        ampvar = varbind.get('amp')
        if ampvar:
            amp_val = ctx.amp().accept(self)
            idx_amp = makeIndex(ampvar, sum_vars)
            body = DXComp("==", idx_amp, amp_val, line=ctx.line_number())
            tmp.append(_build_sum_forall(body, [DXBind(sv.ID(), SType("nat"), line=ctx.line_number()) for sv in sum_vars]))

        # Value predicates
        self.log(f"\n kets {ctx.kets()}")
        var_ket = {loc[i].location(): ket for i, ket in enumerate(ctx.kets().kets())}   
        for var, dvar in varbind.items():
            if var == 'amp':
                continue
            ket = var_ket.get(var)
            if ket is None:
                continue
            idx = ket.accept(self)
            eq = DXComp("==", DXCall('castBVInt', [makeIndex(dvar, sum_vars)]), idx, line=ctx.line_number())
            self.libFuns.add('castBVInt')
            tmp.append(_build_sum_forall(eq, [DXBind(sv.ID(), SType("nat"), line=ctx.line_number()) for sv in sum_vars]))

        
        return tmp
    
    def visitTensor(self, ctx:QXTensor, loc:[QXQRange], qty:QXQTy, varbind:dict):
        """
        Translates a QXTensor specification into Dafny assertions.
        This is used for simple, non-superposition states (TyNor and TyHad).
        """
        # A tensor product applies a single ket state to a range of qubits.
        if len(ctx.kets()) != 1:
            raise Exception("Tensor product specification must have exactly one ket state.")
        
        if len(loc) != 1:
            raise Exception("Tensor product specification is expected on a single locus.")

        predicates = []
        elem = loc[0]
        dvar = varbind.get(elem.location())
        if not dvar:
            return []

        # Define the iterator for the forall loop
        if ctx.ID() is None:
            iterator = DXBind("i", SType("nat"))
        else:
            iterator = DXBind(str(ctx.ID()), SType("nat"))

        # --- Shape Predicates ---
        left = elem.crange().left().accept(self)
        right = elem.crange().right().accept(self)
        
        # The length of the Dafny sequence should match the length of the Qafny range.
        if isinstance(left, DXNum) and left.val() == 0:
            range_len_expr = right
        else:
            range_len_expr = DXBin("-", right, left)
        predicates.append(DXComp("==", DXLength(dvar), range_len_expr, line=ctx.line_number()))

        if not self.t_ensures:
            predicates.append(DXComp(">=", right, left, line=ctx.line_number()))
                
        # --- Value Predicate ---
        ket_val = ctx.kets()[0].accept(self)
        
        # The range for the forall loop, e.g., 0 <= i < |dvar|
        if isinstance(qty, TyHad):
            drange = DXInRange(iterator, DXNum(0), DXLength(dvar), line=ctx.line_number())
            body = DXComp("==", DXIndex(dvar, iterator), ket_val, line=ctx.line_number())
            predicates.append(DXAll(iterator, DXLogic("==>", drange, body), line=ctx.line_number()))    
        elif isinstance(qty, TyNor):
            drange = DXInRange(iterator, DXNum(0), DXLength(dvar), line=ctx.line_number())
            body = DXComp("==", DXIndex(dvar, iterator), ket_val, line=ctx.line_number())
            predicates.append(DXAll(iterator, DXLogic("==>", drange, body), line=ctx.line_number())) 
            predicates.append(DXComp("==", DXCall("castBVInt", [dvar], False), ket_val, line=ctx.line_number()))
            self.libFuns.add('castBVInt')
        
        return predicates

    def visitFor(self, ctx: QXFor) -> list:
        # Step 1: Setup Loop Counter
        iterator_name = ctx.ID()
        iterator = DXBind(iterator_name, SType("nat"))
        lower_bound = ctx.range().left().accept(self)
        upper_bound = ctx.range().right().accept(self)
        init_stmt = DXAssign([iterator], lower_bound, True, line=self.current_line)
        loop_condition = DXComp("<", iterator, upper_bound, line=self.current_line)

        # Step 2: Generate Invariant's Base Case and Loop Variables
        qafny_inv = [inv for inv in ctx.conds() if isinstance(inv.spec(), QXQSpec)]
        base_case_stmts, loop_states = self._generate_loop_base_case(qafny_inv, iterator)
        loop_vars = {name: var for _, _, vmap in loop_states for name, var in vmap.items()}

        # Step 3: Translate Loop Invariants and Body in a Symbolic Scope
        dafny_predicates, loop_body_stmts = [], []
        original_varnums = self.varnums
        try:
            # The symbolic environment is now the list of loop states
            self.varnums = loop_states
            for inv_spec in ctx.conds():
                preds = inv_spec.accept(self)
                if preds: dafny_predicates.extend(preds if isinstance(preds, list) else [preds])
            for stmt in ctx.stmts():
                res = stmt.accept(self)
                if res: loop_body_stmts.extend(res if isinstance(res, list) else [res])
            self.log(f"\n self.varnums {self.varnums} \n loop_states {loop_states}")
            assignments, _ = self._get_final_assignments_and_mapping(loop_states)
            loop_body_stmts.extend(assignments)
            # Add helper lemma calls required by the loop body's operations
            lemma_map = {
                # lib_function_name: lambda: DXCall(lemma_name, [args], is_lemma=True)
                'mergeAmpEn': lambda: DXCall('invPow2Step', [iterator], True, line=self.current_line),
                'countN': lambda: DXCall('countStep', [iterator], True, line=self.current_line),
            }
            
            for lib_fun, lemma_generator in lemma_map.items():
                if lib_fun in self.libFuns:
                    lemma_call = lemma_generator()
                    loop_body_stmts.append(lemma_call)
                    # Ensure the lemma function itself is declared if needed
                    self.libFuns.add(lemma_call.ID())
        finally:
            self.varnums = original_varnums

        # Step 4: Update Main State for After the Loop
        if loop_vars:
            modified_reg_names = {l.location() for inv in qafny_inv for l in inv.spec().locus()}
            subst = SubstDAExp(iterator.ID(), upper_bound)
            new_varnums = [e for e in self.varnums if e[0][0].location() not in modified_reg_names]
            for inv in qafny_inv:
                if not any(compareAExp(subst.visit(l.crange().left().accept(self)), subst.visit(l.crange().right().accept(self))) for l in inv.spec().locus()):
                    final_locus = [QXQRange(l.location(), crange=QXCRange(subst.visit(l.crange().left().accept(self)), subst.visit(l.crange().right().accept(self)))) for l in inv.spec().locus()]
                    # Find the correct var_map for this post-loop state
                    post_loop_var_map = next(vmap for loc, _, vmap in loop_states if loc == inv.spec().locus())
                    new_varnums.append((final_locus, inv.spec().qty(), post_loop_var_map))
            self.varnums = new_varnums

        # Step 5: Assemble and Return
     #   dafny_predicates.append(DXComp("<=", lower_bound, iterator))
     #   dafny_predicates.append(DXComp("<=", iterator, upper_bound))       
        increment_stmt = DXAssign([iterator], DXBin("+", iterator, DXNum(1)))
        loop_body_stmts.append(increment_stmt)
        while_loop = DXWhile(loop_condition, loop_body_stmts, dafny_predicates, line=self.current_line)
        return [init_stmt] + base_case_stmts + [while_loop]

    def _generate_loop_base_case(self, qafny_inv, iterator):
        if not qafny_inv: return [], []
        
        base_case_stmts = []
        loop_states = []

        # For each distinct quantum state described in the invariants, create a base case
        for inv_spec in qafny_inv:
            locus = inv_spec.spec().locus()
            qty = inv_spec.spec().qty()
            state_desc = inv_spec.spec().states()[0]

            # Create a new, separate set of loop-carried variables for this state
            if isinstance(qty, TyEn):
                loop_vars = make_dafny_vars_for_locus(locus, qty, self.counter)
                self.counter += 1
            elif isinstance(qty, (TyHad, TyNor)):
                self.log(f"locus_gene_loop_base {locus} \n {self.varnums}")
                _, _, loop_vars, _ = self.compareSingle(locus[0], self.varnums)

            
            # Store this new state
            loop_states.append((locus, qty, loop_vars))
            
            # Generate the base case assignments for these specific variables
#            subst = SubstDAExp(iterator.ID(), iterator.range().left().accept(self))
            assignments = self._spec_state_to_seq_comp(locus, qty, state_desc, loop_vars)
            base_case_stmts.extend(assignments)
        
        self.log(f"\n loop_base_case{base_case_stmts} \n {loop_states}")
            
        return base_case_stmts, loop_states

    def _spec_state_to_seq_comp(self, locus: list, qty: QXQTy, state: QXQState, var_map: dict, substitutor: SubstDAExp = None) -> list:
        
        assignments = []      
        if isinstance(qty, (TyNor, TyHad)) or isinstance(state, QXTensor):
            pass

        elif isinstance(qty, TyEn) or isinstance(state, QXSum):
            
            sum_iterators = state.sums()
            locus_names = [l.location() for l in locus]
            ket_map = {name: state.kets().kets()[i] for i, name in enumerate(locus_names)}

            for name, dafny_var in var_map.items():
                if name == 'amp': expr = state.amp()
                elif name in ket_map: expr = ket_map[name]
                else: continue

                inner_body = expr.accept(self)
                if substitutor: inner_body = substitutor.visit(inner_body)

                if name != 'amp':
                    q_locus = next(l for l in locus if l.location() == name)
                    width_expr = q_locus.crange().right().accept(self)
                    if substitutor: width_expr = substitutor.visit(width_expr)
                    inner_body = DXCall("castIntBV", [inner_body, width_expr])
                    self.libFuns.add("castIntBV")
                
                comprehension = inner_body
                for sum_iter in reversed(sum_iterators):
                    it = sum_iter.accept(self)
                    dit = DXBind(it, SType("nat"))
                    size_expr = sum_iter.range().right().accept(self)
                    if substitutor: size_expr = substitutor.visit(size_expr)

                    spec = DXRequires(DXInRange(dit, DXNum(0), size_expr))
                    comprehension = DXSeqComp(size_expr, dit, spec, comprehension)

                assignments.append(DXAssign([dafny_var], comprehension, True))
            
        return assignments
        
    
    
    
    
    
    
    # def visitFor(self, ctx: Programmer.QXFor):
    #     tmp_current_line = self.current_line
    #     self.current_line = ctx.line_number()
    #     x = ctx.ID()
    #     tmpinvs = []
    #     for inv in ctx.inv():
    #         tmpinvs += [inv.accept(self)]

    #     self.log(f"\n tmpinvs: {tmpinvs}")

    #     tmpstmts = []
    #     for elem in ctx.stmts():
    #         tmpstmts += elem.accept(self)

    #     lbound = ctx.crange().left().accept(self)
    #     rbound = ctx.crange().right().accept(self)
    #     vx = DXBind(x, SType("nat"))
    #     self.current_line = tmp_current_line
    #     return [DXInit(vx, lbound, line=self.current_line),
    #             DXWhile(DXComp("<", vx, rbound, line=self.current_line),
    #                     tmpstmts, tmpinvs, line=self.current_line)]
    
    def visitBin(self, ctx: Programmer.QXBin):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
#        self.log(f"\nVisiting bin: {ctx} with left {left} and right {right}")
        if ctx.op() == '^':
            if isinstance(left, DXNum) and left.val() == 2:
                self.libFuns.add('pow2')
                return DXCall('pow2',[right], line=ctx.line_number())
            else:
                self.libFuns.add('pow')
                return DXCall('pow',[left, right], line=ctx.line_number())

        elif ctx.op() == '/':
            if isinstance(left, DXNum) and left.val() == 1.0:
#            self.log('\ntransfer', DXBin(ctx.op(),  DXNum(1), right, line=ctx.line_number()))
                return DXBin(ctx.op(), DXNum(1.0), right, line=ctx.line_number())
        
        #need to refine the dot product for two vectors
        elif ctx.op() == '.':
            self.libFuns.add('dotProd')
            return DXCall('dotProd', [DXCall('castIntBV', left, right), right], line=ctx.line_number())
        

        return DXBin(ctx.op(), ctx.left().accept(self), ctx.right().accept(self), line=ctx.line_number())

    def visitNum(self, ctx: Programmer.QXNum):
        return DXNum(ctx.num())
    
    def visitLogic(self, ctx: Programmer.QXLogic):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXLogic(ctx.op(), left, right, line=ctx.line_number())
    
    def visitBool(self, ctx: Programmer.QXComp):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
        return DXComp(ctx.op(), left, right, line=ctx.line_number())
    
    def visitCall(self, ctx: Programmer.QXCall):
    #    self.log('\nvisitCall', ctx.ID())
        self.libFuns.add(str(ctx.ID()))
        return DXCall(str(ctx.ID()), [x.accept(self) for x in ctx.exps()], line=ctx.line_number())
    
    def visitUni(self, ctx: Programmer.QXUni):
        if ctx.op() == 'sqrt':
            self.libFuns.add('sqrt')
            return DXCall('sqrt', [DXCast(SType('real'), ctx.next().accept(self), ctx.line_number())])
        return DXUni(ctx.op(), ctx.next().accept(self), line=ctx.line_number())
    
    def visitSingleT(self, ctx: Programmer.TySingle):
        return SType(ctx.type(), line=ctx.line_number())

    def visitArrayT(self, ctx: Programmer.TyArray):
        ty = ctx.type().accept(self)
        return SeqType(ty, line=ctx.line_number())

    def visitFun(self, ctx: Programmer.TyFun):
        super().visitFun(ctx, line=ctx.line_number())

    def visitQ(self, ctx: Programmer.TyQ):
        return ctx.flag().accept(self)

    def visitCNot(self, ctx: Programmer.QXCNot):
        v = ctx.next().accept(self)
        return DXNot(v, line=ctx.line_number())

    def visitHad(self, ctx: Programmer.QXHad):
        # ensure omega is in our list of library functions
        self.libFuns.add('omega')
        if ctx.state() == "+":
            return DXCall("omega", [DXNum(0), DXNum(2)], line=ctx.line_number())
        else:
            return DXCall("omega", [DXNum(1), DXNum(2)], line=ctx.line_number())
    
    def visitCast(self, ctx: Programmer.QXCast):
        """Handles explicit type casting of a quantum state."""
#        self.log('\ncheckpoint')
        loc_tuple = subLocus(ctx.locus(), self.varnums)
#        self.log(f'\nloc_tuple, {loc_tuple}, \nself.varnums {self.varnums}')
        
        if loc_tuple is not None:
            loc, qty, old_vars = loc_tuple
            
            # Check if the cast is for an entire, existing locus
            remainder = compareLocus(ctx.locus(), loc)
            if not remainder: # Exact match
                # Handle specific, supported casts
                if isinstance(qty, TyHad) and isinstance(ctx.qty(), TyEn):
                    stmts, new_vars = self._gen_had_to_en_cast_stmts(old_vars, ctx.line_number())
         #           self.log(f"\n stmts: {stmts} \n new_vars: {new_vars}")
                    self._update_state_for_locus(loc, ctx.qty(), new_vars)
                    return stmts
        
        # Return empty list if no valid cast was performed
        return []
        
    # ==========================================================================
    # == Core Logic for Conditional Transformation
    # ==========================================================================
    def _gen_had_to_en_cast_stmts(self, vars_map: dict, line_number: int):
        """
        Generates imperative statements to "promote" a TyHad state to a TyEn state.
        Returns the new variables and the list of statements.
        """
        stmts = []
        new_vars = {}
        
        new_vars = self._update_vars_type(vars_map, TyEn(QXNum(1)))
     #   self.log(f"\n new_vars in cast: {new_vars}")

        call = DXCall("hadEn", list(vars_map.values()), line=line_number)
        # Use DXAssign with init=True to generate 'var ... :='
        stmts.append(DXAssign(list(new_vars.values()), call, True, line=line_number))
        self.libFuns.add('hadEn')
        
        return stmts, new_vars

    def _gen_en_nor_extend_stmts(self, en_vars: dict, nor_var: DXBind, line_number: int):
        """
        Generates imperative statements to merge a TyNor state into an existing TyEn state.
        """
        stmts = []
        new_vars = self._update_vars(en_vars)
        
        self.libFuns.add('hadNorEn')
        sorted_old_vars = sorted(en_vars.values(), key=lambda v: v.ID())
        nor_name = nor_var.ID()
        en_vars[nor_name] = nor_var
        new_vars = self._update_vars_type(en_vars, TyEn(QXNum(1)))
        call_args = sorted_old_vars + [nor_var]
        
        sorted_new_vars = sorted(new_vars.values(), key=lambda v: v.ID())
        
        call = DXCall('hadNorEn', call_args, line=line_number)
        stmts.append(DXAssign(sorted_new_vars, call, True, line=line_number))
        
        return stmts, new_vars

    def _gen_en_had_merge_stmts(self, en_vars: dict, had_var: DXBind, ctrl_loc: QXQRange, remainder: QXQRange, line_number: int):
        """
        Generates imperative statements to merge a TyHad state (had_var) with a
        TyEn state (en_vars).
        """

        self.log(f"\n en_vars: {en_vars} \n had_var: {had_var}")
        stmts = []
        new_vars = {}

        if remainder is None:
            q_leftover = None
            q_control_bit = had_var
        # 1. Split the Had variable into its leftover part and the control bit

        else:
        
            org_name = had_var.ID()
            q_leftover = DXBind(org_name, had_var.type(), self.counter)
    #        stmts.append(DXAssign([q_leftover], DXCall("cutHad", [had_var]), True, line=line_number))
            stmts.append(DXAssign([q_leftover], DXSlice(had_var, DXNum(1), None), True, line=line_number))
    #        self.libFuns.add("cutHad")
            self.counter += 1

            q_control_bit = DXBind(org_name, had_var.type().type(), self.counter)
            stmts.append(DXAssign([q_control_bit], DXIndex(had_var, DXNum(0)), True, line=line_number))
            self.counter += 1
        
        # 2. Merge each component of the En state

#        self.libFuns.add('triggerSqrtMul')

        ctrl_reg_name = ctrl_loc.location()
        ctrl_idx = ctrl_loc.crange().left().accept(self)
        original_q_var = None
        
        for name, var in en_vars.items():
            new_var_bind = DXBind(name, var.type(), self.counter) # Simplified type
            
            if name == 'amp':               
                call = DXCall('mergeAmpEn', [var, q_control_bit], line=line_number)
                self.libFuns.add('mergeAmpEn')
            elif name == ctrl_reg_name: 
                original_q_var = var             
                call = DXCall('mergeBitEn', [var, ctrl_idx], line=line_number)
                self.libFuns.add('mergeBitEn')
            else:
                # call = DXCall('duplicateMergeBitEn', [var], line=line_number) 
                # self.libFuns.add('duplicateMergeBitEn')
                call = DXBin('+', var, var)            
            stmts.append(DXAssign([new_var_bind], call, True, line=line_number))
            new_vars[name] = new_var_bind
        self.counter += 1

        # 3. Add calls to helper lemmas
        stmts.append(DXCall("omega0", [], True, line=line_number))
        self.libFuns.add("omega0")
#        if original_q_var:
#             stmts.append(DXCall("mergeBitTrigger", [original_q_var, new_vars[ctrl_reg_name], DXLength(DXIndex(original_q_var, DXNum(0)))], True, line=line_number))
        #     stmts.append(DXCall('pow2add', [], True, line=line_number))
#             self.libFuns.add("mergeBitTrigger")
        #     self.libFuns.add('pow2add')

        self.log(f"\n stmts: {stmts} \n new_vars: {new_vars} \n q_leftover: {q_leftover}")
            
        return stmts, new_vars, q_leftover

    def collectNorLocus(self, q2: list[QXQRange], qs: list[tuple[list[QXQRange], QXQTy, dict]]):
        """
        Find *all* Nor-typed loci in `qs` that match any target in `q2`, using compareSingle.
        Returns:
        - found_loci: flat list of matched QXQRange (order of discovery)
        - vars_merged: merged vars map from all matched entries (first-wins on conflicts)
        - remaining: updated `qs` after carving out the matched Nor parts (non-Nor unchanged)
        """
        # Work on Nor-only pool; pass non-Nor through unchanged
        remaining_nor = [x for x in qs if isinstance(x[1], TyNor)]
        passthrough   = [x for x in qs if not isinstance(x[1], TyNor)]

        found_loci: list[QXQRange] = []
        vars_merged: dict = {}

        for target in q2:
            while True:
                res = self.compareSingle(target, remaining_nor)
                if res is None:
                    break
                orig_var, matched_locus, matched_vars, remaining_nor = res

                # matched_locus is a list (e.g., [target]); extend the flat list
                found_loci.extend(matched_locus)

                # merge vars (keep first value on key conflicts; tweak if you prefer last-wins)
                for k, v in matched_vars.items():
                    if k not in vars_merged:
                        vars_merged[k] = v

        remaining = passthrough + remaining_nor
        return found_loci, vars_merged, remaining

    
    def _update_vars(self, v: dict):
        """Updates Dafny variables with incremented counters."""
        tmp = dict()
        for key in v.keys():
            tmp.update({key: v.get(key).newBind(self.counter)})
        self.counter += 1

        return tmp

    def _update_vars_sub(self, v: dict, qs:set):
        tmp = dict()
        for key in v.keys():
            if key in qs:
                tmp.update({key: v.get(key).newBind(self.counter)})
                self.counter += 1
            else:
                tmp.update({key: v.get(key)})
        return tmp

    #create new DXBind with TyNor and TyHad type Only
    #and for TyEn, we increment the type to 1
    def _update_vars_type(self, v: dict, t: QXQTy):
        tmp = dict()
        for key in v.keys():
            self.log('upVarsType', key, v.get(key), t)
            if isinstance(t, TyNor):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("bv1")), self.counter)})
            elif isinstance(t, TyHad):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("real")), self.counter)})
            elif isinstance(t, TyEn):              
                if 'amp' not in tmp:
                    tmp['amp'] = DXBind('amp', gen_nested_seq_type(1, SType("real")), self.counter)
                    tmp.update({key: v.get(key).newBindType(gen_nested_seq_type(1, SeqType(SType("bv1"))), self.counter)})
                else:
                    tmp.update({key: v.get(key).newBindType(SeqType(v.get(key).type()), self.counter)})
                self.log('\nupVarsType en amp var: ', key, tmp.get(key))
        self.counter += 1

        return tmp

    def superLocus(self, q2: [QXQRange], ty:QXQTy):
        """
        Searches the current state (self.varnums) to find a component that overlaps
        with the target locus 'q2' and matches the target type 'ty'. 
        """
#        self.log(f"\n superLocus: searching for super locus of {q2} with type {ty} in varnums: {self.varnums}")
        vs = []
        for i in range(len(self.varnums)):
            loc, qty, vars = self.varnums[i]
            self.log(f"\n loc {loc} \n qty {qty} \n vars {vars}")
            if EqualityVisitor().visit(ty, qty):
                # Check for any overlap by seeing if any component of one locus
                # is a sub-locus of the other.
                has_overlap = any(compareLocus([l_part], q2) is not None for l_part in loc) or \
                              any(compareLocus([q2_part], loc) is not None for q2_part in q2)

                if has_overlap:
                    # We found a component. Now calculate the remainder of q2.
                    # The remainder is the parts of q2 that are NOT covered by loc.
                    rem = [r for r in q2 if compareLocus([r], loc) is None]
                    
                    # Return the remainder, the found component, and the rest of the state.
                    return rem, loc, qty, vars, (vs + self.varnums[i+1:])
            vs += [self.varnums[i]]
        return None

    def computeType(self, q2: [QXQRange]):
        """
        Calculates the "meet" or the most general type required to unify a set of
        qubit ranges 'q2'. It determines the current type of each range and then
        uses the 'meetType' helper to find their least upper bound.
        """
        tmp = None
        for elem in q2:
            loc, qty, vars = subLocus([elem], self.varnums)
            if tmp is None:
                tmp = qty
            else:
                tmp = meetType(tmp, qty)
        return tmp

    def compareSingle(
        self,
        q1: QXQRange,
        qs: list[tuple[list[QXQRange], QXQTy, dict]]
    ) -> "None | tuple[str | None, list[QXQRange], dict, list[tuple[list[QXQRange], QXQTy, dict]]]":
        """
        Searches for a separable locus `q1` in `qs`.
        Handles:
        - Exact match: returns (None, [q1], vars_map, remaining_states)
        - Left-prefix match: returns (q/q1, [q1], vars_map, remaining_states)
            where `original_var` indicates slicing is needed upstream.
        Returns None if no match is found.
        """
        for i in range(len(qs)):
            locs, qty, vars_map = qs[i]

            # We only handle a single-locus TyHad/TyNor entry as per your spec
            if len(locs) != 1 or not (isinstance(qty, TyHad) or isinstance(qty, TyNor)):
                continue

            existing = locs[0]
            if q1.location() != existing.location():
                continue

            # Equality checks you already use
            left_eq  = compareAExp(q1.crange().left(),  existing.crange().left())
            right_eq = compareAExp(q1.crange().right(), existing.crange().right())

            # Case 1: exact match -> remove this entry from qs
            if left_eq and right_eq:
                remaining_states = qs[:i] + qs[i+1:]
                # matched_locus must be a list
                return (None, [q1], vars_map, remaining_states)

            # Case 2: left-prefix match -> slice "existing" to its right remainder
            # (We assume caller ensured q1.right <= existing.right syntactically;
            if left_eq:
                # Keep the same vars_map for the remainder 
                new_left = q1.crange().right()
                remq1 = QXQRange(
                    q1.location(),
                    crange=QXCRange(new_left, existing.crange().right())
                )
                remaining_tuple = ([remq1], qty, vars_map)

                remaining_states = qs[:i] + [remaining_tuple] + qs[i+1:]
                
                return ([remq1], [q1], vars_map, remaining_states)
            if right_eq:
                new_right = q1.crange().left()
                remq1 = QXQRange(
                    q1.location(),
                    crange=QXCRange(existing.crange().left(), new_right)    
                )
                remaining_tuple = ([remq1], qty, vars_map)
                remaining_states = qs[:i] + [remaining_tuple] + qs[i+1:]

                # Signal to caller which original var to slice: use the VAR NAME (key), not value
                # Pick a stable key; if you have a canonical name, use that instead.
#                original_var = next(iter(vars_map.keys())) if vars_map else None

                return ([remq1], [q1], vars_map, remaining_states)

        # No match found
        return None


    def findHadLocus(self, q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):
        """
        Iterates through a list of required loci 'q2' and searches for the first one
        that exists as a separable (Had or Nor) state in the list of available states 'qs'.
        """
        for i in range(len(q2)):
            v = self.compareSingle(q2[i], qs)
            if v is not None:
                return v
        return None
    
    def includeIfLocus(self, q2: [QXQRange]):
        with self.trace("includeIfLocus"):
            self.log(f"\n lc locus from if: {q2}")
            self.log(f"\n self.varnum {self.varnums}")
            v = subLocus(q2, self.varnums)
            self.log(f"\n subLocus {v}")

            if v is not None:
                locs, qty, vars_map = v

                # Remove exactly one occurrence equal to v (by value) from self.varnums
                remaining = None
                for idx, s in enumerate(self.varnums):
                    if s == v:                       # value equality, not identity
                        remaining = self.varnums[:idx] + self.varnums[idx+1:]
                        break
                if remaining is None:
                    # v wasn't literally in the list (subLocus built a fresh tuple).
                    # Fall back to keeping the list as-is:
                    remaining = list(self.varnums)

                matched_locus = locs if isinstance(locs, list) else [locs]

                return [], matched_locus, qty, vars_map, remaining

            ty = self.computeType(q2)
            rt = self.superLocus(q2, ty)

            if rt is None:
                self.log("no sub/super locus match; return unchanged env")
                return [], None, None, None, list(self.varnums)
        #remainder from q2 after superLocus, super locus and remains in env
            rem, loc, qty, vars_map, env_after = rt
            self.log(f"\n rem: {rem} \n loc qty vars{loc}, \n {qty} \n {vars_map}, \n tmpVarNums {env_after}")

            # Normalize shapes
            base_loc = loc[:] if isinstance(loc, list) else [loc]
            merged_loc = base_loc[:]           # we'll build on this
            current_vars = dict(vars_map)      # copy to avoid aliasing
            stmts = []

            # 2a) If starting from Had, cast to En first
            if isinstance(ty, TyHad):
                cast_stmts, en_vars = self._gen_had_to_en_cast_stmts(current_vars, base_loc[0].line_number())
                stmts.extend(cast_stmts)
                qty = TyEn(QXNum(1))
                current_vars = en_vars

            # 2b) Merge Nor loci from remainder
            nor_locs, nor_vars, env_after = self.collectNorLocus(rem, env_after)
            for nloc in nor_locs:
                name = nloc.location()
                if name in nor_vars:
                    merged_loc = merged_loc + [nloc]        
                    current_vars[name] = nor_vars[name]

            # 2c) If we’re already En, try to merge a Had control locus
            if isinstance(ty, TyEn) and env_after:
                vb = self.findHadLocus(rem, env_after)
                self.log("findHadLocus →", vb)
                if vb is not None:
                    remainder, had_locus, had_vars, env_after = vb
                    had_var = next(iter(had_vars.values()))
                    ctrl_had = had_locus[0]
                    rem_had = remainder[0] if remainder is not None else None
                    merge_stmts, new_vars, q_leftover = self._gen_en_had_merge_stmts(
                        current_vars, had_var, ctrl_had, rem_had, base_loc[0].line_number()
                    )
                    self.log(f"\n new_vars {new_vars} \n q_leftover {q_leftover}")
                    stmts.extend(merge_stmts)
                    current_vars = new_vars
                    merged_loc = merged_loc + had_locus


                    # Replace the matching leftover in env_after with a singleton map
                    for i, (rloc, rqty, rvars) in enumerate(env_after):
                        if rloc and rloc[0].location() == q_leftover.ID():
                            env_after[i] = (rloc, rqty, {q_leftover.ID(): q_leftover})
                            break

            # 3) Return the constructed pieces; let the caller update self.varnums
            self.log("includeIfLocus → merged_loc/qty/vars/env_after:",
                    merged_loc, qty, list(current_vars.keys()), env_after)
            new_env = [(tuple(merged_loc), qty, current_vars)] + env_after
            self.varnums = new_env
            return stmts, merged_loc, qty, current_vars, env_after

    def _sequence_comprehension_transform(self, ctx, unified_locus, unified_qty, unified_vars):
        """
        Generates imperative statements using sequence comprehensions to construct
        the new state after an oracle or conditional oracle operation.
        """
        stmts = []
        new_vars = {}
        is_conditional = isinstance(ctx, Programmer.QXIf)

        if is_conditional:
            q_assign = ctx.stmts()[0]
            operation = q_assign.exp()
        else: # It's a QXQAssign
            q_assign = ctx
            operation = ctx.exp()

        is_phase_oracle = not operation.vectors() or \
            (len(operation.vectors()) == len(operation.bindings()) and \
             all(EqualityVisitor().visit(operation.vectors()[i].vector(), operation.bindings()[i]) for i in range(len(operation.vectors()))))
        
        # Determine the number of iterators needed
        nesting_depth = 0
        if isinstance(unified_qty, TyEn):
            nesting_depth = unified_qty.flag().num()
        
        iterators = [DXBind(f"k{i}", SType("nat")) for i in range(nesting_depth)]

        # Get the transformation logic for the body of the comprehension
        comprehension_logic, transformed_names = self._get_comprehension_logic(q_assign, unified_vars, iterators, unified_qty)

        # Create the sequence comprehension for the target variables
        for name in sorted(transformed_names):
            if is_phase_oracle and name != 'amp':
                continue
            var = unified_vars[name]        
            new_var = DXBind(f"{name}", var.type(), self.counter)
            
            transform_expr = comprehension_logic[name]

            # Build the comprehension body
            if is_conditional:
                control_qubit = ctx.bexp()
                control_var = unified_vars[control_qubit.location()]
                self.log(f"\n control_var: {control_var}")
                indexed_control = self._create_indexed_var(control_var, iterators)
    #            control_bit = DXIndex(indexed_control, control_qubit.crange().left().accept(self))
                condition = DXComp(">=", DXCall("castBVInt", [indexed_control]), DXCall("pow2", [control_qubit.crange().left().accept(self)]))
#                condition = DXComp("==", control_bit, DXNum(1))
                self.log(f"\n condition {condition}")
                
                else_expr = self._create_indexed_var(var, iterators)
                comprehension_body = DXIfExp(condition, transform_expr, else_expr)
                
            else: # Standard oracle
                comprehension_body = transform_expr

            # Build the nested comprehension from the inside out
            comprehension = comprehension_body
            for i in range(len(iterators) - 1, -1, -1):
                iterator = iterators[i]
                range_var = self._create_indexed_var(var, iterators[:i])
                comprehension_range = DXLength(range_var)
                spec = DXRequires(DXInRange(iterator, DXNum(0), comprehension_range))
                comprehension = DXSeqComp(comprehension_range, iterator, spec, comprehension)

            stmts.append(DXAssign([new_var], comprehension, True))
            new_vars[name] = new_var
        self.counter += 1    

        # Handle unchanged variables
        for name, var in unified_vars.items():
            if name not in new_vars:
                new_vars[name] = var
        
        if is_conditional:
            self.en_factor.push(condition)
        
        # if self.en_factor:
        #     self.en_factor = DXBin('&&', self.en_factor.conds(), condition)
        # else:
        #     if is_conditional:
        #         self.en_factor = EnFactor(condition)
        
        return stmts, new_vars
    
    def _get_comprehension_logic(self, q_assign: QXQAssign, unified_vars: dict, iterators: list, qty: QXQTy):
        
        """
        Generates a dictionary of Dafny expressions that define the transformation
        for each variable on a single basis state slice, for ORACLES ONLY.
        """
        operation = q_assign.exp()
        if not isinstance(operation, QXOracle):
            raise Exception("This helper is only for Oracles.")
        
        logic_map = {}
        is_phase_oracle = not operation.vectors() or \
            (len(operation.vectors()) == len(operation.bindings()) and \
                all(EqualityVisitor().visit(operation.vectors()[i].vector(), operation.bindings()[i]) for i in range(len(operation.vectors()))))

        if is_phase_oracle:
            transformed_names = {'amp'}
            amp_in = self._create_indexed_var(unified_vars['amp'], iterators)
            phase_expr_template = operation.phase().accept(self)
            
            final_phase_expr = phase_expr_template
            if operation.bindings():
                target_loci = q_assign.locus()
                subst_map = {}
                for i, binder in enumerate(operation.bindings()):
                    placeholder_name = binder.ID()
                    target_reg_name = target_loci[i].location()
                    subst_map[placeholder_name] = DXCall("castBVInt", [self._create_indexed_var(unified_vars[target_reg_name], iterators)])
                
                self.libFuns.add("castBVInt")
                for placeholder, subst_val in subst_map.items():
                    subst = SubstDAExp(placeholder, subst_val)
                    final_phase_expr = subst.visit(final_phase_expr)
            
            logic_map['amp'] = DXBin("*", amp_in, final_phase_expr)
        else: # Value-transforming oracle
            transformed_names = {l.location() for l in q_assign.locus()}
            
            target_loci_names = sorted(list(transformed_names))
            subst_map = {}
            for i, binder in enumerate(operation.bindings()):
                placeholder_name = binder.ID()
                target_reg_name = target_loci_names[i]
                subst_map[placeholder_name] = DXCall("castBVInt", [self._create_indexed_var(unified_vars[target_reg_name], iterators)])
            self.libFuns.add("castBVInt")

            for i, name in enumerate(target_loci_names):
                ket_expr_template = operation.vectors()[i].accept(self)
                final_ket_expr = ket_expr_template
                for placeholder, subst_val in subst_map.items():
                    subst = SubstDAExp(placeholder, subst_val)
                    final_ket_expr = subst.visit(final_ket_expr)
                
                # Convert the final nat expression back to seq<bv1>
                width_expr = DXLength(self._create_indexed_var(unified_vars[name], iterators))
                logic_map[name] = DXCall("castIntBV", [final_ket_expr, width_expr])
                self.libFuns.add("castIntBV")
            
            # if 'amp' in unified_vars:
            #     transformed_names.add('amp')
            #     logic_map['amp'] = self._create_indexed_var(unified_vars['amp'], iterators)
        
        return logic_map, transformed_names    
    
    # ==========================================================================
    # == Core Logic for ORACLE Transformation (deprecated)
    # ==========================================================================

    @DeprecationWarning
    def _create_lambda_method(self, q_assign: QXQAssign, unified_vars: dict):
        """
        Creates an axiomatic ghost method that defines the transformation
        of a single basis state, to be called from a sequence comprehension.
        """
#        self.counter += 1
        ghost_name = f"lambda_{self.counter}"
        operation = q_assign.exp()
       
        if not isinstance(operation, QXOracle):
            raise Exception("Lambda method generation is only supported for Oracles.")
        
        is_phase_oracle = not operation.vectors() or \
            (len(operation.vectors()) == len(operation.bindings()) and \
             all(EqualityVisitor().visit(operation.vectors()[i].vector(), operation.bindings()[i]) for i in range(len(operation.vectors()))))

        target_names = {l.location() for l in q_assign.locus()}
        
        # Define arguments and returns
        args = []
        if is_phase_oracle: # Phase Oracle
            target_names.add('amp')
            args += [DXBind(f"{name}", self._get_element_type(unified_vars[name].type())) for name in sorted(target_names)]
#            args += [DXBind(f"{name}", unified_vars[name].type()) for name in sorted(target_names) if name != 'amp']
            returns = [DXBind(f"{name}_out", self._get_element_type(unified_vars[name].type())) for name in sorted(target_names) if name == 'amp']
        else:
            args += [DXBind(f"{name}", self._get_element_type(unified_vars[name].type())) for name in sorted(target_names)]
            returns = [DXBind(f"{name}_out", self._get_element_type(unified_vars[name].type())) for name in sorted(target_names)]
#       self.log(f"\n args: {args} \n{returns}")
        
        # if len(returns) > 1:
        #     returns = [DXTup(returns)]

        # Input variables: map each name to the DXBind in args with matching ID
    #    lambda_input_vars = {name: next(arg for arg in args if arg.ID() == name) for name in sorted(target_names)}
        lambda_input_vars = {name: ret for name in sorted(target_names) for ret in args if ret.ID() == f"{name}"}

        # Output variables: map each name to the DXBind in returns with matching ID (removing '_out' suffix)
        lambda_output_vars = {name: ret for name in sorted(target_names) for ret in returns if ret.ID() == f"{name}_out"}
        

        ensures = []

        # Generate ensures for phase oracles
        if is_phase_oracle:
            for name, out_var in lambda_output_vars.items():
                in_var = lambda_input_vars[name]
                if name == 'amp':
                    phase_expr = operation.phase().accept(self)
                    # Substitute placeholders in the phase expression
                    if operation.bindings():
                        x = operation.bindings()[0]
                        # Find the corresponding qubit register 
                        q_in_var = lambda_input_vars[q_assign.location()[0].location()]
                        subst = SubstDAExp(x.ID(), DXCall("castBVInt", [q_in_var]))
                        self.libFuns.add("castBVInt")
                        phase_expr = subst.visit(phase_expr)
                    ensures.append(DXEnsures(DXComp("==", out_var, DXBin("*", in_var, phase_expr))))
                # else: # Qubit registers are unchanged
                #     ensures.append(DXEnsures(DXComp("==", in_var, in_var)))
       
        # Generate ensures for value-transforming oracles
        else:
            ket_idx = 0
            for name, out_var in sorted(lambda_output_vars.items()):
                in_var = lambda_input_vars[name]
                if name == 'amp': 
                    ensures.append(DXEnsures(DXComp("==", out_var, in_var))) # Amp is unchanged
                else:
                    # Add length preservation ensures
                    ensures.append(DXEnsures(DXComp("==", DXLength(out_var), DXLength(in_var))))
                    
                    ket_expr = operation.vectors()[ket_idx].accept(self)
                    # Substitute placeholders in the ket expression
                    for binder, in_name in zip(operation.bindings(), sorted(target_names)):
                        if in_name in lambda_input_vars:
                            replace = DXCall("castBVInt", [lambda_input_vars[in_name]])
                            subst = SubstDAExp(binder.ID(), replace)
                            ket_expr = subst.visit(ket_expr)
    #                self.log(f"\n ket_expr: {ket_expr}")
                    ensures.append(DXEnsures(DXComp("==", DXCall("castBVInt", [out_var]), ket_expr)))
                    self.libFuns.add("castBVInt")
                    ket_idx += 1


        # Add requires clauses for classical arguments
        args += self.classical_args
        reqs = []
        classical_arg_ids = {arg.ID() for arg in self.classical_args}
        for req in self.final_requires:
            collector = BindCollector()
            collector.visit(req.spec())
            if collector.renv and collector.renv.issubset(classical_arg_ids):
                reqs.append(req)
        
        return DXMethod(ghost_name, True, args, returns, reqs + ensures, [], is_function=True)
    

    @DeprecationWarning
    def _get_transformation_logic(self, oracle, old_vars, iterators, target_loc_pos):
        """
        Helper to generate the right-hand side of the transformation equalities for an oracle.
        Returns a dictionary mapping variable names to their transformed expressions.
        This method dispatches to specialized helpers based on the oracle type.
        """
        is_phase_oracle = not oracle.vectors() or \
            (len(oracle.vectors()) == len(oracle.bindings()) and \
             all(EqualityVisitor().visit(oracle.vectors()[i].vector(), oracle.bindings()[i]) for i in range(len(oracle.vectors()))))

        if is_phase_oracle:
            return self._get_phase_oracle_logic(oracle, old_vars, iterators)
        else:
            return self._get_value_oracle_logic(oracle, old_vars, iterators, target_loc_pos)
    @DeprecationWarning
    def _get_phase_oracle_logic(self, oracle, old_vars, iterators):
        """Generates the transformation logic for a phase-modifying oracle."""
        logic_map = {}
        qubit_vars = {k: v for k, v in old_vars.items() if k != 'amp'}
        
        # For qubit registers, the transformed expression is just the old value.
        for name, old_q_var in qubit_vars.items():
            logic_map[name] = self._create_indexed_var(old_q_var, iterators)
        
        # For amplitude, the transformed expression is old_amp * phase.
        if 'amp' in old_vars:
            old_amp_var = old_vars['amp']
            indexed_old_amp = self._create_indexed_var(old_amp_var, iterators)
            phase_template = oracle.phase().accept(self)
            final_phase_expr = phase_template
            if oracle.bindings():
                placeholder_id = oracle.bindings()[0].ID()
                old_qubit_var = next(iter(qubit_vars.values()))
                indexed_old_qubit = self._create_indexed_var(old_qubit_var, iterators)
                subst_val = DXCall("castBVInt", [indexed_old_qubit])
                self.libFuns.add('castBVInt')
                subst = SubstDAExp(placeholder_id, subst_val)
                final_phase_expr = subst.visit(phase_template)
            logic_map['amp'] = DXBin("*", indexed_old_amp, final_phase_expr)
        return logic_map
    @DeprecationWarning
    def _get_value_oracle_logic(self, oracle, old_vars, iterators, target_loc_pos):
        """Generates the transformation logic for a value-modifying oracle."""
        logic_map = {}
        ket_idx = 0
        for var_name, old_bind in sorted(old_vars.items()):
            if var_name == 'amp':
                logic_map['amp'] = self._create_indexed_var(old_bind, iterators)
            else:
                ket_expr_qafny = oracle.vectors()[ket_idx]
                dx_oracle_expr = ket_expr_qafny.accept(self)
                final_expr = dx_oracle_expr
                if not EqualityVisitor().visit(ket_expr_qafny.vector(), oracle.bindings()[ket_idx]):
                    for j, binder in enumerate(oracle.bindings()):
                        if j < len(iterators):
                            subst = SubstDAExp(binder.ID(), iterators[target_loc_pos[j]])
                            final_expr = subst.visit(final_expr)
                logic_map[var_name] = final_expr
                ket_idx += 1
        return logic_map
    
    # ==========================================================================
    # == Helper Methods for Single Operations
    # ==========================================================================


    def _direct_call_transform(self, ctx: QXQAssign, loc, qty, old_vars):
        """
        Generates a direct call to a library function for a single-register operation
        and determines the resulting quantum type.
        """
        stmts = []
        
        op = ctx.exp().op()

        if op == "H":
            if isinstance(qty, TyNor):
                lib_func_name = "hadNorHad"
                new_qty = TyHad()
                
            elif isinstance(qty, TyHad):
                lib_func_name = "hadHad"
                new_qty = TyNor()
            elif isinstance(qty, TyEn):
                lib_func_name = "hadEn"
                new_qty = qty
            else:
                raise Exception(f"Hadamard operation not supported on type {type(qty)}")
        #TODO include other operations
        else:
            raise Exception(f"Single gate operation '{op}' not supported.")
        
        new_vars = self._update_vars_type(old_vars, new_qty)    
        self.libFuns.add(lib_func_name)
        
        sorted_old_vars = sorted(old_vars.values(), key=lambda v: v.ID())
        sorted_new_vars = sorted(new_vars.values(), key=lambda v: v.ID())
        
        call = DXCall(lib_func_name, sorted_old_vars, line=ctx.line_number())
        
        stmts.append(DXAssign(sorted_new_vars, call, True, line=ctx.line_number()))
        
        return stmts, new_qty, new_vars
    
    def _hadamard_on_entangled_transform(self, ctx: QXQAssign, loc, qty, old_vars):
        """
        Handles H on an entangled state by generating imperative sequence comprehensions
        that directly construct the new, transformed state, based on the user's logic.
        """
        stmts = []
        
        # 1. Define the new, deeper quantum state
        input_depth = qty.flag().num()
        output_depth = input_depth + 1
        new_qty = TyEn(QXNum(output_depth))
        new_vars = make_dafny_vars_for_locus(loc, new_qty, self.counter)
        self.counter += 1

        # 2. Setup: Iterators and variable names
        target_name = ctx.locus()[0].location()
        reg_names = [l.location() for l in loc if l.location() != 'amp']
        target_pos = reg_names.index(target_name)
        other_names = [name for name in reg_names if name != target_name]

        # Correctly create and partition the iterators with sequential names
        all_iterators_new = [DXBind(f"k{i}", SType("nat")) for i in range(output_depth)]
        iterator_new = all_iterators_new[target_pos]
        iterators_old = [it for it in all_iterators_new if it != iterator_new]

        target_locus_range = ctx.locus()[0].crange()
        right = self.visit(target_locus_range.right())
        left = self.visit(target_locus_range.left())
        if isinstance(left, DXNum) and left.val() == 0: 
            size_of_operated_reg = right
        else:
            size_of_operated_reg = DXBin("-", self.visit(target_locus_range.right()), self.visit(target_locus_range.left()))
        op = ctx.exp()
        # 3. Build comprehensions for each new variable
        for name, new_var in new_vars.items():
            old_var = old_vars.get(name)
            
            # Build the expression inside the comprehension from the inside out.
            comprehension_body = self._build_hadamard_comprehension_body(name, old_vars, target_name, size_of_operated_reg, iterators_old, iterator_new, op)
            
            # Wrap in nested comprehensions
            comprehension = comprehension_body
            rep_old_var = next(iter(old_vars.values()))
            for i in range(len(all_iterators_new) - 1, -1, -1):
                iterator = all_iterators_new[i]
                
                outer_iterators = all_iterators_new[:i]
                temp_old_iters = [it for it in outer_iterators if it in iterators_old]
                
                if iterator == iterator_new:
                    # The range for the new dimension is based on the size of the target locus from the AST.
                    comp_range = DXCall("pow2", [size_of_operated_reg])
                else:
                    range_var = self._create_indexed_var(rep_old_var, temp_old_iters)
                    comp_range = DXLength(range_var)

                spec = DXRequires(DXInRange(iterator, DXNum(0), comp_range))
                comprehension = DXSeqComp(comp_range, iterator, spec, comprehension)

            stmts.append(DXAssign([new_var], comprehension, True))
            
            if name == target_name:
                assertion_body = DXComp("==", self._create_indexed_var(new_var, all_iterators_new), comprehension_body)
                wrapped_assertion = self._wrap_in_forall(assertion_body, all_iterators_new, new_var)
                stmts.append(DXAssert(wrapped_assertion, line=ctx.line_number()))

        # 4. Add helper lemma calls
        stmts.append(DXCall("triggerSqrtMul", [], True, line=ctx.line_number()))
        stmts.append(DXCall("ampeqtrigger", [], True, line=ctx.line_number()))
    #    stmts.append(DXCall("pow2mul", [], True, line=ctx.line_number()))
    #    stmts.append(DXCall("pow2sqrt", [], True, line=ctx.line_number()))
        self.libFuns.update(["triggerSqrtMul", "ampeqtrigger"])
        
        return stmts, new_qty, new_vars
    
    def _controlled_single_gate_transform(self, ctx: Programmer.QXIf, loc, qty, old_vars):
        """
        Handles a controlled H on an entangled state. This is a structural transformation
        that increases the nesting depth of the state.
        """
        stmts = []
        
        # 1. Define the new, deeper quantum state
        input_depth = qty.flag().num()
        output_depth = input_depth + 1
        new_qty = TyEn(QXNum(output_depth))
        
        new_vars = make_dafny_vars_for_locus(loc, new_qty, self.counter)
        self.counter += 1

        # 2. Setup: Iterators and variable names
        q_assign = ctx.stmts()[0]
        target_name = q_assign.locus()[0].location()
        op = q_assign.exp()
        
        iterators_new = [DXBind(f"k{i}", SType("nat")) for i in range(output_depth)]
        
        iterators_old = iterators_new[:-1]
        iterator_new_dim = iterators_new[-1]
        self.log(f"\n iterator_old, {iterators_old}, \n new_dim {iterator_new_dim}")

        target_locus_range = q_assign.locus()[0].crange()
        right = self.visit(target_locus_range.right())
        left = self.visit(target_locus_range.left())
        if isinstance(left, DXNum) and left.val() == 0: 
            size_of_operated_reg = right
        else:
            size_of_operated_reg = DXBin("-", self.visit(target_locus_range.right()), self.visit(target_locus_range.left()))
        
        # 3. Build comprehensions for each new variable
        rep_old_var = next(iter(old_vars.values()))
        self.log(f"\n rep_old_var {rep_old_var}")
        for name, new_var in new_vars.items():
            old_var = old_vars.get(name)

            # --- Build the 'then' and 'else' branches for the innermost expression ---            
            # 'Then' branch expression
            then_expr = self._build_hadamard_comprehension_body(name, old_vars, target_name, size_of_operated_reg, iterators_old, iterator_new_dim, op)
    #        self.log(f"\n self._get_total_nesting_depth(old_var.type()): {old_var}: {self._get_total_nesting_depth(old_var.type())}")
            if self._get_total_nesting_depth(old_var.type()) == 0:
                else_expr = old_var
            else:
                else_expr = self._create_indexed_var(old_var, iterators_old)
            
            # Build the conditional expression for the innermost element
            control_qubit = ctx.bexp()
            if isinstance(control_qubit, QXQRange):
                control_var = old_vars[control_qubit.location()]
                indexed_control = self._create_indexed_var(control_var, iterators_old)
    #            self.log(f"\n indexed_control {indexed_control}")
    #        control_bit = DXIndex(indexed_control, control_qubit.crange().left().accept(self))
                condition = DXComp(">=", DXCall("castBVInt", [indexed_control]), DXCall("pow2", [control_qubit.crange().left().accept(self)]))
            elif isinstance(control_qubit, QXQComp):
                control_var = old_vars[control_qubit.left().location()]
                indexed_control = self._create_indexed_var(control_var, iterators_old)
                condition = DXComp(control_qubit.op(), DXCall("castBVInt", [indexed_control]), control_qubit.right().accept(self))
                if name == control_qubit.index().ID():
                    then_expr = DXCall("castIntBV", [DXNum(1), DXLength(old_var)])
                    self.libFuns.add("castIntBV")
            self.libFuns.add("castBVInt")
#            factor = EnFactor(condition)

            self.log(f"\n condition {condition}")
            
            
            # The body of the innermost comprehension is the conditional
            inner_comprehension_body = DXIfExp(condition, then_expr, else_expr)

            # --- Wrap in nested comprehensions ---
            comprehension = inner_comprehension_body
            for i in range(len(iterators_new) - 1, -1, -1):
                iterator = iterators_new[i]
                
                # The range of the new dimension is different from the old dimensions
                if iterator == iterator_new_dim:
                    target_var = old_vars[target_name]
                    if self._get_total_nesting_depth(target_var.type()) == 0:
                        range_var = target_var
                    else:
                        range_var = self._create_indexed_var(old_vars[target_name], iterators_old)
                    comp_range = DXCall("pow2", [DXLength(range_var)])
                else:
                    # The iterators for the outer dimensions need to be mapped back to the old state
                    outer_iterators_for_old = [it for it in iterators_new[:i] if it in iterators_old]
                    range_var = self._create_indexed_var(rep_old_var, outer_iterators_for_old)
                    comp_range = DXLength(range_var)

                spec = DXRequires(DXInRange(iterator, DXNum(0), comp_range))
                comprehension = DXSeqComp(comp_range, iterator, spec, comprehension)

            stmts.append(DXAssign([new_var], comprehension, True))
        
        self.en_factor.push(condition)
            
        return stmts, new_qty, new_vars
    
    def _build_hadamard_comprehension_body(self, name, old_vars, target_name, size_of_operated_reg, iterators_old, iterator_new, op):
        """
        Helper to build the innermost expression for the Hadamard transformation comprehension.
        """
        old_var = old_vars.get(name)
        target_var = old_vars.get(target_name)
        self.log(f"\n old_var:{old_var} \n target_var: {target_var}")
    #    self.log(f"\n self._get_total_nesting_depth(target_var) {self._get_total_nesting_depth(target_var.type()) }")
        
        if name == 'amp':
            old_amp_slice = self._create_indexed_var(old_var, iterators_old)
            
            pow2_size = DXCall("pow2", [size_of_operated_reg])
            self.libFuns.add("pow2")
            scaling_factor = DXBin("/", DXNum(1.0), DXCall("sqrt", [DXCast(SType("real"), pow2_size)]))
            self.libFuns.add("sqrt")

            
            if self._get_total_nesting_depth(target_var.type()) == 0:
                return DXBin('*', old_amp_slice, scaling_factor)
            else:
                target_reg_val = DXCall("castBVInt", [self._create_indexed_var(old_vars[target_name], iterators_old)])
                phase_term = DXBin("*", iterator_new, target_reg_val)
                self.libFuns.add("castBVInt")
                
                if op.op() == "H":
                    rou_pow = DXNum(2)
                elif op.op() == "QFT":
                    rou_pow = DXCall("pow2", [size_of_operated_reg])
                phase = DXCall("omega", [phase_term, rou_pow])
                self.libFuns.add("omega")

                return DXBin("*", old_amp_slice, DXBin("*", scaling_factor, phase))
            
        elif name == target_name:
            # The TARGET register's value becomes the new iterator's value.
            self.libFuns.add("castIntBV")
            return DXCall("castIntBV", [iterator_new, size_of_operated_reg])

        else: # Non-target registers
            # The NON-TARGET register's value is preserved from the outer loop.
            return self._create_indexed_var(old_var, iterators_old)
    
    # ==========================================================================
    # == Helper Methods for Predicate Generation
    # ==========================================================================
    @staticmethod
    def _get_total_nesting_depth(var_type: DXType):
        """Calculates the total number of sequence levels until seq<bv1> and real."""
        dims = 0
        tmp = var_type

        while isinstance(tmp, SeqType):
            dims += 1
            tmp = tmp.type()  
        if isinstance(tmp, SType) and tmp.type() == "bv1":
            return max(dims - 1, 0)
        else:
            return dims
    
    @staticmethod
    def _get_element_type(seq_type):
        """Returns the outermost SeqType wrapping bv1, or SType('real') if innermost is real."""
        tmp = seq_type
        prev = None
        while isinstance(tmp, SeqType):
            prev = tmp
            tmp = tmp.type()
        if isinstance(tmp, SType) and tmp.type() == "bv1":
            return prev  # outermost SeqType wrapping bv1
        elif isinstance(tmp, SType) and tmp.type() == "real":
            return tmp   # SType("real") object
        else:
            return tmp

    @staticmethod
    def _create_indexed_var(base_var, iterators):
        """Creates a nested DXIndex expression, e.g., var[k0][k1]."""
        indexed_var = base_var
        for it in iterators:
            indexed_var = DXIndex(indexed_var, it)
        return indexed_var

    def _wrap_in_forall(self, body, iterators, representative_var):
        """Wraps a logical body in nested forall quantifiers with range checks."""
        nested_forall = body
        for k in range(len(iterators) - 1, -1, -1):
            iterator = iterators[k]
            
            # Build the indexed expression for the range limit (e.g., rep_var[k0])
            indexed_for_range = self._create_indexed_var(representative_var, iterators[:k])
            
            range_check = DXInRange(iterator, DXNum(0), DXLength(indexed_for_range), line=self.current_line)
            nested_forall = DXAll(iterator, DXLogic("==>", range_check, nested_forall, line=self.current_line), line=self.current_line)
        return nested_forall