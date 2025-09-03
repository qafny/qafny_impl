import copy
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

class StackFactor:
    pass

class EnFactor(StackFactor):
    def __init__(self, condition: (str, DXAExp, DXAExp)):
        self._condition = condition

    def condition(self):
        return self._condition


# ==============================================================================
# == Helper Functions for Locus and Type Manipulation
# ==============================================================================
def eqQRange(q1: QXQRange, q2: QXQRange):
    """Checks for exact equality between two quantum ranges."""
    return (str(q1.location()) == str(q2.location()) and compareAExp(q1.crange().left(),q2.crange().left())
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

def compareSingle(q1: QXQRange, qs: [([QXQRange], QXQTy, dict)]):

    vs = []
    for i in range(len(qs)):
        loc, qty, vars = qs[i]
        v = loc[0]
        if q1.location() == v.location():
            if compareAExp(q1.crange().left(), v.crange().left()):
                if compareAExp(q1.crange().right(), v.crange().right()): #(exactmatch, vs, [])
                    qv = []
                    vs += (qs[i+1:len(qs)])
                    return (q1, vars, vs)
                else: #(matched_qxrange, vs, [remaining_qxrange])
                    qv = [QXQRange(q1.location(),crange=QXCRange(q1.crange().right(), v.crange().right()))] 
                    vs += (qs[i+1:len(qs)])
                    return (q1, vars, qv + vs)
        vs += [v]

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
    if isinstance(t1, TyNor) and not isinstance(t2, TyHad):
        return t2
    if isinstance(t2, TyNor) and not isinstance(t1, TyHad):
        return t1
    if isinstance(t1, TyHad) and not isinstance(t2, TyNor):
        return t2
    if isinstance(t2, TyHad) and not isinstance(t1, TyNor):
        return t1
    if isinstance(t1, TyEn) and isinstance(t2, TyEn):
        if t1.flag() < t2.flag():
            return t2
        else:
            return t1
    if isinstance(t1, TyEn) and isinstance(t2, TyAA):
        if t1.flag() < t2.flag():
            return t2
        else:
            return TyAA(t1.flag(), t2.qrange(), t2.line_number())
    if isinstance(t1, TyAA) and isinstance(t2, TyEn):
        if t1.flag() < t2.flag():
            return TyAA(t2.flag(), t1.qrange(), t1.line_number())
        else:
            return t1
    else:
        if t1.flag() < t2.flag():
            return t2
        else:
            return t1
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
def findHadLocus(q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):

    for i in range(len(q2)):
        v = compareSingle(q2[i], qs)
        if v is not None:
            return v

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

    def __init__(self, kenv: dict, tenv: dict):
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
        
        # Utility state
        self.counter = 0             # Global counter for unique variable names
        self.libFuns = set()         # Tracks required Dafny library functions
        self.addFuns = []          # List of generated ghost methods to add to the program
        self.current_line = 0        # Tracks the line number for generated AST nodes
        self.t_ensures = False  # Flag to indicate if we are in a postcondition context
        self.classical_args = [] # To store classical arguments of the current method

    # ==========================================================================
    # == Core Visitor Methods (Program and Method Orchestration)
    # ==========================================================================
    def visitProgram(self, ctx: Programmer.QXProgram):
        """Translates the entire Qafny program."""
        methods = [elem.accept(self) for elem in ctx.topLevelStmts()]
        # The main program now includes the main methods and any generated ghost methods.
#        print(f"\nGenerated library functions: {self.addFuns}")
        return DXProgram(methods, line=ctx.line_number())

    def visitMethod(self, ctx: Programmer.QXMethod):
        """
        Translates a single Qafny method into a Dafny method.
        This is the main orchestrator for the predicate transformation process.
        """
        """Translates a single Qafny method into a Dafny method."""
        # --- Initialize State ---
        self.fvar = str(ctx.ID())
        self.current_line = ctx.line_number()
        self.varnums = []
        tenvp = self.tenv.get(self.fvar)[0]
        for locus, q_type in tenvp:     
            var_map = make_dafny_vars_for_locus(locus, q_type, self.counter)
            self.counter += 1
            self.varnums.append((locus, q_type, var_map))

        # --- Translate Arguments ---
        self.classical_args = [b.accept(self) for b in ctx.bindings() if b.accept(self) is not None]
        dafny_args = self.classical_args[:]
#        print(f"\n dafny_args before varnums: {dafny_args}")
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
#        print(f"\n final_requires: {final_requires}")
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
            final_assignments, _ = self._get_final_assignments_and_mapping()
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
#        print(f"\n specs: {all_conditions}")
        
        return DXMethod(self.fvar, ctx.axiom(), dafny_args, dafny_returns, all_conditions, dafny_body, False, line=ctx.line_number())

    # ==========================================================================
    # == Statement Visitors (Predicate Transformers)
    # ==========================================================================
    def visitQAssign(self, ctx: QXQAssign):
        """Transforms the state for a quantum assignment (Oracle or Single gate)."""
        loc_tuple = subLocus(ctx.locus(), self.varnums)
        if not loc_tuple:
            raise Exception("Locus for operation not found in current state.")
        
        loc, qty, old_vars = loc_tuple
        
        # Dispatch to the correct transformation logic
        if isinstance(ctx.exp(), QXOracle):
            stmts, final_vars = self._sequence_comprehension_transform(ctx, loc, qty, old_vars)
        elif isinstance(ctx.exp(), QXSingle):
            pass
        else:
            return [] # Unsupported operation type

        # Update the main state
        self._update_state_for_locus(loc, qty, final_vars)
        return stmts

    def visitOracle(self, ctx: QXOracle):
        bindings=[]
        for binding in ctx.bindings():
            bindings.append(binding.accept(self))
        ket_vectors=[]
        for arg in ctx.vectors():
            ket_vectors.append(arg.accept(self))
        return bindings, ket_vectors
    
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
        lc = LocusCollector()
        lc.visit(ctx.bexp())
        for stmt in ctx.stmts():
            lc.visit(stmt)
        
        # Unify the state, generating statements for the unification process.
        unification_stmts, unified_locus, unified_qty, unified_vars, remaining_vars = self.includeIfLocus(lc.renv)

        print(f"\n unification_stmts: {unification_stmts}, \n unified_locus: {unified_locus}, \
              \n unified_qty: {unified_qty}, \n unified_vars: {unified_vars}, \n remaining_vars: {remaining_vars}")
        
        # Generate the imperative transformation loop
        comprehension_stmts, final_vars = self._sequence_comprehension_transform(ctx, unified_locus, unified_qty, unified_vars)

        lemma_stmts = []

        if unification_stmts:
            lemma_map = {
                "powN": "powNTimesMod",
                "sqrt": "triggerSqrtMul",
                "mergeAmpEn": "pow2mul"
            }
            for lib_func, lemma_name in lemma_map.items():
                if lib_func in self.libFuns:
                    lemma_stmts.append(DXCall(lemma_name, [], True, line=self.current_line))
                    self.libFuns.add(lemma_name)
        
        # Update the main state
        self.varnums = [(unified_locus, unified_qty, final_vars)] + remaining_vars

        #handle the rest of the stmts
        
        return unification_stmts + comprehension_stmts + lemma_stmts

    # ==========================================================================
    # == Helper Methods for State Management
    # ==========================================================================

    def _get_final_assignments_and_mapping(self):
        """
        Generates assignments from the final state variables to the return variables
        and returns the mapping between them for predicate substitution. This version
        is driven by the method's public return signature.
        """
        assignments = []
        mapping = {} # Maps final state var ID to return var object
        
        for locus, return_qty, return_vars_map in self.outvarnums:
            # Find the corresponding final state in the running environment
            final_state_tuple = subLocus(locus, self.varnums)
     #       print(f"\nMapping for locus {locus}: found final state tuple {final_state_tuple}")
            
            if final_state_tuple:
                _, final_qty, final_vars_map = final_state_tuple
#                print(f"\nFinal qty: {final_qty}, Return qty: {return_qty}")
                
                # Check if the types match before creating the assignment
                if compareType(final_qty, return_qty):
                    final_vars = sorted(final_vars_map.values(), key=lambda v: v.ID())
                    return_vars = sorted(return_vars_map.values(), key=lambda v: v.ID())
#                    print(f"\nFinal vars: {final_vars}, Return vars: {return_vars}")

                    if final_vars and return_vars:
                        assignments.append(DXAssign(return_vars, final_vars, line=self.current_line))
                        for i in range(len(final_vars)):
                            mapping[final_vars[i].ID()] = return_vars[i]
#        print(f"\nFinal assignments: {assignments}")                    
        return assignments, mapping


    def _update_state_for_locus(self, target_locus: list, new_qty: QXQTy, new_vars: dict):
        """Updates self.varnums by replacing an old locus with its new version."""
        new_varnums = []
        found = False
        for loc, _, _ in self.varnums:
            # Use compareLocus to check for an exact match
            if compareLocus(target_locus, loc) == []:
                if not found:
                    new_varnums.append((target_locus, new_qty, new_vars))
                    found = True
            else:
                new_varnums.append((loc, _, _))
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
        res = subLocus(ctx.locus(), env)
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
                    len_pred = DXComp(">", qcount, DXNum(0), line=ctx.line_number())
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

        # Value predicates
        var_ket = {loc[i].location(): ket for i, ket in enumerate(ctx.kets())}   
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

        # Amplitude predicate
        ampvar = varbind.get('amp')
        if ampvar:
            amp_val = ctx.amp().accept(self)
            idx_amp = makeIndex(ampvar, sum_vars)
            body = DXComp("==", idx_amp, amp_val, line=ctx.line_number())
            tmp.append(_build_sum_forall(body, [DXBind(sv.ID(), SType("nat"), line=ctx.line_number()) for sv in sum_vars]))
        
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
            predicates.append(DXComp(">", right, left, line=ctx.line_number()))
                
        # --- Value Predicate ---
        ket_val = ctx.kets()[0].accept(self)
        
        # The range for the forall loop, e.g., 0 <= i < |dvar|
        drange = DXInRange(iterator, DXNum(0), DXLength(dvar), line=ctx.line_number())
        body = DXComp("==", DXIndex(dvar, iterator), ket_val, line=ctx.line_number())
        predicates.append(DXAll(iterator, DXLogic("==>", drange, body), line=ctx.line_number()))    
        
        return predicates

    def visitBin(self, ctx: Programmer.QXBin):
        left = ctx.left().accept(self)
        right = ctx.right().accept(self)
#        print(f"\nVisiting bin: {ctx} with left {left} and right {right}")
        if ctx.op() == '^':
            if isinstance(left, DXNum) and left.val() == 2:
                self.libFuns.add('pow2')
                return DXCall('pow2',[right], line=ctx.line_number())
            else:
                self.libFuns.add('powN')
                return DXCall('powN',[left, right], line=ctx.line_number())

        if ctx.op() == '/' and isinstance(left, DXNum) and left.val() == 1.0:
#            print('\ntransfer', DXBin(ctx.op(),  DXNum(1), right, line=ctx.line_number()))
            return DXBin(ctx.op(), DXNum(1.0), right, line=ctx.line_number())

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
    #    print('\nvisitCall', ctx.ID())
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
        
    # ==========================================================================
    # == Core Logic for Conditional Transformation
    # ==========================================================================
    def _gen_had_to_en_cast_stmts(self, vars_map: dict, line_number: int):
        """
        Generates imperative statements to "promote" a TyHad state to a TyEn state.
        Returns the new variables and the list of statements.
        """
        stmts = []
        new_vars = self._upVars(vars_map)
        
        # Assuming hadEn returns (amp, q)
        new_amp_var = new_vars.get('amp')
        # Correctly get a representative qubit variable without assuming its name
        qubit_var_name = next(k for k in new_vars.keys() if k != 'amp')
        new_q_var = new_vars[qubit_var_name]
        
        if not new_amp_var: # Need to create one
            self.counter += 1
            new_amp_var = DXBind("amp", gen_nested_seq_type(1, SType("real")), self.counter)
            new_vars['amp'] = new_amp_var

        call = DXCall("hadEn", list(vars_map.values()), line=line_number)
        # Use DXAssign with init=True to generate 'var ... :='
        stmts.append(DXAssign([new_amp_var, new_q_var], call, True, line=line_number))
        self.libFuns.add('hadEn')
        
        return stmts, new_vars

    def _gen_en_nor_extend_stmts(self, en_vars: dict, nor_var: DXBind, line_number: int):
        """
        Generates imperative statements to merge a TyNor state into an existing TyEn state.
        """
        stmts = []
        new_vars = self._upVars(en_vars)
        
        for name, var in en_vars.items():
            call = DXCall('mergeBitEn', [var, nor_var], line=line_number)
            stmts.append(DXAssign([new_vars[name]], call, True, line=line_number))
        
        self.libFuns.add('mergeBitEn')
        return stmts, new_vars


    def _gen_en_had_merge_stmts(self, en_vars: dict, had_var: DXBind, ctrl_loc: QXQRange, line_number: int):
        """
        Generates imperative statements to merge a TyHad state (had_var) with a
        TyEn state (en_vars).
        """

        print(f"\n en_vars: {en_vars} \n had_var: {had_var}")
        stmts = []
        new_vars = {}
        
        # 1. Split the Had variable into its leftover part and the control bit
        
        org_name = had_var.ID()
        q_leftover = DXBind(org_name, had_var.type(), self.counter)
        stmts.append(DXAssign([q_leftover], DXCall("cutHad", [had_var]), True, line=line_number))
        self.libFuns.add("cutHad")
        self.counter += 1

        q_control_bit = DXBind(org_name, had_var.type().type(), self.counter)
        stmts.append(DXAssign([q_control_bit], DXIndex(had_var, DXNum(0)), True, line=line_number))
        self.counter += 1
        
        # 2. Merge each component of the En state
        self.libFuns.add('mergeBitEn')
        self.libFuns.add('duplicateMergeBitEn')
        self.libFuns.add('mergeAmpEn')
        self.libFuns.add('triggerSqrtMul')

        ctrl_reg_name = ctrl_loc.location()
        ctrl_idx = ctrl_loc.crange().left().accept(self)
        original_q_var = None
        
        for name, var in en_vars.items():
            new_var_bind = DXBind(name, var.type(), self.counter) # Simplified type
            
            if name == 'amp':               
                call = DXCall('mergeAmpEn', [var, q_control_bit], line=line_number)
            elif name == ctrl_reg_name: 
                original_q_var = var             
                call = DXCall('mergeBitEn', [var, ctrl_idx], line=line_number)
            else:
                call = DXCall('duplicateMergeBitEn', [var], line=line_number)             
            stmts.append(DXAssign([new_var_bind], call, True, line=line_number))
            new_vars[name] = new_var_bind
        self.counter += 1

        # 3. Add calls to helper lemmas
        stmts.append(DXCall("omega0", [], True, line=line_number))
        self.libFuns.add("omega0")
        if original_q_var:
             stmts.append(DXCall("mergeBitTrigger", [original_q_var, new_vars[ctrl_reg_name], DXLength(DXIndex(original_q_var, DXNum(0)))], True, line=line_number))
             stmts.append(DXCall('pow2add', [], True, line=line_number))
             self.libFuns.add("mergeBitTrigger")
             self.libFuns.add('pow2add')

        print(f"\n stmts: {stmts} \n new_vars: {new_vars} \n q_leftover: {q_leftover}")
            
        return stmts, new_vars, q_leftover


    def collectNorLocus(self, q2: [QXQRange], qs: [([QXQRange], QXQTy, dict)]):
        """
        A helper function that finds and collects all TyNor typed loci from a list of states 'qs'
        that are specified in the target range 'q2'. It returns the collected loci, their
        variables, and the remaining states.
        """
        qv = []
        vs = []
        for elem in q2:
            for i in range(len(qs)):
                loc, qty, vars = qs[i]
                if isinstance(qty, TyNor):
                    if eqQRange(elem, loc[0]):
                        vs += [(loc, qty, vars)]
                    else:
                        reLoc = compareRangeLocus(elem, loc)
                        qv += [(reLoc, qty, vars)]

                        vs += [(elem, TyNor, self._update_vars(vars))]
                else:
                    qv += [(loc, qty, vars)]

        newVars = dict()
        va = []
        for loc, qty, vars in vs:
            va += loc
            newVars.update({k:v for k,v in vars.items()})

        return va, newVars, qv
    
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
            print('upVarsType', key, v.get(key), t)
            if isinstance(t, TyNor):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("bv1")), self.counter)})
            elif isinstance(t, TyHad):
                tmp.update({key: v.get(key).newBindType(SeqType(SType("real")), self.counter)})
            elif isinstance(t, TyEn):              
                if not 'amp' in tmp:
                    tmp['amp'] = DXBind('amp', gen_nested_seq_type(1, SType("real")), self.counter)
                    tmp.update({key: v.get(key).newBindType(gen_nested_seq_type(1, SeqType(SType("bv1"))), self.counter)})
                else:
                    tmp.update({key: v.get(key).newBindType(SeqType(v.get(key).type()), self.counter)})
            print('\nupVarsType en amp var', key, tmp.get(key))
        self.counter += 1

        return tmp

    def superLocus(self, q2: [QXQRange], ty:QXQTy):
        """
        Searches the current state (self.varnums) to find a component that overlaps
        with the target locus 'q2' and matches the target type 'ty'. This is more
        robust than a simple sub-locus check.
        """
#        print(f"\n superLocus: searching for super locus of {q2} with type {ty} in varnums: {self.varnums}")
        vs = []
        for i in range(len(self.varnums)):
            loc, qty, vars = self.varnums[i]
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

    def compareSingle(self, q1: QXQRange, qs: [([QXQRange], QXQTy, dict)]):
        """
        Searches for a separable locus 'q1' in the list of states 'qs'.
        Handles both exact and partial (prefix) matches, correctly partitioning the state list.
        Returns a tuple: (original_var, matched_locus, matched_vars, remaining_states) or None.
        The 'original_var' is returned only on a partial match to signal that slicing is needed.
        """
        for i in range(len(qs)):
            loc, qty, vars_map = qs[i]
            
            if len(loc) != 1 or not (isinstance(qty, TyHad) or isinstance(qty, TyNor)):
                continue
            
            existing_locus = loc[0]
            
            if q1.location() == existing_locus.location():
                is_left_match = compareAExp(q1.crange().left(), existing_locus.crange().left())
                is_right_match = compareAExp(q1.crange().right(), existing_locus.crange().right())

                # Case 1: Exact Match.
                if is_left_match and is_right_match:
                    remaining_states = qs[:i] + qs[i+1:]
                    return (None, [q1], vars_map, remaining_states)
                
                # Case 2: Partial Match.
                if is_left_match:
                    original_var = next(iter(vars_map.values()))
                    
                    # For a partial match, we don't create new variables for the remainder here.
                    # The original var map is used, but with a new range.
                    # The caller (`includeIfLocus`) is responsible for generating slicing statements.
                    new_left_bound = q1.crange().right()
                    remaining_range = QXQRange(q1.location(), 
                                               crange=QXCRange(new_left_bound, existing_locus.crange().right()))
                    
                    remaining_state_tuple = ([remaining_range], qty, vars_map) # Use original vars_map
                    remaining_states = qs[:i] + [remaining_state_tuple] + qs[i+1:]
                    
                    # Return the original var to signal a split, the matched locus, the original vars, and the new remaining states.
                    return ([q1], vars_map, remaining_states)

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
        v = subLocus(q2, self.varnums)
        if v is not None:
            l, q, vs = v
            remaining = [s for s in self.varnums if s is not v]
            return [], l, q, vs, remaining

        ty = self.computeType(q2)
        rt = self.superLocus(q2, ty)
        #remainder from q2
        if rt is not None:
            rem, loc, qty, vars, tmpVarNums = rt
            
            if isinstance(ty, TyHad):
                stmts, new_vars = self._gen_had_to_en_cast_stmts(vars, loc[0].line_number())
                qty = TyEn(QXNum(1))
                
                nor_remind, nor_vars, remaining_after_nor = self.collectNorLocus(rem, tmpVarNums)
                for nor_locus, nor_var_map in nor_vars.items():
                    nor_var = next(iter(nor_var_map.values()))
                    merge_stmts, new_vars = self._gen_en_nor_extend_stmts(new_vars, nor_var, loc[0].line_number())
                    stmts.extend(merge_stmts)

                self.varnums = ([(loc, qty, new_vars)]) + remaining_after_nor
                return stmts, loc, qty, new_vars, remaining_after_nor

            if isinstance(ty, TyEn):
                current_stmts = []
                current_vars = vars             
                # Merge Nor loci first
                nor_rem, nor_vars_map, remaining_after_nor = self.collectNorLocus(rem, tmpVarNums)
                for nor_loc, nor_var_map in nor_vars_map.items():
                    nor_var = next(iter(nor_var_map.values()))
                    merge_stmts, new_vars = self._gen_en_nor_extend_stmts(current_vars, nor_var, loc[0].line_number())
                    current_stmts.extend(merge_stmts)
                    current_vars = new_vars
                    loc = loc + nor_loc

                # Then merge Had loci
                vb = self.findHadLocus(rem, remaining_after_nor)
                print(f"\n vb {vb}")
                if vb is not None:
                    had_locus, had_vars, remaining_after_had = vb
                    
                    # The specific splitting of the had_var is handled inside _gen_en_had_merge_stmts
                    had_var = next(iter(had_vars.values()))
                    ctrl_loc = had_locus[0]
                    merge_stmts, new_vars, q_leftover = self._gen_en_had_merge_stmts(current_vars, had_var, ctrl_loc, loc[0].line_number())                 
                    current_stmts.extend(merge_stmts)
                    current_vars = new_vars
                    loc = loc + had_locus
                    

                    for i in range(len(remaining_after_had)):
                        rem_loc, rem_qty, rem_vars = remaining_after_had[i]
                        if rem_loc[0].location() == q_leftover.ID():
                            new_rem_var = {q_leftover.ID(): q_leftover}
                            remaining_after_had[i] = (rem_loc, rem_qty, new_rem_var) 
                            break
           
                self.varnums = ([(loc, qty, current_vars)]) + remaining_after_had
                return current_stmts, loc, qty, current_vars, remaining_after_had
                
        return None, None, None, None, self.varnums

    def _sequence_comprehension_transform(self, ctx, unified_locus, unified_qty, unified_vars):
        """
        Generates imperative statements using sequence comprehensions to construct
        the new state after a quantum operation. Handles both standard oracles
        and quantum conditionals.
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




        # 1. Create the helper function for the lambda expression
        lambda_func = self._create_lambda_method(q_assign, unified_vars)
        self.addFuns.append(lambda_func)

        # 2. Create the sequence comprehension for the target variables
        target_names = {l.location() for l in q_assign.locus()}
#        print(f"\n target_names: {target_names}")
        if is_phase_oracle:
            target_names.add('amp') # Phase oracles also transform the amplitude
       
        # Determine the number of iterators needed
        nesting_depth = 0
        if isinstance(unified_qty, TyEn):
            nesting_depth = unified_qty.flag().num()
        
        iterators = [DXBind(f"k{i}", SType("nat")) for i in range(nesting_depth)]

        self.counter += 1
        for name in sorted(target_names):
            if is_phase_oracle and name != 'amp':
                continue
            var = unified_vars[name]        
            new_var = DXBind(f"{name}", var.type(), self.counter)
            
            # Build the expression inside the comprehension
            if is_conditional:
                control_qubit = ctx.bexp()
                control_var = unified_vars[control_qubit.location()]
                indexed_control = self._create_indexed_var(control_var, iterators)
                control_bit = DXIndex(indexed_control, control_qubit.crange().left().accept(self))
                condition = DXComp("==", control_bit, DXNum(1))
                print(f"\n condition {condition}")
                
                then_call_args = [self._create_indexed_var(unified_vars[n], iterators) for n in sorted(target_names)] + self.classical_args
                then_expr = DXCall(lambda_func.ID(), then_call_args)
                else_expr = self._create_indexed_var(var, iterators)
                comprehension_body = DXIfExp(condition, then_expr, else_expr)
            else: # Standard oracle
                call_args = [self._create_indexed_var(unified_vars[n], iterators) for n in sorted(target_names)] + self.classical_args
#                print(f"\n call_args: {call_args}")
                comprehension_body = DXCall(lambda_func.ID(), call_args)

            # Build the nested comprehension from the inside out
            comprehension = comprehension_body
            for i in range(len(iterators) - 1, -1, -1):
                iterator = iterators[i]
                range_var = self._create_indexed_var(var, iterators[:i])
                comprehension_range = DXLength(range_var)
                spec = DXRequires(DXInRange(iterator, DXNum(0), comprehension_range))
                comprehension = DXSeqComp(comprehension_range, iterator, spec, comprehension)

    #        print(f"\n comprehension {comprehension}")
            stmts.append(DXInit(new_var, comprehension))
    #        stmts.append(DXAssign([new_var], comprehension))
            new_vars[name] = new_var


        # Handle unchanged variables
        for name, var in unified_vars.items():
            if name not in new_vars:
                new_vars[name] = var # Simply pass through the old variable
        print(f"\n stmts, new_vars, {stmts}, \n{new_vars}")

        return stmts, new_vars
    
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
#       print(f"\n args: {args} \n{returns}")
        
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
    #                print(f"\n ket_expr: {ket_expr}")
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
    
    # ==========================================================================
    # == Core Logic for ORACLE Transformation
    # ==========================================================================

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
    # == Helper Methods for Predicate Generation
    # ==========================================================================
    @staticmethod
    def _get_total_nesting_depth(var_type: DXType):
        """Calculates the total number of sequence levels."""
        dims = 0
        temp_type = var_type
        while isinstance(temp_type, SeqType):
            dims += 1
            temp_type = temp_type.type()
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