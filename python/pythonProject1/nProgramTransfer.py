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

# def findHadRange(q2 : QXQRange, qs: [([QXQRange], QXQTy, dict)]):

#     vs = []
#     for i in range(len(qs)):
#         loc, qty, vars = qs[i]
#         if isinstance(qty, TyHad):
#             v = compareRangeLocus(q2, loc)
#             if v is not None:
#                 vs += [(v, qty, vars)]+qs[i+1:]
#                 return q2, vars, vs
#             else:
#                 return None
#         vs += [qs[i]]
#     return None


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
        self.working_predicate = []  # The current logical state, which gets transformed
        
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
            self.counter += 1
            var_map = make_dafny_vars_for_locus(locus, q_type, self.counter)
            self.varnums.append((locus, q_type, var_map))

        # --- Translate Arguments ---
        self.classical_args = [b.accept(self) for b in ctx.bindings() if b.accept(self) is not None]
        dafny_args = self.classical_args[:]
        print(f"\n dafny_args before varnums: {dafny_args}")
        for _, _, var_map in self.varnums:
            dafny_args.extend(sorted(var_map.values(), key=lambda v: v.ID()))


        # --- Translate and Store Preconditions ---
        self.final_requires = []
        raw_predicates = []
        for cond in ctx.conds():
            if isinstance(cond, QXRequires):
                self.current_line = cond.line_number()
                preds = cond.accept(self)
                if preds:
                    preds_list = preds if isinstance(preds, list) else [preds]
                    raw_predicates.extend(p for p in preds_list if p is not None and not (isinstance(p, DXBoolValue) and p.value() is True))
        
        # --- Remove Redundant Predicates ---
        # unique_predicates = []
        # for pred in raw_predicates:
        #     is_duplicate = any(EqualityVisitor().visit(pred, up) for up in unique_predicates)
        #     if not is_duplicate:
        #         unique_predicates.append(pred)

        self.final_requires = [DXRequires(p, line=self.current_line) for p in raw_predicates]
        self.working_predicate = copy.deepcopy(raw_predicates)
        print(f"\n final_requires: {self.final_requires}    ")

        # --- Prepare for Postcondition Translation ---
        out_tenv = self.tenv.get(self.fvar)[1]
        self.outvarnums = []
        for locus, q_type in out_tenv:
            self.counter += 1        
            var_map = make_dafny_vars_for_locus(locus, q_type, self.counter)
            self.outvarnums.append((locus, q_type, var_map))

        # --- Translate Method Body ---
        dafny_body = []
        if not ctx.axiom():
            for stmt in ctx.stmts():
                self.current_line = stmt.line_number()
                generated_stmts = stmt.accept(self)
                if generated_stmts:
                    dafny_body.extend(generated_stmts if isinstance(generated_stmts, list) else [generated_stmts])
            
            dafny_body = [s for s in dafny_body if not isinstance(s, DXMethod)]

            print(f"final dafny_body {dafny_body}")

            # --- Assign Final State to Return Variables ---
            final_assignments, final_var_mapping = self._get_final_assignments_and_mapping()
            dafny_body.extend(final_assignments)
            
            # --- Verification Check: Assert that derived state implies user's ensures clause ---
            
            # 2. Translate the user's ensures clauses, which will use the return variables
            self.t_ensures = True
            user_ensures_raw = []
            for cond in ctx.conds():
                if isinstance(cond, QXEnsures):
                    res = cond.accept(self)
                    if res:
                        user_ensures_raw.extend(res if isinstance(res, list) else [res])
            self.t_ensures = False
           

            # # 3. The final derived state must be mapped to the return variables for the assertion
            # derived_mapped_to_returns = self.working_predicate[:]
            # for i, pred in enumerate(derived_mapped_to_returns):
            #     for final_var_id, return_var in final_var_mapping.items():
            #          subst = SubstDAExp(final_var_id, return_var)
            #          derived_mapped_to_returns[i] = subst.visit(pred)

            # # 4. Add the assertion to the method body
            # if derived_mapped_to_returns and user_ensures_raw:
            #     derived_pred = self._join_predicates(derived_mapped_to_returns)
            #     for user_pred in user_ensures_raw:
            #         implication = DXLogic("==>", derived_pred, user_pred, line=self.current_line)
            #         dafny_body.append(DXAssert(implication, line=self.current_line))
            
            # 5. The public contract of the method is what the user wrote.
            final_ensures = [DXEnsures(p, line=self.current_line) for p in user_ensures_raw]

        else: # For axiomatic methods, just use the user's ensures
            final_ensures = []
            for cond in ctx.conds():
                if isinstance(cond, QXEnsures):
                    # We still need to accept to translate it
                    self.t_ensures = True
                    preds = cond.accept(self)
                    self.t_ensures = False
                    if preds:
                        final_ensures.extend([DXEnsures(p) for p in preds] if isinstance(preds, list) else [DXEnsures(preds)])

         #-- translate Returns ---           
        
        dafny_returns = [r.accept(self) for r in ctx.returns() if r.accept(self) is not None]
#        print(f"\nVar map in visitMethod: {self.outvarnums}")
        for _, _, var_map in self.outvarnums:            
            dafny_returns.extend(sorted(var_map.values(), key=lambda v: v.ID()))
        
        all_conditions = self.final_requires + final_ensures
        
        return DXMethod(self.fvar, ctx.axiom(), dafny_args, dafny_returns, all_conditions, dafny_body, line=ctx.line_number())

    # ==========================================================================
    # == Statement Visitors (Predicate Transformers)
    # ==========================================================================
    def visitQAssign(self, ctx: QXQAssign):
        """Transforms the state for a quantum assignment, using a ghost method for oracles."""
        if isinstance(ctx.exp(), QXOracle):
            return self._transform_for_oracle(ctx)
        return []

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

    def _transform_for_oracle(self, ctx: QXQAssign):
        """
        Generates a focused ghost method for a QXOracle, handles the frame
        explicitly, and returns the necessary assignments and calls.
        """
        oracle = ctx.exp()
        loc_tuple = subLocus(ctx.locus(), self.varnums)
        if not loc_tuple:
            raise Exception("Oracle locus not found in current state.")
        
        loc, qty, old_vars = loc_tuple

        if not isinstance(qty, TyEn):
            raise Exception("Oracle can only be applied to an entangled locus.")

        self.counter += 1
        new_vars = {k: v.newBind(self.counter) for k, v in old_vars.items()}
        
        # --- Identify Target and Frame Variables ---
        target_vars_id = {r.location() for r in ctx.locus()}
#        print(f"\n target_vars_id before amp: {target_vars_id}")
        if not (isinstance(oracle.phase().exps()[0], QXNum) and oracle.phase().exps()[0].num() == 0):
            target_vars_id.add('amp')

        target_old_vars = {k: v for k, v in old_vars.items() if k in target_vars_id}
        target_new_vars = {k: v for k, v in new_vars.items() if k in target_vars_id}
        
        frame_vars = {k: v for k, v in old_vars.items() if k not in target_vars_id}

        # --- Infer classical arguments from the oracle expression ---
        bound_vars = {b.ID() for b in oracle.bindings()}
        print(f"\n bound_vars: {bound_vars}")
        collector = BindCollector()
        for arg in oracle.vectors():         
            collector.visit(arg.accept(self))
        for exp in oracle.phase().exps():
            collector.visit(exp.accept(self))
        print(f"\n collector.renv: {collector.renv}")
#        dx_oracle_ket_expr = oracle.vectors()[0].accept(self) #assume only one vector for now
#        collector.visit(dx_oracle_ket_expr)
        free_var_ids = collector.renv - bound_vars
        oracle_classical_args = [arg for arg in self.classical_args if arg.ID() in free_var_ids]

        # --- Create and call the focused ghost method ---
        ghost_method = self._create_oracle_ghost_method(ctx, loc, qty, target_old_vars, target_new_vars, oracle_classical_args)
        self.addFuns.append(ghost_method)
        
        call_args = sorted(target_old_vars.values(), key=lambda v: v.ID()) + oracle_classical_args
        call = DXCall(ghost_method.ID(), call_args, line=ctx.line_number())
        
        # --- Build the statement list ---
        stmts = []
        # 1. Initialize only the new target variables
        stmts.extend([DXInit(v, line=ctx.line_number()) for v in target_new_vars.values()])
        # 2. Call the ghost method for the transformation
        stmts.append(DXAssign(sorted(target_new_vars.values(), key=lambda v: v.ID()), call, line=self.current_line))

        # --- Update State and Predicates ---
        # The new state is a mix of the old frame and the new transformed variables
        combined_new_vars = frame_vars.copy()
        combined_new_vars.update(target_new_vars)

        self._update_working_predicate_after_oracle(ghost_method, old_vars, new_vars)
        self._update_state_for_locus(loc, qty, combined_new_vars)
        
        return stmts

    def _create_oracle_ghost_method(self, ctx: QXQAssign, loc: [QXQRange], qty: QXQTy, old_vars: dict, new_vars: dict, classical_args: list):
        """
        Builds a focused, axiomatic ghost method that logically defines the oracle's transformation.
        """
        oracle = ctx.exp()
        ghost_name = f"ghost_oracle_{self.counter}"
        target_loc_pos = [min(findPos(r, loc), qty.flag().num()-1) for r in ctx.locus()]

        ghost_args = sorted(old_vars.values(), key=lambda v: v.ID()) + classical_args
        ghost_returns = sorted(new_vars.values(), key=lambda v: v.ID())

        ghost_conds = []
        classical_arg_ids = {arg.ID() for arg in classical_args}
        for req in self.final_requires:
            collector = BindCollector()
            collector.visit(req.spec())
            if collector.renv.issubset(classical_arg_ids):
                ghost_conds.append(req)

        # Generate predicates and wrap them in DXEnsures
        shape_preds = self._get_shape_preservation_predicates(old_vars, new_vars)
        
        def rhs_generator(var_name, old_bind, iterators):
            logic_map = self._get_transformation_logic(oracle, old_vars, iterators, target_loc_pos)
            return logic_map.get(var_name)
        
        trans_pred = self._get_transformation_predicate(old_vars, new_vars, rhs_generator)
        
        ensures_clauses = [DXEnsures(p) for p in shape_preds] + [DXEnsures(trans_pred)]

        return DXMethod(ghost_name, True, ghost_args, ghost_returns, ghost_conds + ensures_clauses, [], line=self.current_line)

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
    
    # def _add_transformation_ensures(self, ensures_clauses, oracle, old_vars, new_vars, target_loc_pos):
    #     """
    #     Adds the ensures clause for the logical transformation of values.
    #     This function now distinguishes between phase oracles and value-transforming oracles.
    #     """
    #     qubit_vars = {k: v for k, v in old_vars.items() if k != 'amp'}
    #     if not qubit_vars: return

    #     rep_type = next(iter(qubit_vars.values())).type()
    #     # The number of iterators is the total depth of the qubit register minus one,
    #     # which correctly corresponds to the total depth of the amplitude array.
    #     num_iterators = self._get_total_nesting_depth(rep_type) - 1
    #     iterators = [DXBind(f"k{i}", SType("nat")) for i in range(num_iterators)]

    #     all_equalities = []

    #     # Determine if this is a phase oracle. This is true if the oracle has no
    #     # output vectors specified, or if the output vectors are identical to the input bindings.
    #     is_phase_oracle = False
    #     if not oracle.vectors():
    #         is_phase_oracle = True
    #     else:
    #         # Use EqualityVisitor to check if vector expressions are the same as the bindings.
    #         # This is a heuristic that assumes a 1-to-1 mapping.
    #         if len(oracle.vectors()) == len(oracle.bindings()):
    #             is_identical = True
    #             for i in range(len(oracle.vectors())):
    #                 # A QXKet's vector is the expression. We compare it to the binding.
    #                 if not EqualityVisitor().visit(oracle.vectors()[i].vector(), oracle.bindings()[i]):
    #                     is_identical = False
    #                     break
    #             if is_identical:
    #                 is_phase_oracle = True

    #     # Case 1: Phase Oracle (e.g., QFT). Qubit values don't change, phase does.
    #     if is_phase_oracle:

    #         # Assert the transformation on the amplitude.
    #         if 'amp' in old_vars:
    #             old_amp_var = old_vars['amp']
    #             new_amp_var = new_vars['amp']
    #             indexed_old_amp = self._create_indexed_var(old_amp_var, iterators)
    #             indexed_new_amp = self._create_indexed_var(new_amp_var, iterators)

    #             phase_template = oracle.phase().accept(self)
    #             final_phase_expr = phase_template

    #             # Substitute placeholders in the phase expression, e.g., 'x' with castBVInt(p1[k0]).
    #             if oracle.bindings():
    #                 placeholder_id = oracle.bindings()[0].ID()
    #                 # The value to substitute is the integer value of the *old* qubit register.
    #                 old_qubit_var = next(iter(qubit_vars.values()))
    #                 indexed_old_qubit = self._create_indexed_var(old_qubit_var, iterators)
    #                 subst_val = DXCall("castBVInt", [indexed_old_qubit])
    #                 self.libFuns.add('castBVInt')
    #                 subst = SubstDAExp(placeholder_id, subst_val)
    #                 final_phase_expr = subst.visit(phase_template)

    #             # new_amp = old_amp * phase
    #             amp_equality = DXComp("==", indexed_new_amp, DXBin("*", indexed_old_amp, final_phase_expr), line=self.current_line)
    #             all_equalities.append(amp_equality)

    #     # Case 2: Value-transforming oracle.
    #     else:
    #         ket_idx = 0
    #         for var_name, old_bind in sorted(old_vars.items()):
    #             new_bind = new_vars[var_name]
    #             indexed_new = self._create_indexed_var(new_bind, iterators)

    #             if var_name == 'amp':
    #                 indexed_old = self._create_indexed_var(old_bind, iterators)
    #                 equality = DXComp("==", indexed_new, indexed_old, line=self.current_line) # Amplitudes are unchanged
    #             else:
    #                 ket_expr_qafny = oracle.vectors()[ket_idx]
    #                 dx_oracle_expr = ket_expr_qafny.accept(self)
    #                 final_expr = dx_oracle_expr
    #                 if not EqualityVisitor().visit(ket_expr_qafny.vector(), oracle.bindings()[ket_idx]):
    #                     for j, binder in enumerate(oracle.bindings()):
    #                         if j < len(iterators):
    #                             subst = SubstDAExp(binder.ID(), iterators[target_loc_pos[j]])
    #                             final_expr = subst.visit(final_expr)
                    
    #                 self.libFuns.add('castBVInt')
    #                 equality = DXComp("==", DXCall("castBVInt", [indexed_new]), final_expr, line=self.current_line)
    #                 ket_idx += 1
    #             all_equalities.append(equality)
    #     if not all_equalities: return

    #     combined_body = all_equalities[0]
    #     if len(all_equalities) > 1:
    #         combined_body = DXLogic("&&", all_equalities[0], all_equalities[1], line=self.current_line)
    #         for i in range(2, len(all_equalities)):
    #             combined_body = DXLogic("&&", combined_body, all_equalities[i], line=self.current_line)

    #     final_forall = self._wrap_in_forall(combined_body, iterators, next(iter(qubit_vars.values())))
    #     ensures_clauses.append(DXEnsures(final_forall))




    # def _update_state_for_locus(self, target_locus: list, new_qty: QXQTy, new_vars: dict):
    #     """Updates self.varnums by replacing an old locus with its new version."""
    #     new_varnums = []
    #     found = False
    #     for loc, qty, var_map in self.varnums:
    #         is_match = (len(loc) == len(target_locus)) and all(compareLocus([target_locus[i]], [loc[i]]) == [] for i in range(len(loc)))
    #         if is_match and not found:
    #             new_varnums.append((target_locus, new_qty, new_vars))
    #             found = True
    #         else:
    #             new_varnums.append((loc, qty, var_map))
    #     self.varnums = new_varnums

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
        #deal with classical boolean expression
        if isinstance(ctx.bexp(), QXBool):
            bex = ctx.bexp().accept(self)

            tmpMap = deepcopy(self.varnums)
            terms = []
            for elem in ctx.stmts():
                terms += elem.accept(self)

            self.varnums = tmpMap
            elses = []
            for elem in ctx.else_stmts():
                elses += elem.accept(self)

            return [DXIf(bex, terms, elses, line=ctx.line_number())]

#        if isinstance(ctx.bexp(), QXQRange):
#            return self._transform_for_quantum_conditional(ctx)
        
        lc = LocusCollector()
        lc.visit(ctx.bexp())
        for stmt in ctx.stmts():
            lc.visit(stmt)
        print('\n QIf locus collector:', lc.renv)
        for elem in lc.renv:
            print('\n check type', subLocus([elem], self.varnums))
        #includeIfLocus provides a final locus, type, and num, as well as the result lib function call in Dafny in tres
        tres, nLoc, nqty, nnum = self.includeIfLocus(lc.renv)
        print('\n QIf tres:', tres, nLoc, nqty, nnum)


        if isinstance(ctx.bexp(), QXQIndex):
            bres = ctx.bexp().accept(self)
            changeVar = UpdateVars(nnum, self.counter)
            changeVar.visit(bres)
            self.currLocus = nLoc, nqty, changeVar.vars()
            self.currLocus = changeVar.counter()
            self.conStack += [EnFactor(('==', bres, DXNum(1)))]
            ifexp = []
        else:
            ifexp = ctx.bexp().accept(self)
        
        result = ifexp

        for stmt in ctx.stmts():
            tmp = stmt.accept(self)
            if tmp is None:
                return None
            else:
                result += [tmp]

        return result




    # def visitIf(self, ctx: Programmer.QXIf):
    #     """
    #     Translates a classical if-then-else or a quantum conditional statement.
    #     """
    #     # Case 1: Quantum Conditional `if (q[i]) { ... }`
    #     if isinstance(ctx.bexp(), QXQRange):
    #         # A quantum conditional is a pure predicate transformation.
    #         # It modifies the `working_predicate` directly and returns no statements.
            
    #         # 1. Setup: Find the locus and create new state variables
    #         if not ctx.stmts() or not isinstance(ctx.stmts()[0], QXQAssign):
    #             raise Exception("Quantum conditional body must be a single oracle assignment.")
            
    #         q_assign = ctx.stmts()[0]
    #         control_qubit = ctx.bexp()
    #         control_locus_name = control_qubit.location()
    #         target_locus_names = {r.location() for r in q_assign.locus()}
            
    #         loc_tuple = None
    #         for l, qty, var_map in self.varnums:
    #             names_in_locus = {r.location() for r in l}
    #             if control_locus_name in names_in_locus and target_locus_names.issubset(names_in_locus):
    #                 loc_tuple = (l, qty, var_map)
    #                 break
            
    #         if not loc_tuple:
    #             raise Exception("Control and target for quantum conditional must be in the same entangled locus.")

    #         loc, qty, old_vars = loc_tuple
    #         self.counter += 1
    #         new_vars = {k: v.newBind(self.counter) for k, v in old_vars.items()}

    #         # 2. Build the new predicates that define the transformed state
    #         new_predicates = self._get_shape_preservation_predicates(old_vars, new_vars)
            
    #         # Define the RHS logic for the conditional's `if-then-else` expression
    #         oracle = q_assign.exp()
    #         target_loc_pos = [min(findPos(r, loc), qty.flag().num()-1) for r in q_assign.locus()]
            
    #         qubit_vars = {k: v for k, v in old_vars.items() if k != 'amp'}
    #         rep_type = next(iter(qubit_vars.values())).type()
    #         num_iterators = self._get_total_nesting_depth(rep_type) - 1
    #         iterators = [DXBind(f"k{i}", SType("nat")) for i in range(num_iterators)]

    #         then_logic = self._get_transformation_logic(oracle, old_vars, iterators, target_loc_pos)

    #         def rhs_generator(var_name, old_bind, iterators):
    #             indexed_old = self._create_indexed_var(old_bind, iterators)
    #             control_var_old = old_vars[control_qubit.ID()]
    #             indexed_control_var = self._create_indexed_var(control_var_old, iterators)
    #             control_bit = DXIndex(indexed_control_var, control_qubit.exp().accept(self))
    #             self.libFuns.add('castBVInt')
    #             condition = DXComp("==", DXCall("castBVInt", [control_bit]), DXNum(1))
    #             then_expr = then_logic.get(var_name, indexed_old)
    #             return DXIf(condition, then_expr, indexed_old, line=self.current_line)

    #         # Build the final `forall` clause using the generic helper
    #         transformation_pred = self._get_transformation_predicate(old_vars, new_vars, rhs_generator)
    #         new_predicates.append(transformation_pred)

    #         # 3. Update the main working predicate and state
    #         old_var_ids = {v.ID() for v in old_vars.values()}
    #         unaffected_predicates = [p for p in self.working_predicate if not old_var_ids.intersection(BindCollector().visit(p).renv)]
    #         self.working_predicate = unaffected_predicates + new_predicates
    #         self._update_state_for_locus(loc, qty, new_vars)
            
    #         return [] # Return no statements
    

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
        print(f"\nFinal assignments: {assignments}")                    
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
    def _transform_for_quantum_conditional(self, ctx: Programmer.QXIf):
        """
        Handles the predicate transformation for a quantum conditional.
        This is a pure logical transformation; it does not generate executable statements.
        """
        # 1. Setup: Find the relevant locus and create new state variables
        if not ctx.stmts() or not isinstance(ctx.stmts()[0], QXQAssign): #Need to adjust for nested ifs
            raise Exception("Quantum conditional body must be a single oracle assignment.")
        
        q_assign = ctx.stmts()[0]
        control_qubit = ctx.bexp()
        control_locus_name = control_qubit.location()
        target_locus_names = {r.location() for r in q_assign.locus()}
        
        loc_tuple = None
        for l, qty, var_map in self.varnums:
            names_in_locus = {r.location() for r in l}
            if control_locus_name in names_in_locus and target_locus_names.issubset(names_in_locus):
                print('\n lets check locus', subLocus([control_qubit], self.varnums))
                loc_tuple = (l, qty, var_map)
                break
        
        if not loc_tuple:
            raise Exception("Control and target for quantum conditional must be in the same entangled locus.")

        loc, qty, old_vars = loc_tuple
        self.counter += 1
        new_vars = {k: v.newBind(self.counter) for k, v in old_vars.items()}

        # 2. Build the new predicates that define the transformed state
        new_predicates = self._get_shape_preservation_predicates(old_vars, new_vars)
        
        # Define the RHS logic for the conditional's `if-then-else` expression
        oracle = q_assign.exp()
        target_loc_pos = [min(findPos(r, loc), qty.flag().num()-1) for r in q_assign.locus()]
        
        qubit_vars = {k: v for k, v in old_vars.items() if k != 'amp'}
        rep_type = next(iter(qubit_vars.values())).type()
        num_iterators = self._get_total_nesting_depth(rep_type) - 1
        iterators = [DXBind(f"k{i}", SType("nat")) for i in range(num_iterators)]

        then_logic = self._get_transformation_logic(oracle, old_vars, iterators, target_loc_pos)

        def rhs_generator(var_name, old_bind, iterators):
            indexed_old = self._create_indexed_var(old_bind, iterators)
            control_var_old = old_vars[control_qubit.ID()]
            indexed_control_var = self._create_indexed_var(control_var_old, iterators)
            control_bit = DXIndex(indexed_control_var, control_qubit.exp().accept(self))
            self.libFuns.add('castBVInt')
            condition = DXComp("==", DXCall("castBVInt", [control_bit]), DXNum(1))
            then_expr = then_logic.get(var_name, indexed_old)
            return DXIf(condition, then_expr, indexed_old, line=self.current_line)

        # Build the final `forall` clause using the generic helper
        transformation_pred = self._get_transformation_predicate(old_vars, new_vars, rhs_generator)
        new_predicates.append(transformation_pred)

        # 3. Update the main working predicate and state
        old_var_ids = {v.ID() for v in old_vars.values()}
        unaffected_predicates = [p for p in self.working_predicate if not old_var_ids.intersection(BindCollector().visit(p).renv)]
        self.working_predicate = unaffected_predicates + new_predicates
        self._update_state_for_locus(loc, qty, new_vars)
        
        return [] # Return no statements

    def genHadEnCastPred(self, vars: dict, qty: QXQTy, line_number: int):
        """
        Generates the Dafny AST for casting a TyHad state to a TyEn state.
        This involves creating new Dafny variables for the TyEn representation and
        generating a call to the 'hadEn' library function.
        """
        newvars = self.upVarsType(vars, qty)
        result = [DXInit(x, line=line_number) for x in newvars.values()]
        newampvar = [x for x in newvars.values() if x.ID() == 'amp']
        othervars = [x for x in newvars.values() if x.ID() != 'amp']
        result += [DXAssign(newampvar + othervars, DXCall("hadEn", vars.values(), line=line_number),
                            line=line_number)]
        self.libFuns.add('hadEn')
        return newvars, result

    def genEnNorExtendPred(self, x:DXBind, y:DXBind, qty:QXQTy, line_number: int):
        """
        Generates the Dafny AST for extending an existing TyEn locus by merging a
        classical (TyNor) qubit into it. It generates a call to the 'mergeBitEn'
        library function.
        """
        self.libFuns.add('mergeBitEn') #well, this is not from Nor type but Had type
        v = DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXCall('mergeBitEn',
                                [DXLength(x), y]),True, line=line_number)
        self.counter += 1
        return v

    #TODO: need to modify the meaning of duplicateMergeBitEn to be a AA type choice,
    #the number of elements in seq will not change, but we will have case for y == 0 and y == 1
    def genEnHadAASeqPred(self, vars: dict, y:DXBind, qty:QXQTy, line_number: int):
        """
        Generates the Dafny AST for the complex operation of merging a TyHad control
        qubit with a TyEn target locus. This results in a TyAA state, representing
        the two branches of the quantum conditional.
        """
        x = vars.values()[0]
        res = []

        yleft = y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)
        res += DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXNum(0),True, line=line_number)
        self.counter += 1

        yright = y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)
        res += DXAssign([y.newBindType(genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                         DXNum(1),True, line=line_number)
        self.counter += 1

        self.libFuns.add('duplicateMergeBitEn')
        self.libFuns.add('mergeAmpEn')
        self.libFuns.add('omega')
        half = DXBin('/', QXNum(1), DXUni('sqrt', 2))
        omega = DXCall('omega', [DXNum(1),DXNum(2)], True, line=self.current_line)
        for elem in vars.values():
            if elem.ID() != 'amp':
                #the duplicateMergeBitEn will simply copy the same result from the old elem to be a new one
                #and perform y == 0 ==> duplicate as well as y == 1 ==> duplicate
                res += [DXAssign([DXBind(elem.ID(), genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                     DXCall('duplicateMergeBitEn',  [yleft, DXNum(0), DXLength(x), elem]),
                     True, line=line_number)]
                self.counter += 1
                res += [DXAssign([DXBind(elem.ID(), genType(qty.flag(), SeqType(SType("bv1"))), self.counter)],
                     DXCall('duplicateMergeBitEn',  [yright, DXNum(1), DXLength(x), elem]),
                     True, line=line_number)]
                self.counter += 1
            else:
                res += [DXAssign([DXBind('amp', genType(qty.flag(), (SType("real"))), self.counter)],
                     DXCall('mergeAmpEn', [yleft, DXNum(0), DXLength(x), elem, half]),
                     True, line=line_number)]
                self.counter += 1
                res += [DXAssign([DXBind('amp', genType(qty.flag(), (SType("real"))), self.counter)],
                     DXCall('mergeAmpEn', [yleft, DXNum(0), DXLength(x), elem,
                                           DXBin('*', half, DXBin('*', omega, y))]),
                     True, line=line_number)]
                self.counter += 1


    def superLocus(self, q2: [QXQRange], ty:QXQTy):
        """
        Searches the current state (self.varnums) to find a component that overlaps
        with the target locus 'q2' and matches the target type 'ty'.
        """
        vs = []
        for i in range(len(self.varnums)):
            loc, qty, vars = self.varnums[i]
            if ty == qty:
                # A more robust overlap check:
                # Does any part of the existing locus 'loc' appear in the required locus 'q2'?
                is_loc_in_q2 = any(compareLocus([l_part], q2) is not None for l_part in loc)
                # Does any part of the required locus 'q2' appear in the existing locus 'loc'?
                is_q2_in_loc = any(compareLocus([q2_part], loc) is not None for q2_part in q2)

                if is_loc_in_q2 or is_q2_in_loc:
                    # We found an overlapping component.
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

    def includeIfLocus(self, q2: [QXQRange]):
        v = subLocus(q2, self.varnums)
        print(f"\n includeIfLocus: looking for locus {q2}, found: {v}")
        if v is not None:
            return [], v

        ty = self.computeType(q2)
        rt = self.superLocus(q2, ty)

        print(f"\n includeIfLocus: computed type {ty} for locus {q2}, super locus result: {rt}")

        if rt is not None:
            remind, loc, qty, vars, tmpVarNums = rt
            if isinstance(ty, TyHad):
                _, pred = self.genHadEnCastPred(vars, qty, loc[0].line_number())
                qty = TyEn(QXNum(1), qty.line_number())
                norRe, norVars, tmpVarNumsa = self.collectNorLocus(remind, tmpVarNums)
                loc = loc + norRe
                for v in norVars.values():
                    pred += [self.genEnNorExtendPred(vars.values()[0], v, qty, loc[0].line_number())]
                vars.update({k:v for k,v in norVars.items()})
                self.varnums = ([(loc, qty, vars)]) + tmpVarNumsa
                return pred, loc, qty, vars

            if isinstance(ty, TyEn):
                print('\n includeIfLocus en-en:', loc, qty, vars)
                print('\n includeIfLocus en-en remind:', remind, tmpVarNums)
                norRe, norVars, tmpVarNumsa = self.collectNorLocus(remind, tmpVarNums)
                print('\n includeIfLocus en-en norRe:', norRe, norVars, tmpVarNumsa)
                loc = loc + norRe
                pred = []
                for v in norVars.values():
                    pred += [self.genEnNorExtendPred(vars.values()[0], v, qty, loc[0].line_number())]
                vars.update({k:v for k,v in norVars.items()})
                vb = findHadLocus(remind, tmpVarNumsa)
                print('\n includeIfLocus en-en vb:', vb)
                if vb is not None:
                    qa, oldVars, tmpVarNumsb = vb
                    pred += self.genEnHadAASeqPred(vars, oldVars.values()[0], ty, loc[0].line_number())
                    qty = TyEn(qty.flag(), qa, loc[0].line_number())
                self.varnums = ([(loc, qty, vars)]) + tmpVarNumsa
                return pred, loc, qty, vars
        return None


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
                    if EqualityVisitor().visit(elem, loc[0]):
                        vs += [(loc, qty, vars)]
                    else:
                        reLoc = compareRangeLocus(elem, loc)
                        qv += [(reLoc, qty, vars)]

                        vs += [(elem, qty, self.upVars(vars))]
                else:
                    qv += [(loc, qty, vars)]

        newVars = dict()
        va = []
        for loc, qty, vars in vs:
            va += loc
            newVars.update({k:v for k,v in vars.items()})

        return va, newVars, qv
    
    def upVars(self, v: dict):
        """Updates Dafny variables with incremented counters."""
        tmp = dict()
        for key in v.keys():
            tmp.update({key: v.get(key).newBind(self.counter)})
            self.counter += 1

        return tmp

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
    
    def _update_working_predicate_after_oracle(self, ghost_method, old_vars, new_vars):
        """Replaces old predicates with the new state defined by the ghost method."""
        new_predicates = []
        affected_var_ids = {v.ID() for v in old_vars.values()}
        
        for pred in self.working_predicate:
            collector = BindCollector()
            collector.visit(pred)
            if not affected_var_ids.intersection(collector.renv):
                new_predicates.append(pred)

        ghost_returns = sorted(ghost_method.returns(), key=lambda v: v.ID())
        current_state_vars = sorted(new_vars.values(), key=lambda v: v.ID())

        for ensure_clause in ghost_method.conds():
            if isinstance(ensure_clause, DXEnsures):
                pred = ensure_clause.spec()
                transformed_pred = pred
                for i in range(len(ghost_returns)):
                    subst = SubstDAExp(ghost_returns[i].ID(), current_state_vars[i])
                    transformed_pred = subst.visit(transformed_pred)
                new_predicates.append(transformed_pred)

        self.working_predicate = new_predicates
    
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