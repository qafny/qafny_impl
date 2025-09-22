import pytest
from CollectKind import CollectKind, compareType, compareAExp
from Programmer import *

@pytest.fixture
def collect_kind():
    """Fixture to create a fresh CollectKind instance for each test."""
    return CollectKind()

# --- compareType Tests ---
def test_compare_type_both_none():
    """Test that compareType returns True when both types are None."""
    assert compareType(None, None) is True

def test_compare_type_one_none():
    """Test that compareType returns False when one type is None."""
    ty = TySingle("nat")
    assert compareType(None, ty) is False
    assert compareType(ty, None) is False

def test_compare_type_tysingle_same():
    """Test that compareType returns True for two TySingle with the same type."""
    ty1 = TySingle("nat")
    ty2 = TySingle("nat")
    assert compareType(ty1, ty2) is True

def test_compare_type_tysingle_different():
    """Test that compareType returns False for two TySingle with different types."""
    ty1 = TySingle("nat")
    ty2 = TySingle("bool")
    assert compareType(ty1, ty2) is False

# --- compareAExp Tests ---
def test_compare_aexp_qxnum_same():
    """Test that compareAExp returns True for two QXNum with the same number."""
    a1 = QXNum(42)
    a2 = QXNum(42)
    assert compareAExp(a1, a2) is True

def test_compare_aexp_qxnum_different():
    """Test that compareAExp returns False for two QXNum with different numbers."""
    a1 = QXNum(42)
    a2 = QXNum(43)
    assert compareAExp(a1, a2) is False

def test_compare_aexp_qxbind_same():
    """Test that compareAExp returns True for two QXBind with the same ID and type."""
    ty = TySingle("nat")
    a1 = QXBind("x", ty)
    a2 = QXBind("x", ty)
    assert compareAExp(a1, a2) is True

def test_compare_aexp_qxbind_different_id():
    """Test that compareAExp returns False for two QXBind with different IDs."""
    ty = TySingle("nat")
    a1 = QXBind("x", ty)
    a2 = QXBind("y", ty)
    assert compareAExp(a1, a2) is False

# --- visitMethod Tests ---
def test_visit_method(collect_kind):
    """Test that visitMethod correctly populates the environment with parameters and returns."""
    ty_nat = TySingle("nat")
    ty_bool = TySingle("bool")
    binding_x = QXBind("x", ty_nat)
    return_y = QXBind("y", ty_bool)
    method = QXMethod("test_method", False, [binding_x], [return_y], [], [])
    result = collect_kind.visitMethod(method)
    assert result is True
    assert "test_method" in collect_kind.env
    assert collect_kind.env["test_method"][0]["x"] == ty_nat  # tenv
    assert collect_kind.env["test_method"][1]["y"] == ty_bool  # xenv

# --- visitInit Tests ---
def test_visit_init(collect_kind):
    """Test that visitInit adds a variable to tenv with its type."""
    ty = TySingle("nat")
    binding = QXBind("z", ty)
    init_stmt = QXInit(binding)
    result = collect_kind.visitInit(init_stmt)
    assert result is True
    assert "z" in collect_kind.tenv
    assert collect_kind.tenv["z"] == ty

# --- visitMeasure Tests ---
def test_visit_measure_valid(collect_kind):
    """Test that visitMeasure returns True for IDs with valid bit types in tenv."""
    ty_nat = TySingle("nat")
    collect_kind.tenv = {"m": ty_nat}
    id_m = QXBind("m", ty_nat)
    measure_stmt = QXMeasure([id_m], [])
    result = collect_kind.visitMeasure(measure_stmt)
    assert result is True

def test_visit_measure_invalid(collect_kind):
    """Test that visitMeasure returns False for IDs with non-bit types."""
    ty_qubit = TySingle("qubit")
    collect_kind.tenv = {"m": ty_qubit}
    id_m = QXBind("m", ty_qubit)
    measure_stmt = QXMeasure([id_m], [])
    result = collect_kind.visitMeasure(measure_stmt)
    assert result is False

# --- visitFor Tests ---
def test_visit_for(collect_kind):
    """Test that visitFor adds the loop variable to tenv with type 'nat'."""
    crange = QXCRange(left=QXNum(0), right=QXNum(10))
    for_stmt = QXFor(id="i", crange=crange, conds=[], stmts=[])
    result = collect_kind.visitFor(for_stmt)
    assert result is True
    assert "i" in collect_kind.tenv
    assert isinstance(collect_kind.tenv["i"], TySingle)
    assert collect_kind.tenv["i"].type() == "nat"

# --- visitBind Tests ---
def test_visit_bind_in_tenv(collect_kind):
    """Test that visitBind returns True if the binding's ID is in tenv with a matching type."""
    ty = TySingle("nat")
    collect_kind.tenv = {"x": ty}
    ctx = QXBind("x", ty)
    assert collect_kind.visitBind(ctx) is True

def test_visit_bind_not_in_env(collect_kind):
    """Test that visitBind returns False if the binding's ID is not in tenv or xenv."""
    ty = TySingle("nat")
    ctx = QXBind("x", ty)
    assert collect_kind.visitBind(ctx) is False

# --- visitCAssign Tests ---
def test_visit_cassign_valid(collect_kind):
    """Test that visitCAssign returns True for a valid assignment."""
    ty = TySingle("nat")
    collect_kind.tenv = {"x": ty}
    aexp = QXNum(42)
    cass_stmt = QXCAssign([QXBind("x", ty)], aexp)
    result = collect_kind.visitCAssign(cass_stmt)
    assert result is True

def test_visit_cassign_invalid_id(collect_kind):
    """Test that visitCAssign returns False for an invalid ID in assignment."""
    ty = TySingle("nat")
    collect_kind.tenv = {}
    aexp = QXNum(42)
    cass_stmt = QXCAssign([QXBind("x", ty)], aexp)
    result = collect_kind.visitCAssign(cass_stmt)
    assert result is False

# --- visitIf Tests ---
def test_visit_if_valid(collect_kind):
    """Test that visitIf returns True for a valid if statement."""
    bexp = QXBoolLiteral(True)
    collect_kind.xenv = {}  # No expected return values
    stmts = [QXReturn(ids=[])]  # Empty return statement
    if_stmt = QXIf(bexp, stmts, [])
    result = collect_kind.visitIf(if_stmt)
    assert result is True

def test_visit_if_valid_with_return_ids(collect_kind):
    """Test that visitIf returns True for a valid if with a non-empty return."""
    bexp = QXBoolLiteral(True)
    collect_kind.xenv = {"y": TySingle("bool")}  # Expected return type
    stmts = [QXReturn(ids=["y"])]  # Return variable 'y'
    if_stmt = QXIf(bexp, stmts, [])
    result = collect_kind.visitIf(if_stmt)
    assert result is True

# --- visitQSpec Tests ---
def test_visit_qspec_valid(collect_kind):
    """Test that visitQSpec returns True for a valid quantum specification with no crange."""
    locus = [QXQRange("q", crange=None)]
    qty = TyQ(QXNum(1))
    states = [QXTensor([QXSKet(QXNum(0))])]
    qspec = QXQSpec(locus, qty, states)
    result = collect_kind.visitQSpec(qspec)
    assert result is True

# --- visitOracle Tests ---
def test_visit_oracle_valid(collect_kind):
    """Test that visitOracle returns True for a valid oracle operation."""
    bindings = [QXBind("k", TySingle("nat"))]
    amp = QXNum(1)
    kets = [QXSKet(QXNum(0))]
    oracle = QXOracle(bindings, amp, kets)
    result = collect_kind.visitOracle(oracle)
    assert result is True

# --- visitProgram Tests ---
def test_visit_program_valid(collect_kind):
    """Test that visitProgram returns True for a valid program."""
    method = QXMethod("main", False, [], [], [], [])
    program = QXProgram([method])
    result = collect_kind.visitProgram(program)
    assert result is True

# --- visitCall Tests ---
def test_visit_call_empty_exps(collect_kind):
    """Test that visitCall returns True for a call with no arguments."""
    call = QXCall("some_method", [])
    result = collect_kind.visitCall(call)
    assert result is True

def test_visit_call_valid_exps(collect_kind):
    """Test that visitCall returns True for valid expressions."""
    ty = TySingle("nat")
    collect_kind.tenv = {"x": ty}
    exps = [QXNum(42), QXBind("x", ty)]
    call = QXCall("some_method", exps)
    result = collect_kind.visitCall(call)
    assert result is True

def test_visit_call_invalid_exp(collect_kind):
    """Test that visitCall returns False for an invalid expression."""
    collect_kind.tenv = {}
    exps = [QXBind("x", TySingle("nat"))]  # "x" not in tenv or xenv
    call = QXCall("some_method", exps)
    result = collect_kind.visitCall(call)
    assert result is False

def test_visit_call_mixed_exps(collect_kind):
    """Test that visitCall returns False if any expression is invalid."""
    ty = TySingle("nat")
    collect_kind.tenv = {"x": ty}
    exps = [QXNum(42), QXBind("y", ty)]  # "y" not in tenv
    call = QXCall("some_method", exps)
    result = collect_kind.visitCall(call)
    assert result is False

def test_visit_call_nested_exps(collect_kind):
    """Test that visitCall handles nested expressions correctly."""
    ty = TySingle("nat")
    collect_kind.tenv = {"x": ty}
    bin_exp = QXBin("add", QXNum(1), QXBind("x", ty))
    call = QXCall("some_method", [bin_exp])
    result = collect_kind.visitCall(call)
    assert result is True

def test_visit_call_env_interaction(collect_kind):
    """Test that visitCall validates expressions against tenv."""
    ty_nat = TySingle("nat")
    ty_bool = TySingle("bool")
    collect_kind.tenv = {"x": ty_nat}
    exps = [QXBind("x", ty_nat), QXBind("x", ty_bool)]  # Type mismatch
    call = QXCall("some_method", exps)
    result = collect_kind.visitCall(call)
    assert result is False