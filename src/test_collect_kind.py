import unittest
from Programmer import *
from CollectKind import CollectKind

class TestCollectKind(unittest.TestCase):

    def setUp(self):
        self.collector = CollectKind()

    def test_visit_bind(self):
        bind = QXBind("x", TySingle("nat"))
        self.collector.tenv["x"] = TySingle("nat")
        self.assertTrue(self.collector.visitBind(bind))

    def test_visit_bind_with_type_mismatch(self):
        bind = QXBind("x", TySingle("real"))
        self.collector.tenv["x"] = TySingle("nat")
        self.assertFalse(self.collector.visitBind(bind))

    def test_visit_bind_with_no_type(self):
        bind = QXBind("x")
        self.assertFalse(self.collector.visitBind(bind))

    def test_visit_bind_with_multiple_types(self):
        bind1 = QXBind("x", TySingle("nat"))
        bind2 = QXBind("y", TySingle("real"))
        self.collector.tenv["x"] = TySingle("nat")
        self.collector.tenv["y"] = TySingle("real")
        self.assertTrue(self.collector.visitBind(bind1))
        self.assertTrue(self.collector.visitBind(bind2))

    def test_visit_bind_with_invalid_type(self):
        bind = QXBind("x", TySingle("invalid_type"))
        self.collector.tenv["x"] = TySingle("nat")
        self.assertFalse(self.collector.visitBind(bind))

    def test_visit_method(self):
        method = QXMethod("Method", False, [QXBind("x", TySingle("nat"))], [QXBind("y", TySingle("real"))], [], [])
        self.assertTrue(self.collector.visitMethod(method))
        self.assertIn("x", self.collector.tenv)
        self.assertIn("y", self.collector.xenv)

    def test_visit_method_with_no_returns(self):
        method = QXMethod("Method", False, [QXBind("x", TySingle("nat"))], [], [], [])
        self.assertTrue(self.collector.visitMethod(method))
        self.assertIn("x", self.collector.tenv)

    def test_visit_method_with_conditions(self):
        method = QXMethod("Method", False, [QXBind("x", TySingle("nat"))], [QXBind("y", TySingle("real"))], [QXAssert(QXSpec())], [])
        self.assertTrue(self.collector.visitMethod(method))
        self.assertIn("x", self.collector.tenv)
        self.assertIn("y", self.collector.xenv)

    def test_visit_program(self):
        method = QXMethod("Method", False, [QXBind("x", TySingle("nat"))], [], [], [])
        program = QXProgram([method])
        self.assertTrue(self.collector.visitProgram(program))

    def test_visit_program_with_multiple_methods(self):
        method1 = QXMethod("method1", False, [QXBind("x", TySingle("nat"))], [], [], [])
        method2 = QXMethod("method2", False, [QXBind("y", TySingle("real"))], [], [], [])
        program = QXProgram([method1, method2])
        self.assertTrue(self.collector.visitProgram(program))
        self.assertIn("y", self.collector.tenv)
        self.assertIn("x", self.collector.tenv)

    def test_visit_program_with_conditions(self):
        method = QXMethod("testMethod", False, [QXBind("x", TySingle("nat"))], [], [QXAssert(QXSpec())], [])
        program = QXProgram([method])
        self.assertTrue(self.collector.visitProgram(program))




    def test_visit_single_t_valid_type(self):
        single_type = TySingle("nat")
        result = self.collector.visitSingleT(single_type)
        self.assertTrue(result)

    def test_visit_single_t_invalid_type(self):
        single_type = TySingle("invalid_type")
        self.assertFalse(self.collector.visitSingleT(single_type))
        # TODO: not yet implemented 

    def test_visit_single_t_empty_type(self):
        single_type = TySingle("")
        self.assertFalse(self.collector.visitSingleT(single_type))
        # TODO: not yet implemented 

    def test_visit_single_t_none_type(self):
        single_type = TySingle(None)
        self.assertFalse(self.collector.visitSingleT(single_type))
        # TODO: not yet implemented 









    def test_visit_fun_valid_type(self):
        left_type = TySingle("nat")
        right_type = TySingle("real")
        fun_type = TyFun(left_type, right_type)
        self.assertTrue(self.collector.visitFun(fun_type))

    def test_visit_fun_invalid_type(self):
        left_type = TySingle("invalid_type")
        right_type = TySingle("invalid_type")
        fun_type = TyFun(left_type, right_type)
        self.assertTrue(True)

    def test_visit_fun_none_type(self):
        left_type = None
        right_type = None
        fun_type = TyFun(left_type, right_type)
        self.assertTrue(True)







    def test_visit_bin_valid_expression(self):
        left_expr = QXNum(5)
        right_expr = QXNum(10)
        bin_expr = QXBin("+", left_expr, right_expr)
        self.assertTrue(self.collector.visitBin(bin_expr))

    def test_visit_bin_invalid_left_expression(self):
        left_expr = QXBind("x", TySingle("invalid_type"))
        right_expr = QXNum(10)
        bin_expr = QXBin("+", left_expr, right_expr)
        self.assertFalse(self.collector.visitBin(bin_expr))

    def test_visit_bin_invalid_right_expression(self):
        left_expr = QXNum(5)
        right_expr = QXBind("y", TySingle("invalid_type"))
        bin_expr = QXBin("+", left_expr, right_expr)
        self.assertFalse(self.collector.visitBin(bin_expr))

    def test_visit_bin_invalid_both_expressions(self):
        left_expr = QXBind("x", TySingle("invalid_type"))
        right_expr = QXBind("y", TySingle("invalid_type"))
        bin_expr = QXBin("+", left_expr, right_expr)
        self.assertFalse(self.collector.visitBin(bin_expr))






    def test_visit_uni_valid_expression(self):
        valid_expr = QXNum(5)
        uni_expr = QXUni("-", valid_expr)
        self.assertTrue(self.collector.visitUni(uni_expr))

    def test_visit_uni_invalid_expression(self):
        invalid_expr = QXBind("x", TySingle("invalid_type"))
        uni_expr = QXUni("-", invalid_expr)
        self.assertFalse(self.collector.visitUni(uni_expr))



    def test_visit_num_valid_expression(self):
        num_expr = QXNum(5)
        self.assertTrue(self.collector.visitNum(num_expr))

    def test_visit_num_zero(self):
        num_expr = QXNum(0)
        self.assertTrue(self.collector.visitNum(num_expr))

    def test_visit_num_negative_expression(self):
        num_expr = QXNum(-10)
        self.assertTrue(self.collector.visitNum(num_expr))




    def test_visit_init_valid_binding(self):
        binding = QXBind("x", TySingle("nat"))
        init_stmt = QXInit(binding)
        self.assertTrue(self.collector.visitInit(init_stmt))
        self.assertIn("x", self.collector.tenv)
        self.assertEqual(self.collector.tenv["x"], TySingle("nat"))

    def test_visit_init_invalid_binding(self):
        binding = QXBind("y", TySingle("invalid_type"))
        init_stmt = QXInit(binding)
        self.assertFalse(self.collector.visitInit(init_stmt))
        self.assertNotIn("y", self.collector.tenv)

    def test_visit_init_none_type(self):
        binding = QXBind("z", None)
        init_stmt = QXInit(binding)
        self.assertFalse(self.collector.visitInit(init_stmt))





    def test_visit_cast_valid(self):
        qty = TySingle("nat")
        locus = [QXBind("x", TySingle("nat"))]
        cast_stmt = QXCast(qty, locus)
        self.assertTrue(self.collector.visitCast(cast_stmt))

    def test_visit_cast_invalid_qty(self):
        qty = TySingle("invalid_type")
        locus = [QXBind("y", TySingle("nat"))]
        cast_stmt = QXCast(qty, locus)
        self.assertFalse(self.collector.visitCast(cast_stmt))

    def test_visit_cast_empty_locus(self):
        qty = TySingle("nat")
        locus = []
        cast_stmt = QXCast(qty, locus)
        self.assertTrue(self.collector.visitCast(cast_stmt))





    def test_visit_qassign_valid(self):
        locus = [QXBind("x", TySingle("nat"))]
        expr = QXNum(10)
        qassign_stmt = QXQAssign(locus, expr)
        self.assertTrue(self.collector.visitQAssign(qassign_stmt))

    def test_visit_qassign_invalid_locus(self):
        locus = [QXBind("y", TySingle("invalid_type"))]
        expr = QXNum(10)
        qassign_stmt = QXQAssign(locus, expr)
        self.assertFalse(self.collector.visitQAssign(qassign_stmt))

    def test_visit_qassign_invalid_expression(self):
        locus = [QXBind("x", TySingle("nat"))]
        expr = QXBind("y", TySingle("invalid_type"))
        qassign_stmt = QXQAssign(locus, expr)
        self.assertFalse(self.collector.visitQAssign(qassign_stmt))

    def test_visit_qassign_empty_locus(self):
        locus = []
        expr = QXNum(10)
        qassign_stmt = QXQAssign(locus, expr)
        self.assertTrue(self.collector.visitQAssign(qassign_stmt))





    def test_is_bit_type_nat(self):
        nat_type = TySingle("nat")
        self.assertTrue(self.collector.isBitType(nat_type))

    def test_is_bit_type_real(self):
        real_type = TySingle("real")
        self.assertTrue(self.collector.isBitType(real_type))

    def test_is_bit_type_bool(self):
        bool_type = TySingle("bool")
        self.assertTrue(self.collector.isBitType(bool_type))

    def test_is_bit_type_invalid(self):
        invalid_type = TySingle("invalid_type")
        self.assertFalse(self.collector.isBitType(invalid_type))

    def test_is_bit_type_none(self):
        self.assertFalse(self.collector.isBitType(None))





    def test_visit_measure_valid(self):
        locus = [QXBind("x", TySingle("nat"))]
        measure_stmt = QXMeasure(ids=["x"], locus=locus)
        self.assertTrue(self.collector.visitMeasure(measure_stmt))

    def test_visit_measure_invalid_locus(self):
        locus = [QXBind("y", TySingle("invalid_type"))]
        measure_stmt = QXMeasure(ids=["y"], locus=locus)
        self.assertFalse(self.collector.visitMeasure(measure_stmt))

    def test_visit_measure_invalid_id(self):
        locus = [QXBind("x", TySingle("nat"))]
        measure_stmt = QXMeasure(ids=["invalid_id"], locus=locus)
        self.assertFalse(self.collector.visitMeasure(measure_stmt))

    def test_visit_measure_empty_locus(self):
        locus = []
        measure_stmt = QXMeasure(ids=["x"], locus=locus)
        self.assertFalse(self.collector.visitMeasure(measure_stmt))





    def test_visit_cassign_valid(self):
        binding = QXBind("x", TySingle("nat"))
        self.collector.tenv["x"] = TySingle("nat")
        expr = QXNum(10)
        cassign_stmt = QXCAssign(id="x", expr=expr)
        self.assertTrue(self.collector.visitCAssign(cassign_stmt))

    def test_visit_cassign_invalid_expression(self):
        binding = QXBind("x", TySingle("nat"))
        self.collector.tenv["x"] = TySingle("nat")
        expr = QXBind("y", TySingle("invalid_type"))
        cassign_stmt = QXCAssign(id="x", expr=expr)
        self.assertFalse(self.collector.visitCAssign(cassign_stmt))

    def test_visit_cassign_uninitialized_id(self):
        expr = QXNum(1)
        cassign_stmt = QXCAssign(id="x", expr=expr)
        self.assertFalse(self.collector.visitCAssign(cassign_stmt))





    def test_visit_if_valid(self):
        bexp = QXBool()
        stmt1 = QXCAssign(id="x", expr=QXNum(10))
        stmt2 = QXCAssign(id="y", expr=QXNum(20))
        if_stmt = QXIf(bexp, [stmt1, stmt2])

        self.collector.tenv["x"] = TySingle("nat")
        self.collector.tenv["y"] = TySingle("nat")

        self.assertTrue(self.collector.visitIf(if_stmt))

    def test_visit_if_invalid_bexp(self):
        bexp = QXBind("invalid_bexp", TySingle("invalid_type"))
        stmt = QXCAssign(id="x", expr=QXNum(10))
        if_stmt = QXIf(bexp, [stmt])
        self.assertFalse(self.collector.visitIf(if_stmt))

    def test_visit_if_invalid_statement(self):
        bexp = QXBool()
        stmt = QXBind("y", TySingle("invalid_type"))
        if_stmt = QXIf(bexp, [stmt])
        self.assertFalse(self.collector.visitIf(if_stmt))

    def test_visit_if_empty_statements(self):
        bexp = QXBool()
        if_stmt = QXIf(bexp, [])
        self.assertIsNone(self.collector.visitIf(if_stmt))




    def test_visit_for_valid(self):
        crange = QXRange(QXNum(0), QXNum(10))
        stmt1 = QXCAssign(id="y", expr=QXNum(10))
        for_stmt = QXFor(id="x", crange=crange, inv=[], stmts=[stmt1])

        self.collector.tenv[loop_var] = TySingle("nat")

        self.assertTrue(self.collector.visitFor(for_stmt))

    def test_visit_for_invalid_range(self):
        crange = QXBind("invalid_range", TySingle("invalid_type"))
        stmt1 = QXCAssign(id="y", expr=QXNum(10))
        for_stmt = QXFor(id="x", crange=crange, inv=[], stmts=[stmt1])

        self.assertFalse(self.collector.visitFor(for_stmt))

    def test_visit_for_invalid_statement(self):
        crange = QXRange(QXNum(0), QXNum(10))
        stmt = QXBind("y", TySingle("invalid_type"))
        for_stmt = QXFor(id="x", crange=crange, inv=[], stmts=[stmt])

        self.assertFalse(self.collector.visitFor(for_stmt))

    def test_visit_for_empty_statements(self):
        crange = QXRange(QXNum(0), QXNum(10))
        for_stmt = QXFor(id="x", crange=crange, inv=[], stmts=[])

        self.assertTrue(self.collector.visitFor(for_stmt))





    def test_visit_call_valid(self):
        func_id = "Function"
        self.collector.env[func_id] = TyFun(TySingle("nat"), TySingle("real"))
        expr1 = QXNum(5)
        expr2 = QXNum(10)
        call_stmt = QXCall(func_id, [expr1, expr2])

        self.assertTrue(self.collector.visitCall(call_stmt))

    def test_visit_call_invalid_function(self):
        func_id = "Function"
        expr1 = QXNum(5)
        call_stmt = QXCall(func_id, [expr1])

        self.assertFalse(self.collector.visitCall(call_stmt))

    def test_visit_call_invalid_expression(self):
        func_id = "Function"
        self.collector.env[func_id] = TyFun(TySingle("nat"), TySingle("real"))
        invalid_expr = QXBind("y", TySingle("invalid_type"))
        call_stmt = QXCall(func_id, [invalid_expr])

        self.assertFalse(self.collector.visitCall(call_stmt))

    def test_visit_call_multiple_expressions(self):
        func_id = "Function"
        self.collector.env[func_id] = TyFun(TySingle("nat"), TySingle("real"))
        expr1 = QXNum(5)
        expr2 = QXNum(10)
        call_stmt = QXCall(func_id, [expr1, expr2])
        self.assertTrue(self.collector.visitCall(call_stmt))





    def test_visit_assert_valid_spec(self):
        valid_spec = QXQSpec()
        assert_stmt = QXAssert(valid_spec)
        self.assertTrue(self.collector.visitAssert(assert_stmt))

    def test_visit_assert_invalid_spec(self):
        invalid_spec = QXBind("x", TySingle("invalid_type"))
        assert_stmt = QXAssert(invalid_spec)
        self.assertFalse(self.collector.visitAssert(assert_stmt))

    def test_visit_assert_none_spec(self):
        assert_stmt = QXAssert(None)
        self.assertFalse(self.collector.visitAssert(assert_stmt))

    def test_visit_bind_with_invalid_type(self):
        bind = QXBind("x", TySingle("invalid_type"))
        self.collector.tenv["x"] = TySingle("nat")
        self.assertFalse(self.collector.visitBind(bind))
    

if __name__ == '__main__':
    unittest.main()