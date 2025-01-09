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


    def test_visit_method(self):
        method = QXMethod("testMethod", False, [QXBind("x", TySingle("nat"))], [QXBind("y", TySingle("real"))], [], [])
        self.assertTrue(self.collector.visitMethod(method))
        self.assertIn("x", self.collector.tenv)
        self.assertIn("y", self.collector.xenv)


if __name__ == '__main__':
    unittest.main()