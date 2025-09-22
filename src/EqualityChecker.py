from TargetProgramVisitor import TargetProgramVisitor
from TargetProgrammer import *

class EqualityVisitor(TargetProgramVisitor):
    """
    A visitor to check for structural equality between two Dafny AST nodes.
    It returns True if the two ASTs are identical, otherwise False.
    """
    def visit(self, ctx1, ctx2):
        """
        The main dispatch method. Checks if the types of the two nodes are the same
        before calling the more specific visit method.
        """
        if type(ctx1) != type(ctx2):
            return False
        # The super().visit() will call the appropriate visit method for the given type.
        # We need to pass both contexts to the specific visit methods.
        method_name = 'visit' + type(ctx1).__name__
        method = getattr(self, method_name, None)
        if method:
            return method(ctx1, ctx2)
        return True # Assume equality if no specific visitor is defined
    def visitlist(self, ctx1: list, ctx2: list):
        """
        Compares two lists of AST nodes for structural equality.
        """
        if len(ctx1) != len(ctx2):
            return False
        return all(self.visit(e1, e2) for e1, e2 in zip(ctx1, ctx2))

    def visitBin(self, ctx1: DXBin, ctx2: DXBin):
        return ctx1.op() == ctx2.op() and self.visit(ctx1.left(), ctx2.left()) and self.visit(ctx1.right(), ctx2.right())

    def visitUni(self, ctx1: DXUni, ctx2: DXUni):
        return ctx1.op() == ctx2.op() and self.visit(ctx1.next(), ctx2.next())

    def visitBind(self, ctx1: DXBind, ctx2: DXBind):
        # Note: This does not compare types for simplicity. A more robust
        # version would recursively check type equality as well.
        return ctx1.ID() == ctx2.ID() and ctx1.num() == ctx2.num()

    def visitNum(self, ctx1: DXNum, ctx2: DXNum):
        return ctx1.val() == ctx2.val()

    def visitComp(self, ctx1: DXComp, ctx2: DXComp):
        return ctx1.op() == ctx2.op() and self.visit(ctx1.left(), ctx2.left()) and self.visit(ctx1.right(), ctx2.right())
        
    def visitAll(self, ctx1: DXAll, ctx2: DXAll):
        return self.visit(ctx1.bind(), ctx2.bind()) and self.visit(ctx1.next(), ctx2.next())

    def visitLength(self, ctx1: DXLength, ctx2: DXLength):
        return self.visit(ctx1.var(), ctx2.var())

    def visitIndex(self, ctx1: DXIndex, ctx2: DXIndex):
        return self.visit(ctx1.bind(), ctx2.bind()) and self.visit(ctx1.index(), ctx2.index())

    def visitCall(self, ctx1: DXCall, ctx2: DXCall):
        if ctx1.ID() != ctx2.ID() or len(ctx1.exps()) != len(ctx2.exps()):
            return False
        return all(self.visit(e1, e2) for e1, e2 in zip(ctx1.exps(), ctx2.exps()))

    def visitLogic(self, ctx1: DXLogic, ctx2: DXLogic):
        return ctx1.op() == ctx2.op() and self.visit(ctx1.left(), ctx2.left()) and self.visit(ctx1.right(), ctx2.right())

