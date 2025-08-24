import TargetProgrammer
from TargetProgrammer import *
from TargetProgramVisitor import TargetProgramVisitor

class BindCollector(TargetProgramVisitor):
    """
    A visitor that traverses a Dafny AST and collects all unique variable
    bindings (DXBind nodes) it encounters.
    """
    def __init__(self):
        """Initializes the collector with an empty set to store variable IDs."""
        self.renv = set()

    def visitBind(self, ctx: TargetProgrammer.DXBind):
        """
        Visits a DXBind node, adds its ID to the set, and continues traversal.
        """
        if ctx.ID():
            self.renv.add(ctx.ID())
        
        # Continue traversal to visit the type information within the binding, if any.
        super().visitBind(ctx)