from ProgramVisitor import ProgramVisitor
from Programmer import *

class QafnyPP(ProgramVisitor):
    """Visitor for pretty printing quantum states and types"""
    
    def visitProgram(self, ctx: QXProgram):
        program = ''
        for method in ctx.topLevelStmts():
            program += method.accept(self) + "\n\n"
        return program
    
    def visitMethod(self, ctx: QXMethod):
        method = f"method {ctx.ID()}("
        
        # Process bindings
        bindings = []
        for binding in ctx.bindings():
            bindings.append(binding.accept(self))
        method += ', '.join(bindings)

        # Process return types if they exist
        if ctx.returns():
            returns = []
            for ret in ctx.returns():
                returns.append(ret.accept(self))
            method += ') returns (' + ', '.join(returns)
        method += ')\n'
        
        # Process requires/ensures
        for cond in ctx.conds():
            if isinstance(cond, QXRequires):
    #            print('\nrequires', cond, '\n')
                method += f"  requires {cond.accept(self)}\n"
            elif isinstance(cond, QXEnsures):
                method += f"  ensures {cond.accept(self)}\n"
    #            print('\nensures', cond, '\n')
        
        # Handle axiom case
        if ctx.axiom():
            method = f"method {'':axiom} {ctx.ID()}(" + ', '.join(bindings)
            if ctx.returns():
                method += ') returns (' + ', '.join(returns)
            method += ')\n'
            return method
        
        # Process method body
        body = []
        for stmt in ctx.stmts():
            body.append(f"  {stmt.accept(self)}")
 #       print(method + "{\n" + '\n'.join(body) + "\n}")
        return method + "{\n" + '\n'.join(body) + "\n}"

    
    def visitQAssign(self, ctx: QXQAssign):
        loci = [locus.accept(self) for locus in ctx.locus()]
        exp = ctx.exp().accept(self)
        return f"{', '.join(loci)} *= {exp};"
    
    def visitQSpec(self, ctx: QXQSpec):
        qty = ctx.qty().accept(self) if ctx.qty() else ""
        loci = [locus.accept(self) for locus in ctx.locus()]
        if isinstance(ctx.states(), list):
            state = ' '.join(st.accept(self) for st in ctx.states())
        else:
            state = ctx.states().accept(self)
        return f"{', '.join(loci)} : {qty} ↦ {state}"
    
    def visitCall(self, ctx: QXCall):
        func_name = ctx.ID()
        if ctx.exps():
            args = [arg.accept(self) for arg in ctx.exps()]
        else:
            args = []
        if  ctx.ID() == 'omega' and ctx.exps():
            return f" ω({', '.join(args)})"
        return f"{', '.join(args)}"
    
    def visitBind(self, ctx: QXBind):
        type_str = ""
        if ctx.type() is not None:
            type_str = ctx.type().accept(self)
            return f"{ctx.ID()}: {type_str}"
    #    print(f"Binding without type: {ctx.ID()}")
        return f"{ctx.ID()}"
    
    def visitQ(self, ctx: TyQ):
        return f"Q[{ctx.flag().accept(self)}]"
    
    def visitNor(self, ctx: TyNor):
        return "nor"
    
    def visitTyHad(self, ctx: TyHad):
        return "had"
    
    def visitEn(self, ctx: TyEn):
        return f"en({ctx.flag().accept(self)})"
    
    # def visitSums(self, ctx: QXSums):
    #     qxsums = []

    #     for sum in ctx.qxsums():
    #         sumspec = sum.accept(self) if hasattr(sum, 'accept') else ""
    #         qxsums.append(sumspec)
    #     return ' + '.join(qxsums)
    
    def visitSum(self, ctx: QXSum):
        # Handle summation variables and ranges
        sums = []
        
        if ctx.sums():
            for sum_elem in ctx.sums():
        #    print(sum_elem)
                sums.append(f"∑ {sum_elem.accept(self)}")
                
        # Handle amplitude
        # amps = []
        # for elem in ctx.amp():
        #     amps.append(f"{elem.accept(self)}")   
        # Handle kets
        amp = ctx.amp().accept(self)
        kets = []
        for ket in ctx.kets():
            state = ket.accept(self)
            kets.append(f"|{state}⟩")

        # Join summations and state with proper spacing
        return f"{''.join(sums)} {'*'.join(amp)}{' '.join(kets)}"
    
    def visitTensor(self, ctx: QXTensor):
        """Format tensor product of quantum states"""
        # Process each ket in the tensor product
        kets = []
        for ket in ctx.kets():
            state = ket.accept(self)
            kets.append(f"|{state}⟩")
        
        # Join kets with tensor product symbol ⊗
        return " ⊗ ".join(kets)
    
    def visitSKet(self, ctx: QXSKet):
        return ctx.vector().accept(self)
    
    def visitVKet(self, ctx: QXVKet):
        return ctx.vector().accept(self)
    
    def visitNum(self, ctx: QXNum):
        return str(ctx.num())
    
    def visitHad(self, ctx: QXHad):
        return str(ctx.state())
    
    def visitQRange(self, ctx: QXQRange):
        left = ctx.crange().left().accept(self)
        right = ctx.crange().right().accept(self)
        return f"{ctx.location()}[{left}, {right})"
    
    def visitCon(self, ctx: QXCon):
        left = ctx.range().left().accept(self)
        right = ctx.range().right().accept(self)
        return f"{ctx.ID()} ∈ [{left}, {right}) . "
    
    def visitBin(self, ctx: QXBin):
        left  = ctx.left().accept(self)      
        right = ctx.right().accept(self)
        return f"({left}{ctx.op()}{right})"
    
    def visitBool(self, ctx:QXComp):
    # Handle raw integers on either side
        left = ctx.left().accept(self) if hasattr(ctx.left(), 'accept') else str(ctx.left())
        right = ctx.right().accept(self) if hasattr(ctx.right(), 'accept') else str(ctx.right())
        return f"({left}{ctx.op()} {right})"
    
    def visitUni(self, ctx: QXUni):
    #    print('uni', ctx)
        next = ctx.next().accept(self)
    #    print(next,'next')
        if ctx.op() == 'abs':
            return f"{next}"
        return f"{ctx.op()}{next}"