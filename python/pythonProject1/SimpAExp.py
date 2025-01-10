import Programmer
from ProgramVisitor import ProgramVisitor
from Programmer import *
from TypeChecker import *


class SimpAExp(ProgramVisitor):


    def visitQSpec(self, ctx: Programmer.QXQSpec):

        if isinstance(ctx.state(), QXTensor):
            ks = ctx.state()
            vs = zip(ctx.locus(), ks.kets())
            tmploc = []
            tmpkv = []
            for elem,kv in vs:
                v = elem.accept(self)
                k1 = kv.accept(self)
                if not compareAExp(v.left(), v.right()):
                    tmploc += [v]
                    tmpkv += [k1]
            ran = None
            if ks.range() is not None:
                ran = ks.range().accept(self)
            amp = None
            if ks.amp() is not None:
                amp = ks.amp().accept(self)
            return QXQSpec(tmploc, ctx.qty(), QXTensor(tmpkv, ks.ID(), ran, amp))

        if isinstance(ctx.state(), QXSum):
            ks = ctx.state()
            vs = zip(ctx.locus(), ks.kets())
            tmploc = []
            tmpkv = []
            for elem, kv in vs:
                v = elem.accept(self)
                k1 = kv.accept(self)
                if not compareAExp(v.left(), v.right()):
                    tmploc += [v]
                    tmpkv += [k1]
            sums = []
            for sum in ks.sums():
                sums += [sum.accept(self)]
            return QXQSpec(tmploc, ctx.qty(), QXSum(sums, ks.amp().accept(self), tmpkv))

        loc = []
        for elem in ctx.locus():
            loc += [elem.accept(self)]
        v = ctx.state().accept(self)

        return QXQSpec(loc, ctx.qty(), v)


    def visitHad(self, ctx: Programmer.QXHad):
        return ctx


    def visitSKet(self, ctx: Programmer.QXSKet):
        return QXSKet(ctx.vector().accept(self))


    def visitVKet(self, ctx: Programmer.QXVKet):
        return QXVKet(ctx.vector().accept(self))


    def visitQRange(self, ctx: Programmer.QXQRange):
        v = ctx.crange().accept(self)
        return QXQRange(ctx.ID(), v)


    def visitCRange(self, ctx: Programmer.QXCRange):
        l = ctx.left().accept(self)
        r = ctx.right().accept(self)
        return QXCRange(l, r)


    def visitTensor(self, ctx: Programmer.QXTensor):
        ks = []
        for elem in ctx.kets():
            ks += [elem.accept(self)]
        ran = None
        if ctx.range() is not None:
            ran = ctx.range().accept(self)
        amp = None
        if ctx.amp() is not None:
            amp = ctx.amp().accept(self)
        return QXTensor(ks, ctx.ID(), ran, amp)


    def visitSum(self, ctx: Programmer.QXSum):
        sums = []
        for elem in ctx.sums():
            sums += [elem.accept(self)]

        ks = []
        for kelem in ctx.kets():
            ks += [kelem.accept(self)]

        return QXSum(sums, ctx.amp().accept(self), ks)


    def visitPart(self, ctx: Programmer.QXPart):
        qnum = ctx.qnum().accept(self)
        fname = ctx.fname().accept(self)
        tamp = ctx.trueAmp().accept(self)
        famp = ctx.falseAmp().accept(self)
        return QXPart(qnum, fname, tamp, famp)


    def visitCon(self, ctx: Programmer.QXCon):
        v = ctx.range().accept(self)
        return QXCon(ctx.ID(), v)


    def visitBind(self, ctx: Programmer.QXBind):
        return ctx


    def visitNum(self, ctx: Programmer.QXNum):
        return ctx


    def visitBin(self, ctx: Programmer.QXBin):
        v1 = ctx.left().accept(self)
        v2 = ctx.right().accept(self)
        if isinstance(v1, QXNum) and isinstance(v2, QXNum):
            if ctx.op() == '+':
                return QXNum(v1.num()+v2.num())
            if ctx.op() == '-':
                return QXNum(v1.num()-v2.num())
            if ctx.op() == '*':
                return QXNum(v1.num() * v2.num())
            if ctx.op() == '/':
                return QXNum(v1.num() // v2.num())
            if ctx.op() == '%':
                return QXNum(v1.num() % v2.num())
            if ctx.op() == '^':
                return QXNum(v1.num() ** v2.num())

        if isinstance(v2, QXNum):
            if v2.num() == 0:
                if ctx.op() == '+' or ctx.op() == '-' or ctx.op() == '^':
                    return v1
                if ctx.op() == '*':
                    return QXNum(0)

            if isinstance(v1, QXBin):
                if isinstance(v1.right(), QXNum):
                    if ctx.op() == '+':
                        if v1.op() == '+':
                            return QXBin('+', v1.left(), QXNum(v2.num()+v1.right().num()))
                        if v1.op() == '-':
                            if v2.num() < v1.right().num():
                                return QXBin('-', v1.left(), QXNum(v1.right().num()-v2.num()))
                            else:
                                return QXBin('+', v1.left(), QXNum(v2.num()-v1.right().num()))
                    if ctx.op() == '-':
                        if v1.op() == '+':
                            if v2.num() < v1.right().num():
                                return QXBin('+', v1.left(), QXNum(v1.right().num()-v2.num()))
                            else:
                                return QXBin('-', v1.left(), QXNum(v2.num()-v1.right().num()))
                        if v1.op() == '-':
                            return QXBin('-', v1.left(), QXNum(v2.num()+v1.right().num()))

        if isinstance(v1, QXNum):
            if v1.num() == 0:
                if ctx.op() == '+' or ctx.op() == '-':
                    return v2
                if ctx.op() == '*' or ctx.op() == '/' or ctx.op() == '%':
                    return QXNum(0)
                if ctx.op() == '^':
                    return QXNum(1)

        if isinstance(v2, QXBin):
            if isinstance(v2.left(), QXNum):
                if ctx.op() == '+':
                    if v2.op() == '+':
                        return QXBin('+', QXNum(v1.num() + v2.right().num()), v2.right())
                    if v2.op() == '-':
                        if v1.num() < v2.right().num():
                            return QXBin('-', QXNum(v2.right().num() - v1.num()),v2.right())
                        else:
                            return QXBin('+', QXNum(v1.num() - v2.right().num()), v2.left())
                if ctx.op() == '-':
                    if v2.op() == '+':
                        if v1.num() < v2.right().num():
                            return QXBin('+', QXNum(v2.right().num() - v1.num()), v2.left())
                        else:
                            return QXBin('-', QXNum(v1.num() - v2.right().num()),v2.left())
                    if v2.op() == '-':
                        return QXBin('-', QXNum(v1.num() + v2.right().num()),v2.left())

        return QXBin(ctx.op(), v1, v2)

    def visitUni(self, ctx: Programmer.QXUni):
        v1 = ctx.next().accept(self)
        if isinstance(v1, QXNum):
            if ctx.op() == '+':
                return QXNum(v1.num())
            if ctx.op() == '-':
                return QXNum(-v1.num())
            if ctx.op() == 'abs':
                return QXNum(abs(v1.num()))
        return QXUni(ctx.op(), v1)

    def visitQIndex(self, ctx: Programmer.QXQIndex):
        return QXQIndex(ctx.ID(), ctx.index().accept(self))
