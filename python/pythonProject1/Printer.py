from TargetProgrammer import *
from PrinterVisitor import *

def method1():
    inp = DXBind('x', SType('seq<int>'))
    out = DXBind('y', SType('seq<int>'))
    i = DXBind('i', SType('int'))
    zero = DXBind('0', SType('int'))
    s2 = DXInit(i, zero)
    one = DXBind('1', SType('int'))
    ls1 = DXAssign([out], DXBin('+', out, DXIndex(DXBind(''), DXBin('+',DXIndex(inp, i), one))))
    ls2 = DXAssign([i], DXBin('+', i, one))
    s1 = DXAssign([out], DXBind('[]', SType('array')))

    cond = DXComp('<', i, DXLength(inp))


    k = DXBind('k', SType('int'))
    respec = DXAll(k, DXComp('==>', DXInRange(k, zero, DXLength(inp)), DXComp('==', DXIndex(inp, k), zero)))
    enspec = DXAll(k, DXComp('==>', DXInRange(k, zero, DXLength(out)), DXComp('==', DXIndex(out, k), one)))


    req = DXRequires(respec)
    en = DXEnsures(enspec)

    inv1 = DXComp('==', DXLength(out), i)
    inv2 = DXAll(k, DXComp('<=', zero, DXComp('<', k, DXComp("==>", i, DXComp('==', DXIndex(out, k), one)))))
    inv2 = DXAll(k, DXComp("==>", DXInRange(k, zero, i), DXComp('==', DXIndex(out, k), one)))
    loop = DXWhile(cond, [ls1, ls2], [inv1, inv2])

    method1 = DXMethod('test', False, [DXBind('x', SType('seq<int>'))], [DXBind('y', SType('seq<int>'))], [req, en], [s1, s2, loop])
    return method1


def method2():
    inp1 = DXBind('k', SType('real'))
    inp2 = DXBind('n', SType('nat'))

    out = DXBind('x', SType('seq<real>'))

    zero = DXBind('0', SType('int'))
    j = DXBind('j', SType('int'))
    enspec1 = DXBin('==', DXLength(out), inp2)
    enspec2 = DXAll(j, DXComp('==>', DXInRange(j, zero, inp2), DXComp('==', DXIndex(out, j), inp1)))
    ensures1 = DXEnsures(enspec1)
    ensures2 = DXEnsures(enspec2)

    i = DXBind('i', SType('int'))
    zero = DXBind('0', SType('int'))
    one = DXBind('1', SType('int'))
    s1 = DXAssign([out], DXBind('[]'))
    s2 = DXInit(i, zero)

    loop_cond = DXComp('<', i, inp2)
    inv1 = DXComp('<=', zero, DXComp('<=', i, inp2))
    inv2 = DXComp('==', DXLength(out), i)
    inv3 = DXAll(j, DXComp('==>', DXInRange(j, zero, i), DXComp('==', DXIndex(out, j), inp1)))
    ls1 = DXAssign([out], DXBin('+', out, DXIndex(DXBind(''), inp1)))
    ls2 = DXAssign([i], DXBin('+', i, one))
    loop = DXWhile(loop_cond, [ls1, ls2], [inv1, inv2, inv3])

    method2 = DXMethod('copy', False, [DXBind(inp1.ID(), inp1.type()), DXBind(inp2.ID(), inp2.type())], [DXBind(out.ID(), out.type())], [ensures1, ensures2], [s1, s2, loop])
    return method2

program = DXProgram([method1(), method2()])


print(program.accept(PrinterVisitor()))

