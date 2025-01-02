from TargetProgrammer import *
from PrinterVisitor import *

def method1():
    inp = DXVar('x', SType('seq<int>'))
    out = DXVar('y', SType('seq<int>'))
    i = DXVar('i', SType('int'))
    zero = DXVar('0', SType('int'))
    s2 = DXInit(i, zero)
    one = DXVar('1', SType('int'))
    ls1 = DXAssign([out], DXBin('+', out, DXIndex(DXVar(''), DXBin('+',DXIndex(inp, i), one))))
    ls2 = DXAssign([i], DXBin('+', i, one))
    s1 = DXAssign([out], DXVar('[]', SType('array')))

    cond = DXComp('<', i, DXLength(inp))


    k = DXVar('k', SType('int'))
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
    inp1 = DXVar('k', SType('real'))
    inp2 = DXVar('n', SType('nat'))

    out = DXVar('x', SType('seq<real>'))

    zero = DXVar('0', SType('int'))
    j = DXVar('j', SType('int'))
    enspec1 = DXBin('==', DXLength(out), inp2)
    enspec2 = DXAll(j, DXComp('==>', DXInRange(j, zero, inp2), DXComp('==', DXIndex(out, j), inp1)))
    ensures1 = DXEnsures(enspec1)
    ensures2 = DXEnsures(enspec2)

    i = DXVar('i', SType('int'))
    zero = DXVar('0', SType('int'))
    one = DXVar('1', SType('int'))
    s1 = DXAssign([out], DXVar('[]'))
    s2 = DXInit(i, zero)

    loop_cond = DXComp('<', i, inp2)
    inv1 = DXComp('<=', zero, DXComp('<=', i, inp2))
    inv2 = DXComp('==', DXLength(out), i)
    inv3 = DXAll(j, DXComp('==>', DXInRange(j, zero, i), DXComp('==', DXIndex(out, j), inp1)))
    ls1 = DXAssign([out], DXBin('+', out, DXIndex(DXVar(''), inp1)))
    ls2 = DXAssign([i], DXBin('+', i, one))
    loop = DXWhile(loop_cond, [ls1, ls2], [inv1, inv2, inv3])

    method2 = DXMethod('copy', False, [DXBind(inp1.ID(), inp1.type()), DXBind(inp2.ID(), inp2.type())], [DXBind(out.ID(), out.type())], [ensures1, ensures2], [s1, s2, loop])
    return method2

program = DXProgram([method1(), method2()])


print(program.accept(PrinterVisitor()))

