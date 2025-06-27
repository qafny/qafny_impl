# add the parent directory (root python directory) to the path
import sys
import os

script_dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(script_dir)
sys.path.append("../")

# regular imports
import rich
from rich.pretty import pprint, Pretty
from rich.table import Table
from rich import box

import inspect

import suite
from suite import TestSuite

from antlr4 import FileStream, CommonTokenStream
from ExpLexer import ExpLexer
from ExpParser import ExpParser
from ProgramTransformer import ProgramTransformer

from Programmer import *

from typing import (
    Any,
    Union,
    Iterable,
    Tuple
)

import utils

# An array containing the expected results of transforming each case
# These were all hand-checked but generated from ProgramTransformer.py
TRANSFORMED_CASES = [
    # 1- 10
    QXProgram([QXMethod('hadtest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyHad(), [QXTensor([QXSKet(QXHad('+'))], id='i', crange=QXCRange(QXNum(0), QXBind('n')))]))], [QXAssert(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXQAssign([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], QXSingle('H')), QXAssert(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyHad(), [QXTensor([QXSKet(QXHad('+'))], id='i', crange=QXCRange(QXNum(0), QXBind('n')))]))])]),
    QXProgram([QXMethod('hadtest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k'))])]))], [QXQAssign([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], QXSingle('H')), QXCast(TyEn(QXNum(1)), [QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))])])]),
    QXProgram([QXMethod('hadtest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXSum([QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('*', QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), QXCall('omega', [QXBin('*', QXBind('j'), QXBind('k')), QXNum(2)])), [QXSKet(QXBind('j')), QXSKet(QXBind('k'))])]))], [QXQAssign([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], QXSingle('H'))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('a', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXComp('>', QXBind('N'), QXNum(0))), QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('*', QXBind('a'), QXBind('k')), QXBind('N')))])]))], [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('*', QXBind('a'), QXBind('x')), QXBind('N')))]))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('k', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyNor(), [QXTensor([QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyNor(), [QXTensor([QXSKet(QXBin('%', QXBin('+', QXBind('k'), QXNum(1)), QXBin('^', QXNum(2), QXBind('n'))))])]))], [QXQAssign([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1)), QXBin('^', QXNum(2), QXBind('n'))))]))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('k', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXSum([QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), [QXSKet(QXBind('j')), QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXSum([QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('*', QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), QXCall('omega', [QXBind('k'), QXBin('^', QXNum(2), QXBind('n'))])), [QXSKet(QXBind('j')), QXSKet(QXBind('k'))])]))], [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXOracle([QXBind('x')], QXCall('omega', [QXBind('x'), QXBin('^', QXNum(2), QXBind('n'))]), [QXSKet(QXBind('x'))]))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('k', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBin('%', QXBin('+', QXBind('j'), QXNum(1)), QXBin('^', QXNum(2), QXBind('n'))))])]))], [QXQAssign([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], QXSingle('H')), QXQAssign([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1)), QXBin('^', QXNum(2), QXBind('n'))))]))])]),
    QXProgram([QXMethod('conditionaltest1', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('i', type=TySingle('nat')), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXComp('<=', QXNum(0), QXComp('<', QXBind('i'), QXBind('n')))), QXRequires(QXComp('>', QXBind('N'), QXNum(0))), QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('i'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('i'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('i')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])])), QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXBind('i'), QXBind('n')))], TyHad(), [QXTensor([QXSKet(QXHad('+'))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXBin('+', QXBind('i'), QXNum(1)), QXBind('n')))], TyHad(), [QXTensor([QXSKet(QXHad('+'))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBin('+', QXBind('i'), QXNum(1)))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBin('+', QXBind('i'), QXNum(1)))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBin('+', QXBind('i'), QXNum(1))))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])]))], [QXIf(QXQRange('q', crange=QXCRange(QXBind('i'), QXBin('+', QXBind('i'), QXNum(1)))), [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('*', QXBin('^', QXBind('base'), QXBin('^', QXNum(2), QXBind('i'))), QXBind('x')), QXBind('N')))]))], None)])]),
    QXProgram([QXMethod('conditionaltest2', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXNum(1))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXNum(1)))], TyHad(), [QXTensor([QXSKet(QXHad('+'))])])), QXRequires(QXQSpec([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXNum(1))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXTensor([QXSKet(QXNum(0)), QXSKet(QXNum(0))], amp=QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2)))), QXSum([QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXBin('*', QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2))), QXNum(1)), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXNum(1)), QXSKet(QXBind('k'))])]))], [QXIf(QXQRange('q', crange=QXCRange(QXNum(0), QXNum(1))), [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXSingle('H'))], None)])]),
    QXProgram([QXMethod('conditionaltest3', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXNum(1)))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXNum(1)))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBind('k'), QXNum(2)))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXNum(1)))], TyEn(QXNum(2)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBind('k'), QXNum(2)))], condition=QXComp('==', QXBin('%', QXBind('k'), QXNum(2)), QXNum(0))), QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXNum(2)))], QXBin('*', QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBin('+', QXBind('n'), QXNum(1))))), QXCall('omega', [QXBin('*', QXBind('j'), QXBin('%', QXBind('k'), QXNum(2))), QXNum(2)])), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))], condition=QXComp('==', QXBin('%', QXBind('k'), QXNum(2)), QXNum(1)))]))], [QXIf(QXQRange('q', crange=QXCRange(QXNum(0), QXNum(1))), [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXNum(1)))], QXSingle('H'))], None)])]),

    # 11 - 20
    QXProgram([QXMethod('conditionaltest4', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('r', type=TyQ(QXNum(1))), QXBind('u', type=TyQ(QXNum(1))), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))])])), QXRequires(QXQSpec([QXQRange('r', crange=QXCRange(QXNum(0), QXNum(1)))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXRequires(QXQSpec([QXQRange('u', crange=QXCRange(QXNum(0), QXNum(1)))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('u', crange=QXCRange(QXNum(0), QXNum(1))), QXQRange('r', crange=QXCRange(QXNum(0), QXNum(1)))], TyEn(QXNum(3)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('i', QXCRange(QXNum(0), QXNum(2)))], QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j')), QXSKet(QXNum(1)), QXSKet(QXBind('i'))], condition=QXComp('<', QXBind('j'), QXComp('==', QXBind('N'), QXNum(1)))), QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j')), QXSKet(QXNum(0)), QXSKet(QXNum(0))], condition=QXComp('<', QXBind('j'), QXComp('==', QXBind('N'), QXNum(0))))]))], [QXIf(QXQComp('<', QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n'))), QXBind('N'), QXQIndex('u', QXNum(0))), [QXQAssign([QXQRange('r', crange=QXCRange(QXNum(0), QXNum(1)))], QXSingle('H'))], None)])]),
    QXProgram([QXMethod('conditionaltest5', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('r', type=TyQ(QXNum(1))), QXBind('u', type=TyQ(QXNum(1))), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))])])), QXRequires(QXQSpec([QXQRange('r', crange=QXCRange(QXNum(0), QXNum(1)))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXRequires(QXQSpec([QXQRange('u', crange=QXCRange(QXNum(0), QXNum(1)))], TyNor(), [QXTensor([QXSKet(QXNum(0))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('u', crange=QXCRange(QXNum(0), QXNum(1))), QXQRange('r', crange=QXCRange(QXNum(0), QXNum(1)))], TyEn(QXNum(3)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXBin('*', QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), QXNum(1)), QXUni('sqrt', QXNum(2))), [QXSKet(QXBind('k')), QXSKet(QXBin('+', QXBind('j'), QXNum(1))), QXSKet(QXNum(1)), QXSKet(QXNum(1))], condition=QXComp('<', QXBind('j'), QXComp('==', QXBind('N'), QXNum(1)))), QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXBin('*', QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), QXNum(1)), QXUni('sqrt', QXNum(2))), [QXSKet(QXBind('k')), QXSKet(QXBind('j')), QXSKet(QXNum(1)), QXSKet(QXNum(0))], condition=QXComp('<', QXBind('j'), QXComp('==', QXBind('N'), QXNum(0)))), QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j')), QXSKet(QXNum(0)), QXSKet(QXNum(0))])]))], [QXIf(QXQComp('<', QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXBind('N'), QXQIndex('u', QXNum(0))), [QXQAssign([QXQRange('r', crange=QXCRange(QXNum(0), QXNum(1)))], QXSingle('H')), QXIf(QXQRange('r', crange=QXCRange(QXNum(0), QXNum(1))), [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('+', QXBind('x'), QXNum(1)))]))], None)], None)])]),
    QXProgram([QXMethod('postQFT', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXComp('>', QXBind('N'), QXNum(0))), QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('*', QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), QXCall('omega', [QXBin('*', QXBind('j'), QXBind('k')), QXBin('^', QXNum(2), QXBind('n'))])), [QXSKet(QXBind('j')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])]))], [QXQAssign([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], QXSingle('QFT'))])]),
    QXProgram([QXMethod('postHad', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXComp('>', QXBind('N'), QXNum(0))), QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('/', QXNum(1), QXUni('sqrt', QXBin('^', QXNum(2), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])])), QXEnsures(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n'))), QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], TyEn(QXNum(2)), [QXSum([QXCon('k', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n')))), QXCon('j', QXCRange(QXNum(0), QXBin('^', QXNum(2), QXBind('n'))))], QXBin('*', QXBin('/', QXNum(1), QXBin('^', QXNum(2), QXBind('n'))), QXCall('omega', [QXBin('*', QXBind('j'), QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N'))), QXNum(2)])), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))])]))], [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXSingle('H'))])]),
    QXProgram([QXMethod('singleGrovers', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('f', type=TyFun([TySingle('nat')], TySingle('bool')))], [], [QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyAA(), [QXTensor([QXPartGroup('f', QXBoolLiteral(True), QXUni('sin', QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2), QXBind('n'))]), QXBin('^', QXNum(2), QXBind('n'))))])))]), QXTensor([QXPartGroup('f', QXBoolLiteral(False), QXUni('cos', QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2), QXBind('n'))]), QXBin('^', QXNum(2), QXBind('n'))))])))])])), QXRequires(QXQSpec([QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyAA(), [QXTensor([QXPartGroup('f', QXBoolLiteral(True), QXUni('sin', QXBin('*', QXNum(3), QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2), QXBind('n'))]), QXBin('^', QXNum(2), QXBind('n'))))]))))]), QXTensor([QXPartGroup('f', QXBoolLiteral(False), QXUni('cos', QXBin('*', QXNum(3), QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2), QXBind('n'))]), QXBin('^', QXNum(2), QXBind('n'))))]))))])]))], [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXCall('dis', [QXSingle('H'), QXBind('f'), QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2), QXBind('n'))]), QXBin('^', QXNum(2), QXBind('n'))))]))])]),
    QXProgram([QXMethod('singleEstimate', False, [QXBind('n', type=TySingle('nat')), QXBind('t', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('f', type=TyFun([TySingle('nat')], TySingle('bool'))), QXBind('r', type=TySingle('real'))], [], [QXRequires(QXComp('<', QXBind('t'), QXBind('n'))), QXRequires(QXComp('==', QXBind('r'), QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2), QXBind('n'))]), QXBin('^', QXNum(2), QXBind('n')))))), QXRequires(QXQSpec([QXQRange('p', crange=QXCRange(QXNum(0), QXNum(2))), QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyAA(), [QXSum([QXCon('k', QXCRange(QXNum(0), QXNum(2)))], QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2))), [QXSKet(QXBind('k')), QXSKet(QXBind('k')), QXPartLambda(QXBind('f'), QXUni('sin', QXCall('arcsin', [QXBind('r')])))]), QXSum([QXCon('k', QXCRange(QXNum(0), QXNum(2)))], QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2))), [QXSKet(QXBind('k')), QXSKet(QXBind('k')), QXPartLambda(QXBind('f'), QXUni('cos', QXCall('arcsin', [QXBind('r')])))])])), QXEnsures(QXQSpec([QXQRange('p', crange=QXCRange(QXNum(0), QXNum(2))), QXQRange('q', crange=QXCRange(QXNum(0), QXBind('n')))], TyAA(), [QXTensor([QXSKet(QXNum(0)), QXSKet(QXNum(0)), QXPartLambda(QXBind('f'), QXUni('sin', QXCall('arcsin', [QXBind('r')])))], amp=QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2)))), QXTensor([QXSKet(QXNum(1)), QXSKet(QXNum(1)), QXPartLambda(QXBind('f'), QXUni('sin', QXBin('*', QXNum(3), QXCall('arcsin', [QXBind('r')]))))], amp=QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2)))), QXTensor([QXSKet(QXNum(0)), QXSKet(QXNum(0)), QXPartLambda(QXBind('f'), QXUni('cos', QXCall('arcsin', [QXBind('r')])))], amp=QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2)))), QXTensor([QXSKet(QXNum(1)), QXSKet(QXNum(1)), QXPartLambda(QXBind('f'), QXUni('cos', QXBin('*', QXNum(3), QXCall('arcsin', [QXBind('r')]))))], amp=QXBin('/', QXNum(1), QXUni('sqrt', QXNum(2))))]))], [QXIf(QXQRange('x', crange=QXCRange(QXNum(1), QXNum(2))), [QXQAssign([QXQRange('p', crange=QXCRange(QXNum(0), QXBind('n')))], QXCall('dis', [QXSingle('H'), QXBind('f'), QXBind('r')]))], None)])]),
    

    # 21 - 30

    # 31 - 39
]


class ParseError(Exception):
    pass

class TestProgramTransformer(TestSuite):

    def parse_file(self, filename: str):
        '''Parses a file, returning the ANTLR astract syntax tree.'''

        file_stream = FileStream(filename, encoding='utf-8')
        lexer = ExpLexer(file_stream)
        token_stream = CommonTokenStream(lexer)
        parser = ExpParser(token_stream)
        root = parser.program()

        if parser.getNumberOfSyntaxErrors() > 0:
            # fail
            raise ParseError(f'Failed to parse: {filename}')

        return root

    def convert_file(self, ast):
        '''Attempts to convert a file to the Qafny Programmer AST.'''
        program_transformer = ProgramTransformer()
        qafny_ast = program_transformer.visit(ast)

        return qafny_ast

    def test_program_transformer(self):
        for i, filename in enumerate(suite.TEST_FILES):
            should_test = self.start_case(filename, f'Failed to transform: {filename}')

            if not should_test:
                continue

            antlr_ast = self.parse_file(filename)
            qafny_ast = self.convert_file(antlr_ast)

            # check the ast against .... what?
            # print(qafny_ast)
            # pprint.pp(qafny_ast.__dict__)
            self.console.print("Transformed output:")
            pprint(qafny_ast, console=self.console)
            # Transform the representation into a one-liner
            self.console.print("One-liner:")
            self.printRichReprInOneLine(qafny_ast)
            
            correct = TRANSFORMED_CASES[i] == qafny_ast if i < len(TRANSFORMED_CASES) and TRANSFORMED_CASES[i] is not None else True

            if not correct:
                # self.console.print("[white on red]This is an error line![/]")
                comparison_table = Table(box=box.MINIMAL)
                comparison_table.add_column("Expected")
                comparison_table.add_column("Result")

                comparison_table.add_row(Pretty(TRANSFORMED_CASES[i], indent_size=2), Pretty(qafny_ast, indent_size=2))
                self.console.print(comparison_table)

            self.end_case(correct)

    def test_merge_states(self):
        program_transformer = ProgramTransformer()

        # assert == program_transformer.mergeStates(, )

    # Result = Iterable[Union[Any, Tuple[Any], Tuple[str, Any], Tuple[str, Any, Any]]]
    def printRichReprInOneLine(self, obj: Any):
        '''A recursive function that prints a rich representation in one line.'''

        # ────────── Helper Functions ──────────
        def iter_rich_args(rich_args: Any) -> Iterable[Union[Any, Tuple[str, Any]]]:
            '''Iterate through all __rich_repr__ args. Copied from: https://github.com/Textualize/rich/blob/8c4d3d1d50047e3aaa4140d0ffc1e0c9f1df5af4/rich/pretty.py#L633'''
            for arg in rich_args:
                if isinstance(arg, tuple):
                    if len(arg) == 3:
                        key, child, default = arg
                        if default == child:
                            continue
                        yield key, child
                    elif len(arg) == 2:
                        key, child = arg
                        yield key, child
                    elif len(arg) == 1:
                        yield arg[0]
                else:
                    yield arg

        # ────────── Body code ──────────
        representation = None
        if hasattr(obj, "__rich_repr__") and not inspect.isclass(obj):
            representation = obj.__rich_repr__()

        if representation is not None:
            # object has __rich_repr__

            self.console.print(obj.__class__.__name__ + '(', end='')

            for i, section in enumerate(iter_rich_args(representation)):
                if i > 0:
                    self.console.print(', ', end='')  # print commas between children

                if isinstance(section, tuple):
                    # key and child
                    key, child = section
                    self.console.print(f'{key}=', end='')
                    self.printRichReprInOneLine(child)
                else:
                    self.printRichReprInOneLine(section)

            self.console.print(')', end='')
        elif isinstance(obj, utils.CONTAINERS):
            # container, handle every child
            for test_container_type in utils.CONTAINERS:
                if isinstance(obj, test_container_type):
                    container_type = test_container_type

            open_brace, close_brace, empty = utils.BRACES[container_type](obj)
            
            self.console.print(open_brace, end='')

            if isinstance(obj, utils.MAPPING_CONTAINERS):
                # map
                pass
            else:
                # normal container
                for i, child in enumerate(iter(obj)):
                    if i > 0:
                        self.console.print(', ', end='')
                        
                    self.printRichReprInOneLine(child)

            self.console.print(close_brace, end='')

        else:
            # value, print it out
            self.console.print(repr(obj), end='')


def run():
    test_suite = TestProgramTransformer()
    test_suite.run()


if __name__ == '__main__':
    run()