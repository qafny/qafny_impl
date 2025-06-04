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
    QXProgram([QXMethod('hadtest', False, [QXBind('n', TySingle('nat')), QXBind('q', TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])],TyNor(),[QXTensor([QXSKet(QXNum(0.0))])])),QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))], id='i', crange=QXCRange(QXNum(0.0), QXBind('n')))]))], [QXAssert(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXAssert(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))], id='i', crange=QXCRange(QXNum(0.0), QXBind('n')))]))])]),
    QXProgram([QXMethod('hadtest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k'))])]))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXCast(TyEn(QXNum(1)), [QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])])])]),
    QXProgram([QXMethod('hadtest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('*', QXBin('^', QXNum(2.0), QXBind('n')), QXCall('omega', [QXBin('*', QXBind('j'), QXBind('k')), QXNum(2.0)]))), [QXSKet(QXBind('j')), QXSKet(QXBind('k'))])]))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H'))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('a', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('*', QXBind('a'), QXBin('%', QXBind('k'), QXBind('N'))))])]))], [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('*', QXBind('a'), QXBin('%', QXBind('x'), QXBind('N'))))]))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('k', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXBin('%', QXBin('+', QXBind('k'), QXNum(1.0)), QXBin('^', QXNum(2.0), QXBind('n'))))])]))], [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('+', QXBind('x'), QXNum(1.0)))]))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('k', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('^', QXNum(2.0), QXBind('n'))), [QXSKet(QXBind('j')), QXSKet(QXBind('k'))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('*', QXBin('^', QXNum(2.0), QXBind('n')), QXCall('omega', [QXBind('k'), QXBin('^', QXNum(2.0), QXBind('n'))]))), [QXSKet(QXBind('j')), QXSKet(QXBind('k'))])]))], [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXBind('k'), QXBin('^', QXNum(2.0), QXBind('n'))]), [QXSKet(QXBind('x'))]))])]),
    QXProgram([QXMethod('lambdatest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('k', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1.0)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBin('%', QXBin('+', QXBind('j'), QXNum(1.0)), QXBin('^', QXNum(2.0), QXBind('n'))))])]))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('+', QXBind('x'), QXNum(1.0)))]))])]),
    QXProgram([QXMethod('conditionaltest1', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('i', type=TySingle('nat')), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXComp('<=', QXNum(0.0), QXComp('<', QXBind('i'), QXBind('n')))), QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('i'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('i'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('i')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])])), QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXBind('i'), QXBind('n'))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXBin('+', QXBind('i'), QXNum(1.0)), QXBind('n'))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXBind('i'), QXNum(1.0)))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBin('+', QXBind('i'), QXNum(1.0)))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBin('+', QXBind('i'), QXNum(1.0))))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])]))], [QXIf(QXQRange('q', [QXCRange(QXBind('i'), QXBin('+', QXBind('i'), QXNum(1)))]), [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('^', QXBind('base'), QXBin('*', QXBin('^', QXNum(2.0), QXBind('i')), QXBin('%', QXBind('x'), QXBind('N')))))]))], None)])]),
    QXProgram([QXMethod('conditionaltest2', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXNum(1.0))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))])])), QXRequires(QXQSpec([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXNum(2.0))), QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXIfExp(QXComp('==', QXBind('k'), QXNum(0.0)), QXNum(0.0), QXBind('j')))])]))], [QXIf(QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H'))], None)])]),
    QXProgram([QXMethod('conditionaltest3', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXNum(1.0)))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBind('k'), QXNum(2.0)))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyEn(QXNum(2.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))), condition=QXComp('==', QXBin('%', QXBind('k'), QXNum(2.0)), QXNum(0.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBind('k'), QXNum(2.0)))]), QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))), condition=QXComp('==', QXBin('%', QXBind('k'), QXNum(2.0)), QXNum(1.0)))], QXBin('/', QXNum(1.0), QXBin('*', QXUni('sqrt', QXBin('^', QXNum(2.0), QXBin('+', QXBind('n'), QXNum(1.0)))), QXCall('omega', [QXBin('*', QXBind('j'), QXBin('%', QXBind('k'), QXNum(2.0))), QXNum(2.0)]))), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))])]))], [QXIf(QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H'))], None)])]),
    QXProgram([QXMethod('conditionaltest4', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('r', type=TyQ(QXNum(1.0))), QXBind('u', type=TyQ(QXNum(1.0))), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('^', QXNum(2.0), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))])])), QXRequires(QXQSpec([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXRequires(QXQSpec([QXQRange('u', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('u', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyEn(QXNum(3.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('t', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXBin('^', QXNum(2.0), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j')), QXSKet(QXIfExp(QXComp('<', QXBind('k'), QXBind('N')), QXNum(1.0), QXNum(0.0))), QXSKet(QXIfExp(QXComp('<', QXBind('k'), QXBind('N')), QXBind('t'), QXNum(0.0)))])]))], [QXIf(QXQComp('<', QXBind('p'), QXBind('N'), QXQIndex('u', QXNum(0.0))), [QXQAssign([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H'))], None)])]),
    QXProgram([QXMethod('conditionaltest5', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('r', type=TyQ(QXNum(1.0))), QXBind('u', type=TyQ(QXNum(1.0))), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('^', QXNum(2.0), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))])])), QXRequires(QXQSpec([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXRequires(QXQSpec([QXQRange('u', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('u', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyEn(QXNum(3.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('t', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXBin('^', QXNum(2.0), QXBind('n'))), [QXSKet(QXBind('k')), QXSKet(QXIfExp(QXComp('<', QXBind('k'), QXBind('N')), QXBin('+', QXBind('j'), QXNum(1.0)), QXBind('j'))), QXSKet(QXIfExp(QXComp('<', QXBind('k'), QXBind('N')), QXNum(1.0), QXNum(0.0))), QXSKet(QXIfExp(QXComp('<', QXBind('k'), QXBind('N')), QXBind('t'), QXNum(0.0)))])]))], [QXIf(QXQComp('<', QXCRangeAExp(QXBind('q'), QXCRange(QXNum(0.0), QXBind('n'))), QXBind('N'), QXQIndex('u', QXNum(0.0))), [QXQAssign([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H')), QXIf(QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('+', QXBind('x'), QXNum(1.0)))]))], None)], None)])]),
    QXProgram([QXMethod('postQFT', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('*', QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n'))), QXCall('omega', [QXBin('*', QXBind('j'), QXBind('k')), QXBin('^', QXNum(2.0), QXBind('n'))]))), [QXSKet(QXBind('j')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])]))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('QFT'))])]),
    QXProgram([QXMethod('postHad', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('base', type=TySingle('nat')), QXBind('N', type=TySingle('nat'))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(2.0)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('*', QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n'))), QXCall('omega', [QXBin('*', QXBind('j'), QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N'))), QXNum(2.0)]))), [QXSKet(QXBind('k')), QXSKet(QXBind('j'))])]))], [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H'))])]),
    QXProgram([QXMethod('singleGrovers', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('f', type=TyFun([TySingle('nat')], TySingle('bool')))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyAA(), [QXTensor([QXPartGroup('f', QXBoolLiteral(True), QXUni('sin', QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2.0), QXBind('n'))]), QXBin('^', QXNum(2.0), QXBind('n'))))])))]), QXTensor([QXPartGroup('f', QXBoolLiteral(False), QXUni('cos', QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2.0), QXBind('n'))]), QXBin('^', QXNum(2.0), QXBind('n'))))])))])])), QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyAA(), [QXTensor([QXPartGroup('f', QXBoolLiteral(True), QXUni('sin', QXBin('*', QXNum(3.0), QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2.0), QXBind('n'))]), QXBin('^', QXNum(2.0), QXBind('n'))))]))))]), QXTensor([QXPartGroup('f', QXBoolLiteral(False), QXUni('cos', QXBin('*', QXNum(3.0), QXCall('arcsin', [QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2.0), QXBind('n'))]), QXBin('^', QXNum(2.0), QXBind('n'))))]))))])]))], [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXCall('dis', [QXSingle('H'), QXBind('f'), QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2.0), QXBind('n'))]), QXBin('^', QXNum(2.0), QXBind('n'))))]))])]),
    QXProgram([QXMethod('singleEstimate', False, [QXBind('n', type=TySingle('nat')), QXBind('t', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('f', type=TyFun([TySingle('nat')], TySingle('bool'))), QXBind('r', type=TySingle('real'))], [], [QXRequires(QXComp('<', QXBind('t'), QXBind('n'))), QXRequires(QXComp('==', QXBind('r'), QXUni('sqrt', QXBin('/', QXCall('sumFun', [QXBind('f'), QXBin('^', QXNum(2.0), QXBind('n'))]), QXBin('^', QXNum(2.0), QXBind('n')))))), QXRequires(QXQSpec([QXQRange('p', [QXCRange(QXNum(0.0), QXNum(2.0))]), QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyAA(), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('k')), QXSKet(QXBind('k')), QXPartLambda(QXBind('f'), QXUni('sin', QXCall('arcsin', [QXBind('r')])))]), QXSum([QXCon('k', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('k')), QXSKet(QXBind('k')), QXPartLambda(QXBind('f'), QXUni('cos', QXCall('arcsin', [QXBind('r')])))])])), QXRequires(QXQSpec([QXQRange('p', [QXCRange(QXNum(0.0), QXNum(2.0))]), QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyAA(), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('k')), QXSKet(QXBind('k')), QXIfExp(QXComp('==', QXBind('k'), QXNum(0.0)), QXPartLambda(QXBind('f'), QXUni('sin', QXCall('arcsin', [QXBind('r')]))), QXPartLambda(QXBind('f'), QXUni('sin', QXBin('*', QXNum(3.0), QXCall('arcsin', [QXBind('r')])))))]), QXSum([QXCon('k', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('k')), QXSKet(QXBind('k')), QXIfExp(QXComp('==', QXBind('k'), QXNum(0.0)), QXPartLambda(QXBind('f'), QXUni('cos', QXCall('arcsin', [QXBind('r')]))), QXPartLambda(QXBind('f'), QXUni('cos', QXBin('*', QXNum(3.0), QXCall('arcsin', [QXBind('r')])))))])]))], [QXIf(QXQRange('x', [QXCRange(QXNum(1.0), QXBin('+', QXNum(1.0), QXNum(1)))]), [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXCall('dis', [QXSingle('H'), QXBind('f'), QXBind('r')]))], None)])]),
    QXProgram([QXMethod('BellPair', False, [QXBind('q', type=TyQ(QXNum(2.0)))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXNum(2.0))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXNum(2.0))])], TyEn(QXNum(1)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('j')), QXSKet(QXBind('j'))])]))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H')), QXIf(QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), [QXQAssign([QXQRange('q', [QXCRange(QXNum(1.0), QXBin('+', QXNum(1.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1.0)), QXNum(2.0)))]))], None)])]),
    QXProgram([QXMethod('GHZ', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n')))], [], [QXRequires(QXComp('>=', QXBind('n'), QXNum(2.0))), QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('j'))])]))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H')), QXFor('i', QXCRange(QXNum(0.0), QXBin('-', QXBind('n'), QXNum(1.0))), [QXInvariant(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXBind('i'), QXNum(1.0)))])], TyEn(QXNum(1)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('j'))])])), QXInvariant(QXQSpec([QXQRange('q', [QXCRange(QXBin('+', QXBind('i'), QXNum(1.0)), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])]))], [QXIf(QXQRange('q', [QXCRange(QXBind('i'), QXBin('+', QXBind('i'), QXNum(1)))]), [QXQAssign([QXQRange('q', [QXCRange(QXBin('+', QXBind('i'), QXNum(1.0)), QXBin('+', QXBin('+', QXBind('i'), QXNum(1.0)), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1.0)), QXNum(2.0)))]))], None)])])]),
    QXProgram([QXMethod('Teleporation', False, [QXBind('q', type=TyQ(QXNum(1.0))), QXBind('p', type=TyQ(QXNum(1.0))), QXBind('r', type=TyQ(QXNum(1.0)))], [QXBind('y', type=TySingle('nat')), QXBind('prob', type=TySingle('real'))], [QXRequires(QXQSpec([QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXRequires(QXQSpec([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXComp('==', QXBind('r'), QXBind('q')))], [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H')), QXIf(QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), [QXQAssign([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1.0)), QXNum(2.0)))]))], None), QXAssert(QXQSpec([QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXNum(2.0)))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXNum(2.0))), [QXSKet(QXBind('k')), QXSKet(QXBind('k'))])])), QXIf(QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1.0)), QXNum(2.0)))]))], None), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H')), QXMeasure(['y', 'p1'], [QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])]), QXMeasure(['z', 'p2'], [QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])]), QXIf(QXComp('==', QXBind('y'), QXNum(1.0)), [QXQAssign([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1.0)), QXNum(2.0)))]))], None), QXIf(QXComp('==', QXBind('z'), QXNum(1.0)), [QXQAssign([QXQRange('r', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXBin('*', QXNum(1.0), QXBind('x')), QXNum(2.0)]), [QXVKet(QXBind('x'))]))], None)])]),
    QXProgram([QXInclude('BellPair.qfy'), QXMethod('Superdense', False, [QXBind('q', type=TyQ(QXNum(2.0))), QXBind('x', type=TySingle('bool')), QXBind('y', type=TySingle('bool'))], [QXBind('u', type=TySingle('bool')), QXBind('v', type=TySingle('bool')), QXBind('probu', type=TySingle('real')), QXBind('probv', type=TySingle('real'))], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXNum(2.0))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXComp('==', QXBind('u'), QXBind('x'))), QXEnsures(QXComp('==', QXBind('v'), QXBind('y'))), QXEnsures(QXComp('==', QXBind('probu'), QXNum(1.0))), QXEnsures(QXComp('==', QXBind('probv'), QXNum(1.0)))], [QXCall('Bellpair', [QXCRangeAExp(QXBind('q'), QXCRange(QXNum(0.0), QXNum(2.0)))]), QXIf(QXBind('y'), [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('+', QXBind('x'), QXNum(1.0)))]))], None), QXIf(QXBind('x'), [QXQAssign([QXQRange('q', [QXCRange(QXNum(1.0), QXBin('+', QXNum(1.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(1.0), QXNum(2.0)]), [QXVKet(QXBind('x'))]))], None), QXIf(QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))]), [QXQAssign([QXQRange('q', [QXCRange(QXNum(1.0), QXBin('+', QXNum(1.0), QXNum(1)))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('+', QXBind('x'), QXNum(1.0)))]))], None), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H')), QXMeasure([QXBind('u'), QXBind('probu')], [QXQRange('q', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])]), QXMeasure([QXBind('v'), QXBind('probv')], [QXQRange('q', [QXCRange(QXNum(1.0), QXBin('+', QXNum(1.0), QXNum(1)))])])])]),
    QXProgram([QXMethod('Shors', False, [QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXBind('n'))), QXBind('n', type=TySingle('nat')), QXBind('N', type=TySingle('nat')), QXBind('base', type=TySingle('nat')), QXBind('r', type=TySingle('nat')), QXBind('c', type=TySingle('nat'))], [QXBind('y', type=TySingle('nat')), QXBind('py', type=TySingle('real'))], [QXRequires(QXComp('<', QXNum(1.0), QXComp('<', QXBind('base'), QXBind('N')))), QXRequires(QXComp('==', QXCall('gcd', [QXBind('base'), QXBind('N')]), QXNum(1.0))), QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXRequires(QXQSpec([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXEnsures(QXComp('==', QXBind('y'), QXBin('*', QXBind('c'), QXBin('^', QXNum(2.0), QXBin('/', QXBind('n'), QXBind('r')))))), QXEnsures(QXComp('==', QXBind('py'), QXBin('/', QXNum(4.0), QXBin('*', QXBind('pi'), QXBin('*', QXBind('pi'), QXBind('r'))))))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('%', QXBin('+', QXBind('x'), QXNum(1.0)), QXNum(2.0)))])), QXFor('i', QXCRange(QXNum(0.0), QXBind('n')), [QXInvariant(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('i'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('i'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('i')))), [QXSKet(QXBind('k')), QXSKet(QXBin('%', QXBin('^', QXBind('base'), QXBind('k')), QXBind('N')))])])), QXInvariant(QXQSpec([QXQRange('q', [QXCRange(QXBind('i'), QXBind('n'))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))])]))], [QXIf(QXQRange('q', [QXCRange(QXBind('i'), QXBin('+', QXBind('i'), QXNum(1)))]), [QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])], QXOracle([QXBind('x')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBin('^', QXBind('base'), QXBin('*', QXBin('^', QXNum(2.0), QXBind('i')), QXBin('%', QXBind('x'), QXBind('N')))))]))], None)]), QXMeasure([QXBind('v'), QXBind('prob')], [QXQRange('p', [QXCRange(QXNum(0.0), QXBind('n'))])]), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('QFT')), QXMeasure([QXBind('y'), QXBind('py')], [QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], res=QXBin('*', QXBind('c'), QXBin('^', QXNum(2.0), QXBin('/', QXBind('n'), QXBind('r')))))])]),
    QXProgram([QXMethod('DeutschJozsa', False, [QXBind('q', type=TyQ(QXBind('n'))), QXBind('p', type=TyQ(QXNum(1.0))), QXBind('n', type=TySingle('nat')), QXBind('f', type=TyFun([TySingle('nat')], TySingle('bool')))], [QXBind('k', type=TySingle('nat')), QXBind('prob', type=TySingle('real'))], [QXRequires(QXComp('>=', QXBind('n'), QXNum(1.0))), QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([QXSKet(QXNum(0.0))])])), QXRequires(QXQSpec([QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyNor(), [QXTensor([QXSKet(QXNum(1.0))])])), QXRequires(QXLogic('||', QXCall('balance', [QXBind('f')]), QXCall('constant', [QXBind('f')]))), QXEnsures(QXLogic('==>', QXLogic('&&', QXCall('balance', [QXBind('f')]), QXComp('==', QXBind('k'), QXNum(0.0))), QXComp('==', QXBind('prob'), QXNum(0.0)))), QXEnsures(QXLogic('==>', QXLogic('&&', QXCall('constant', [QXBind('f')]), QXComp('==', QXBind('k'), QXNum(1.0))), QXComp('==', QXBind('prob'), QXNum(1.0))))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXQAssign([QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXSingle('H')), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], QXOracle([QXBind('x'), QXBind('y')], QXCall('omega', [QXNum(0), QXNum(1)]), [QXSKet(QXBind('x')), QXSKet(QXBin('+', QXBind('y'), QXCall('f', [QXBind('x')])))])), QXAssert(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyEn(QXNum(1)), [QXSum([QXCon('i', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('*', QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n'))), QXCall('omega', [QXCall('f', [QXBind('i')]), QXNum(2.0)]))), [QXSKet(QXBind('i')), QXSKet(QXHad('-'))])])), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXAssert(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))]), QXQRange('p', [QXCRange(QXNum(0.0), QXBin('+', QXNum(0.0), QXNum(1)))])], TyEn(QXNum(2.0)), [QXSum([QXCon('j', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n')))), QXCon('i', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXBin('*', QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n'))), QXCall('omega', [QXBin('+', QXCall('f', [QXBind('i')]), QXBin('*', QXBind('i'), QXBind('j'))), QXNum(2.0)]))), [QXSKet(QXBind('j')), QXSKet(QXHad('-'))])])), QXMeasure([QXBind('prob'), QXBind('k')], [QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])])])]),
    
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
            
            correct = TRANSFORMED_CASES[i] == qafny_ast if i < len(TRANSFORMED_CASES) else True

            if not correct:
                # pprint(TRANSFORMED_CASES[i])
                # self.console.print(Columns([Pretty(qafny_ast), Pretty(TRANSFORMED_CASES[i])]))
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