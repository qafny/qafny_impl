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

import suite
from suite import TestSuite

from antlr4 import FileStream, CommonTokenStream
from ExpLexer import ExpLexer
from ExpParser import ExpParser
from ProgramTransformer import ProgramTransformer

from Programmer import *


# An array containing the expected results of transforming each case
# These were all hand-checked but generated from ProgramTransformer.py
TRANSFORMED_CASES = [
    QXProgram([QXMethod('hadtest', False, [QXBind('n', TySingle('nat')), QXBind('q', TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])],TyNor(),[QXTensor([[QXSKet(QXNum(0.0))]])])),QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))], id='i', crange=QXCRange(QXNum(0.0), QXBind('n')))]))], [QXAssert(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([[QXSKet(QXNum(0.0))]])])), QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXAssert(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyHad(), [QXTensor([QXSKet(QXHad('+'))], id='i', crange=QXCRange(QXNum(0.0), QXBind('n')))]))])]),
    QXProgram([QXMethod('hadtest', False, [QXBind('n', type=TySingle('nat')), QXBind('q', type=TyQ(QXBind('n')))], [], [QXRequires(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyNor(), [QXTensor([[QXSKet(QXNum(0.0))]])])), QXEnsures(QXQSpec([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], TyEn(QXNum(1)), [QXSum([QXCon('k', QXCRange(QXNum(0.0), QXBin('^', QXNum(2.0), QXBind('n'))))], QXBin('/', QXNum(1.0), QXUni('sqrt', QXBin('^', QXNum(2.0), QXBind('n')))), [[QXSKet(QXBind('k'))]])]))], [QXQAssign([QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])], QXSingle('H')), QXCast(TyEn(QXNum(1)), [QXQRange('q', [QXCRange(QXNum(0.0), QXBind('n'))])])])]),
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

    def test_merge_specs(self):
        program_transformer = ProgramTransformer()

        # assert == program_transformer.mergeStates(, )


def run():
    test_suite = TestProgramTransformer()
    test_suite.run()


if __name__ == '__main__':
    run()