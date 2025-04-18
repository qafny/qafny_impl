# add the parent directory (root python directory) to the path
import sys
import os

script_dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(script_dir)
sys.path.append("../")

# regular imports

import suite
from suite import TestSuite

from antlr4 import FileStream, CommonTokenStream
from ExpLexer import ExpLexer
from ExpParser import ExpParser
from ProgramTransformer import ProgramTransformer


class TestProgramTransformer(TestSuite):

    def parse_file(self, filename: str):
        '''Parses a file, returning the ANTLR astract syntax tree.'''

        file_stream = FileStream(filename, encoding='utf-8')
        lexer = ExpLexer(file_stream)
        token_stream = CommonTokenStream(lexer)
        parser = ExpParser(token_stream)
        return parser.program()

    def convert_file(self, ast):
        '''Attempts to convert a file to the Qafny Programmer AST.'''
        program_transformer = ProgramTransformer()
        qafny_ast = program_transformer.visit(ast)

        return qafny_ast

    def test_program_transformer(self):
        for filename in suite.TEST_FILES:
            self.start_case()
            
            antlr_ast = self.parse_file(filename)
            qafny_ast = self.convert_file(antlr_ast)

            self.end_case(True, filename, error_context)


if __name__ == '__main__':
    test_suite = TestProgramTransformer()
    test_suite.run()
