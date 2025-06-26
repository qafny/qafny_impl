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


class ParseError(Exception):
    pass

class TestProgramTransformer():

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
    #    print(f"Converted AST: {qafny_ast}")

        return qafny_ast

    def test_program_transformer(self):
        for i, filename in enumerate(suite.TEST_FILES):

            antlr_ast = self.parse_file(filename)
            qafny_ast = self.convert_file(antlr_ast)


    def test_merge_states(self):
        program_transformer = ProgramTransformer()



def run():
    test_suite = TestProgramTransformer()
    test_suite.test_program_transformer()


if __name__ == '__main__':
    run()