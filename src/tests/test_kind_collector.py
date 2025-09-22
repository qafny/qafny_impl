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
from CollectKind import CollectKind
from test_prog_transformer import TestProgramTransformer

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

class TestKindCollector(TestProgramTransformer):
    
    def collect_locus(self, qafny_ast):
        '''Collects the locus of a Qafny AST.'''
        kinds_collector = CollectKind()
        result = kinds_collector.visit(qafny_ast)
        if not result:
            print(f"Collected kinds: {kinds_collector.get_kenv()}")
        else:
            pass
        return result

        

    def test_kind_collector(self):
        for i, filename in enumerate(suite.TEST_FILES):

            antlr_ast = self.parse_file(filename)
            qafny_ast = self.convert_file(antlr_ast)
            prog_kinds = self.collect_locus(qafny_ast)
            if not prog_kinds:
                print(f"Failed to collect kinds from: {filename}\n") 

    def test_merge_states(self):
        program_transformer = ProgramTransformer()


def run():
    test_suite = TestKindCollector()
    test_suite.test_kind_collector()


if __name__ == '__main__':
    run()