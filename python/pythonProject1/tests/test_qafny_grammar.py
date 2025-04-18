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


class TestQafnyGrammar(TestSuite): # (unittest.TestCase):

    def parse_file(self, filename) -> bool:
        '''Attempts to parse a file, specified by filename, returning true if the file parsed successfully.'''

        file_stream = FileStream(filename, encoding='utf-8')
        lexer = ExpLexer(file_stream)
        token_stream = CommonTokenStream(lexer)
        parser = ExpParser(token_stream)
        root = parser.program()

        return parser.getNumberOfSyntaxErrors() == 0

    def test_files(self):
        for filename in suite.TEST_FILES:
            # capture stdout
            self.start_case()
            result = self.parse_file(filename)
            self.end_case(result, filename, f'Failed to parse: {filename}')


def run():
    suite = TestQafnyGrammar(verbose=False)
    suite.run()


if __name__ == '__main__':
    run()
