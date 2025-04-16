# import unittest

# add the parent directory (root python directory) to the path
import sys
import os

script_dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(script_dir)
sys.path.append("../")

# regular imports
import time

from antlr4 import FileStream, CommonTokenStream
from ExpLexer import ExpLexer
from ExpParser import ExpParser

from io import StringIO

from colored import stylize, fore

class TestQafnyGrammar(): # (unittest.TestCase):

    def __init__(self, *, fail_fast: bool = True, verbose: bool = False):
        self.failed_cases = []
        self.count = 0
        self.fail_fast = fail_fast
        self.verbose = verbose

    def parse_file(self, filename):
        ''''''

        file_stream = FileStream(filename, encoding='utf-8')
        lexer = ExpLexer(file_stream)
        token_stream = CommonTokenStream(lexer)
        parser = ExpParser(token_stream)
        root = parser.program()

        return parser.getNumberOfSyntaxErrors() == 0

    def test_files(self):
        files = [
            'test1',
            'test2',
            'test3',
            'test4',
            'test5',
            'test6',
            'test7',
            'test8',
            'test9',
            'test10',
            'test11',
            'test12',
            'test13',
            'test14',
            'test15',
            'test16',
            'BellPair',
            'GHZ',
            'Teleportation',
            'Superdense',
            'Shors',
            'DeutschJozsa',
            'simon',
            'DiscreteLog',
            'Grovers',
            'QPE',
            'SWAPTest',
            'AmpAmp',
            'AmplitudeEstimation',
            'AppxCounting',
            'BHSP',
            'BHT',
            'BV',
            'FirstAmpEstimate',
            # 'FixedPtSearch', # currently unfinished
            'FOQA',
            # 'HSG', # marked as todo, should we revisit?
            # 'LCU',
            'LongDistanceEntangle',
            'NonBoolean',
            'QFTModQ',
            'SimpleAmpEstimate',
            'Stabilizer',
            'StateDistinguishing'
        ]

        # add the qafny test root directory and extension to the testing files
        files = [f'../../../test/Qafny/{base}.qfy' for base in files]

        for file in files:
            # capture stdout
            stdout = sys.stdout
            stderr = sys.stderr
            sys.stdout = StringIO()
            sys.stderr = StringIO()
            result = self.parse_file(file)
            output = sys.stdout.getvalue() + sys.stderr.getvalue()
            sys.stdout = stdout
            sys.stderr = stderr

            self.count += 1

            if result:
                if self.verbose:
                    print(stylize('[PASS]', fore('green')) + f' {file}')
                else:
                    print(stylize('.', fore('green')), end='')
            else:
                if self.verbose:
                    print(stylize('[FAIL]', fore('red')) + f' {file}')
                else:
                    print(stylize('F', fore('red')), end='')
                # log failure
                self.failed_cases.append((file, output))

                if self.fail_fast:
                    break
        print('')

    def run(self):
        start = time.time()
        self.test_files()
        end = time.time()
        
        for (filename, output) in self.failed_cases:
            print(stylize('====================================================================', fore('red')))
            print(stylize(f'[FAIL]', fore('red')) + f' Failed to parse: {filename}')
            print(stylize('--------------------------------------------------------------------', fore('red')))
            print(output)

        successful = self.count - len(self.failed_cases)

        emoji = stylize('✓', fore('green')) if successful == self.count else stylize('🞫', fore('red'))
        elapsed_time_fmt = stylize('{0:.6g}s'.format(end - start), fore('yellow'))
        print(f'[{successful}/{self.count}] {emoji} - {elapsed_time_fmt}')

# if __name__ == '__main__':
#    unittest.main(buffer=True)

def run():
    suite = TestQafnyGrammar()
    suite.run()

if __name__ == '__main__':
    run()
