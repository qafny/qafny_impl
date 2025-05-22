import time
import sys
from io import StringIO
import traceback
import argparse
from enum import Enum

from colored import stylize, fore

# a variable storing all of the file names currently in test/Qafny/ and their testing order
TEST_FILES = [
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
    # 'QuantumFixedPoint', # unknown if finished
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
TEST_FILES = [f'../../../test/Qafny/{base}.qfy' for base in TEST_FILES]

class TestSuite:

    class EarlyOut(Exception):
        pass

    class CaseInfo:
        '''Helper record class to track important information about each test case'''

        class Type(Enum):
            PASS = 1
            FAIL = 2

        def __init__(self, context: str, std_output: str, std_error: str, type: Type = Type.FAIL):
            self.context = context
            self.std_output = std_output
            self.std_error = std_error
            self.type = type

        def __str__(self):
            color = 'green' if self.type == TestSuite.CaseInfo.Type.PASS else 'red'
            message = 'PASS' if self.type == TestSuite.CaseInfo.Type.PASS else 'FAIL'

            result = stylize('====================================================================\n', fore(color))
            result += stylize(f'[{message}]', fore(color)) + ' ' + self.context + '\n'
            result += stylize('--------------------------------------------------------------------\n', fore(color))
            result += self.std_output + '\n'
            result += self.std_error + '\n'

            return result

    def parse_cli(self):
        # extremely simple argument parsing
        cl_parser = argparse.ArgumentParser()
        # flag to indicate whether fail_fast should be set to true
        cl_parser.add_argument('-f', '--fail-fast', action='store_true', help='stop all tests as soon as one fails')
        # flag to indicate whether verbose should be set to true (extra debug information)
        cl_parser.add_argument('-v', '--verbose', action='store_true', help='print out more information about test passing/failing')
        # flag to indicate whether to also print standard output and standard error from passing cases
        cl_parser.add_argument('-s', '--force-output', action='store_true', help='print stdout and stderr even from passing cases')

        return cl_parser.parse_args()

    def constructor_fallbacks(self, fail_fast: bool, verbose: bool, force_output: bool):
        args = self.parse_cli()

        if args.fail_fast is None:
            args.fail_fast = fail_fast
        if args.verbose is None:
            args.verbose = verbose
        if args.force_output is None:
            args.force_output = force_output

        return args

    def __init__(self, *, fail_fast: bool = False, verbose: bool = False, force_output: bool = False):
        '''Creates a new test suite. This constructor should be called by sub-classes.'''
        # parse arguments (if there are any)
        ctor_args = self.constructor_fallbacks(fail_fast, verbose, force_output)

        # A number indicating the total number of test cases run
        self.total_cases = 0
        # A number indicating the total number of successful test cases
        self.successful_cases = 0
        # A flag indicating whether the test suite should bail at first failure
        self.fail_fast = ctor_args.fail_fast
        # A flag indicating whether the output should be verbose
        self.verbose = ctor_args.verbose
        # A flag indicating whether stdout/stderr should always be printed (regardless of whether the case succeeds/fails)
        self.force_output = ctor_args.force_output
        # An array tracking the test cases, their outputs, and context messages
        # If force_output is set to true, it will contain CaseInfo structs for all of the test cases
        # If force_output is set to false, it will contain CaseInfo structs of the failed cases
        self.cases = []

        # private instances
        # An array of strings of all the test cases on this class
        self.__test_methods = [method for method in dir(self.__class__) if method.startswith('test')]

        # A flag indicating whether we are currently capturing stdout/stderr
        self.__capturing = False

    def begin_capture(self):
        '''Starts capturing standard output and standard error.'''
        assert not self.__capturing, 'A new capture cannot be started whilst an old one is already underway.'

        self.__old_stdout = sys.stdout
        self.__old_stderr = sys.stderr
        self.__stdout_pipe = StringIO()
        self.__stderr_pipe = StringIO()
        sys.stdout = self.__stdout_pipe
        sys.stderr = self.__stderr_pipe
        self.__capturing = True

    def end_capture(self) -> (str, str):
        '''Ends capturing standard output and standard error, returning them as two strings.'''
        assert self.__capturing, 'A capture cannot be stopped if none have started.'

        sys.stdout = self.__old_stdout
        sys.stderr = self.__old_stderr
        standard_output = self.__stdout_pipe.getvalue()
        standard_error = self.__stderr_pipe.getvalue()
        self.__capturing = False

        return (standard_output, standard_error)

    def start_case(self):
        self.begin_capture()

    def end_case(self, successful: bool, name: str, error_context: str):
        standard_output, standard_error = self.end_capture()

        self.total_cases += 1

        if successful:
            if self.verbose:
                print(stylize('[PASS]', fore('green')) + f' {name}')
            else:
                print(stylize('.', fore('green')), end='')

            if self.force_output:
                # log success (context only used on fail)
                self.cases.append(self.CaseInfo(name, standard_output, standard_error, TestSuite.CaseInfo.Type.PASS))

            self.successful_cases += 1
        else:
            if self.verbose:
                print(stylize('[FAIL]', fore('red')) + f' {name}')
            else:
                print(stylize('F', fore('red')), end='')

            # log failure
            self.cases.append(self.CaseInfo(error_context, standard_output, standard_error))

            if self.fail_fast:
                raise self.EarlyOut()

    def run(self) -> bool:
        '''Runs the test suite, returns true if all tests passed, false if any test(s) failed.'''
        start = time.time()

        for test_method_name in self.__test_methods:
            test_method = getattr(self, test_method_name)
            try:
                test_method()
            except self.EarlyOut as e:
                print('')
                break
            except Exception as e:
                standard_output = ''
                standard_errror = ''
                if self.__capturing:
                    standard_output, standard_error = self.end_capture()
                
                print('')

                exception_info = traceback.format_exc()
                print(stylize(f'Error happened whilst running test case: {test_method_name}\n', fore('red'))) # {exception_info}
                
                self.total_cases += 1
                self.cases.append(self.CaseInfo(f'Exception occured whilst testing: {test_method_name}', standard_output, standard_error + exception_info))
            print('')

        end = time.time()

        for case in self.cases:
            print(case)

        emoji = stylize('✓', fore('green')) if self.successful_cases == self.total_cases else stylize('🞫', fore('red'))
        elapsed_time_fmt = stylize('{0:.6g}s'.format(end - start), fore('yellow'))
        print(f'[{self.successful_cases}/{self.total_cases}] {emoji} - {elapsed_time_fmt}')

        return self.successful_cases == self.total_cases
