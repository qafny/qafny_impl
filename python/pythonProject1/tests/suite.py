import time
import sys
from io import StringIO

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

        def __init__(self, context: str, std_output: str, std_error: str):
            self.context = context
            self.std_output = std_output
            self.std_error = std_error

    def __init__(self, *, fail_fast: bool = False, verbose: bool = False):
        # A number indicating the total number of test cases run
        self.total_cases = 0
        # A number indicating the total number of successful test cases
        self.successful_cases = 0
        # A flag indicating whether the test suite should bail at first failure
        self.fail_fast = fail_fast
        # A flag indicating whether the output should be verbose
        self.verbose = verbose
        # An array tracking all of the failed test cases, their outputs, and context messages
        self.failed_cases = []

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

            self.successful_cases += 1
        else:
            if self.verbose:
                print(stylize('[FAIL]', fore('red')) + f' {name}')
            else:
                print(stylize('F', fore('red')), end='')

            # log failure
            self.failed_cases.append(CaseInfo(context, standard_output, standard_error))

            if self.fail_fast:
                raise EarlyOut()

    def run(self):
        start = time.time()

        for test_method_name in self.__test_methods:
            test_method = getattr(self, test_method_name)
            try:
                test_method()
            except self.EarlyOut as e:
                break
            print('')

        end = time.time()

        for failed_case in self.failed_cases:
            print(stylize('====================================================================', fore('red')))
            print(stylize(f'[FAIL]', fore('red')) + ' ' + failed_case.context)
            print(stylize('--------------------------------------------------------------------', fore('red')))
            print(failed_case.std_output)
            print(failed_case.std_err)

        emoji = stylize('✓', fore('green')) if self.successful_cases == self.total_cases else stylize('🞫', fore('red'))
        elapsed_time_fmt = stylize('{0:.6g}s'.format(end - start), fore('yellow'))
        print(f'[{self.successful_cases}/{self.total_cases}] {emoji} - {elapsed_time_fmt}')