import suite
from suite import TestSuite


class TestProgramTransformer(TestSuite):

    def test_program_transformer():
        for file in suite.TEST_FILES:
            print(file)


if __name__ == '__main__':
    test_suite = TestProgramTransformer()
    test_suite.run()
