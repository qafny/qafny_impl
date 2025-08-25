from os import get_terminal_size
from textwrap import wrap

if __name__ != '__main__':
    from .ErrorReport import ErrorReport
    from .CodeSnippet import CodeSnippet
else:
    from ErrorReport import ErrorReport
    from CodeSnippet import CodeSnippet

class CodeReport(ErrorReport):
    '''
    Represents a human-readable error report with source code attached.
    A combination of ErrorReport and CodeSnippet
    Has features for:
    * line numbering
    * syntax highlighting
    * restricting the area of interest
    * attaching error information


    Report:
    [error]
    [sources]
    
    error: Type mismatch
    × cannot convert real to seq<real>
    ├─► context
    ╰─► more context

    ╭─[test2.qfy:2:5]────────────────────────────────────────────────────╮
    │ 1 method hadtest(n: nat, q : Q[n])                                 │
    │ 2   requires { q[0, n) : nor ↦ |0⟩ }                               │
    ·     └──┬───┘                                                       ·
    ·        ╰───────► this precondition was not satisfied               ·
    │ 3   ensures  { q[0, n) : en ↦ ∑ k ∈ [0, 2^n) . 1/sqrt(2^n) | k ⟩ }│
    │ 4 {                                                                │
    ╰────────────────────────────────────────────────────────────────────╯
    ╞════════════════════════════════════════════════════════════════════╡ (terminal width)
    error: Precondition fail
    ...
    '''

    def __init__(self, code: str, *, gutter: bool = True):
        """Initializer for CodeReport, takes in the code as a string"""
        # the code in text fire
        self._code = code

        # should we print out the gutter? (the line numbers to the left of the code)
        self._gutter = gutter

    def __str__(self):
        """Creates a string from this code report, presumably to print on the console"""
        
        result = ''

        # how much horizontal space we have to work with
        # used to split long lines of code onto multiple terminal lines whilst maintaining the gutter (space with line numbers)
        term_y = get_terminal_size().columns
        
        lines = self._code.splitlines()
        
        gutter_size = 0
        if self._gutter:
            # the number of columns the largest number takes up (to keep the numbers aligned)
            gutter_size = len(str(len(lines)))

            term_y -= gutter_size + 3 # the three extra spaces come from ' │ '

        for line_no, line in enumerate(lines):
            gutter_str = str(line_no + 1).rjust(gutter_size), fore('dark_gray')
            # split long lines across multiple
            if len(line) > term_y:
                for chunk_no, chunk in enumerate(wrap(line, term_y)):
                    if self._gutter:
                        result += f'{gutter_str} │ '
                    result += f'{chunk}\n'
                    if chunk_no == 0:
                        gutter_str = ''.rjust(gutter_size)
            else:
                # base case (wrap doesn't work on empty strings)
                if self._gutter:
                    result += f'{gutter_str} │ '
                result += f'{line}\n'

        return result