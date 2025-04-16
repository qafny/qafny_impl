from enum import Enum
from colored import fore, stylize
from os import get_terminal_size
from textwrap import wrap
import bisect

class CodeSnippet:
    '''
    Represents a human-readable (and syntax highlighted) snippet of code.

    Features:
     * Syntax highlighting
     * Contextual highlights
     * Code numbering
     * Source code name/numbering

    ╭─[test2.qfy:2:5]────────────────────────────────────────────────────╮
    │ 1 method hadtest(n: nat, q : Q[n])                                 │
    │ 2   requires { q[0, n) : nor ↦ |0⟩ }                               │
    ·     └──┬───┘   ▲                └┬┘  ┌ this specific part of the   ·
    ·        │       │                 ╰───┤ precondition could not be   ·
    ·        │       │                     └ proven.                     ·
    ·        │       ╰──────── check bounds for this variable            ·
    ·        ╰───────► this precondition was not satisfied               ·
    │ 3   ensures  { q[0, n) : en ↦ ∑ k ∈ [0, 2^n) . 1/sqrt(2^n) | k ⟩ }│
    │ 4 {                                                                │
    ╰────────────────────────────────────────────────────────────────────╯ 
    '''

    class LanguageID(Enum):
        DAFNY = 1
        QAFNY = 2

    class Context:
        def __init__(self, start: int, end: int, message: str):
            if end < start:
                raise ValueError(f'The end of a context snippet cannot come after its start. (start = {start}, end = {end})')
            self._start = start
            self._end = end
            self._message = message

        def start(self) -> int:
            return self._start

        def end(self) -> int:
            return self._end

        def message(self) -> int:
            return self._message

        def center(self) -> int:
            '''Returns the integer in the center of this context'''
            return int((self._start + self._end) / 2)

        def stretch(self) -> int:
            '''Returns how far the range attached to this context stretches'''
            return self._end - self._start

    def __init__(self, code: str, *, gutter: bool = True, highlight: bool = True, language: LanguageID = LanguageID.QAFNY, growth_with_context: bool = False):
        self._code = code
        self._gutter = gutter
        self._highlight = highlight
        self._language = language
        self._filename = None
        self._line = None
        self._column = None
        # empty dictionary of context
        self._context = {}
        # whether the displayed lines should match up with the additional context
        self._growWithContext = growth_with_context

    def shouldShowGutter(gutter: bool):
        self._gutter = gutter
        return self

    def shouldHighlight(highlight: bool):
        self._highlight = highlight
        return self

    def setLanguage(langauge: LanguageID):
        self._language = language
        return self

    def withFilename(self, filename: str):
        self._filename = filename
        return self

    def withLineColumn(self, line: int, column: int):
        self._line = line
        self._column = column
        return self

    def addContext(self, line: int, start_column: int, end_column: int, context: str):
        '''Add a message to the source code output.'''
        if not line in self._context:
            self._context[line] = []

        bisect.insort(self._context[line], CodeSnippet.Context(start_column, end_column, context), key=lambda x: x.start())
        return self

    def limitToLines(self, start: int, end: int):
        return self

    def __str__(self):
        """Creates a string from this code report, presumably to print on the console"""
        # stores the return value for this method
        result = ''

        # how much horizontal space we have to work with
        # used to split long lines of code onto multiple terminal lines whilst maintaining the gutter (space with line numbers)
        term_y = get_terminal_size().columns

        # the first line, the edge of the box
        result += '╭─'

        # the first line should include the filename and line:column if it is set
        identifier = ':'.join(filter(None, [self._filename, str(self._line), str(self._column)]))
        if identifier:
            identifier = f'[{identifier}]'
            result += identifier + '\n'
        else:
            result += '─────'

        # fill in the code
        lines = self._code.splitlines()
        
        gutter_size = 0
        if self._gutter:
            # the number of columns the largest number takes up (to keep the numbers aligned)
            gutter_size = len(str(len(lines)))

            term_y -= gutter_size + 3 # the three extra spaces come from ' │ '
        else:
            term_y -= 2 # to account for the box sizing

        for line_no, line in enumerate(lines):
            gutter_str = stylize(str(line_no + 1).rjust(gutter_size), fore('dark_gray'))
            # split long lines across multiple
            chunks = wrap(line, term_y)

            # wrap will eat empty strings
            if len(chunks) == 0:
                chunks = ['']

            # loop through each chunk
            chunk_end = 0
            for chunk_no, chunk in enumerate(chunks):
                    # display the gutter
                    if self._gutter:
                        result += f'│ {gutter_str} '
                    else:
                        result += '│ '

                    result += f'{chunk}\n'

                    # update variables required by context
                    chunk_start = chunk_end
                    chunk_end += len(chunk)

                    # do we have context on this line in a range we just outputted?
                    if line_no in self._context:
                        # is the context in the range we just outputted?
                        contexts = list(filter(lambda context: context.start() < chunk_end, self._context[line_no]))
                        
                        # output the underlines under the sections of the line that pertain to the context
                        # left padding to account for the gutter / box
                        empty_space = ''.rjust(gutter_size + 1) if self._gutter else ''
                        result += f'· {empty_space}' 
                        current_pos = chunk_start
                        for context in contexts:
                            # add whitespace from our current position up to context start
                            result += ' ' * (context.start() - current_pos)
                            # add the underscore
                            if context.stretch() == 0:
                                # single character ┬
                                result += '▲'
                            elif context.stretch() == 1:
                                # two characters ├┘
                                result += '├┘'
                            else:
                                # enough space for a full highlight: └──┬───┘
                                result += '└' + '─' * (context.center() - (context.start() + 1)) + '┬' + '─' * (context.end() - (context.center() + 1)) + '┘'

                            current_pos = context.end()
                        result += '\n'

                        # output each context message on new lines, ensuring lines wrap correctly and don't end up squished against the right side
                        while len(contexts) > 0:
                            result += f'. {empty_space}'
                            current_pos = chunk_start
                            for i in range(0, len(contexts)):
                                # add white space from our current position up to context start
                                result += ' ' * (contexts[i].center() - current_pos)
                                if i == len(contexts) - 1:
                                    # last one in the list, output it
                                    result += '╰──'
                                    if contexts[i].stretch == 0:
                                        result += '─'
                                    else:
                                        result += '►'
                                    result += ' '
                                    result += contexts[i].message()

                                else:
                                    # just output the │ continuation
                                    result += '│'

                                current_pos = contexts[i].center() + 1

                            result += '\n'

                            contexts.pop()

                    # remove the line number after the first chunk
                    if chunk_no == 0:
                        gutter_str = ''.rjust(gutter_size)

        # ending line break
        result += '╰───────'

        return result

if __name__ == '__main__':
    print(CodeSnippet('''//Simple Had application
method hadtest(n: nat, q: Q[n])
  requires { q[0, n) : nor ↦ |0⟩ }
  ensures  { q [0,n) : had ↦ ⊗ i ∈ [0,n) . |+⟩ }
{
  // induce superposition on the first qubit
  assert { q[0, n) : nor ↦ |0⟩ };
  q[0,n) *= H;
  assert { q [0,n) : had ↦ ⊗ i ∈ [0,n) . |+⟩ };
  //assert { q [0] : had ↦ ⊗ i . (1) }; 
  // always take the ith qubit as the controller 
  //if (q[0]) { q[1]  *= λ (x => |(x + 1) % 2 ⟩); } //can be remove the % 2, since q[1] is one qubit, why the type checking cannot infer that the % 2 is unnecessary
}

//|-⟩ == 1/\\sqrt{2} (|0> - |1>)--> omega(1,2) == exp(2 pi i * 1/2)
//(|0⟩)^n --->  x: seq<bv1>, x:[0,n) ---> [0,0,0,0]
//|+⟩ --- 1/\\sqrt{2} (|0> + |1>) --> omega(0,2) == exp(2 pi i * 0/2) ===> exp(i x ) == cos x + i sin x
''').withFilename('test1.qfy').withLineColumn(2, 1).limitToLines(0, 5).addContext(1, 0, 5, 'unknown keyword \'method\''))