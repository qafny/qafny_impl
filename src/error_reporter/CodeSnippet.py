from enum import Enum
from os import get_terminal_size
from textwrap import wrap
import bisect
import rich

from SyntaxHighlighter import QafnyHighlighter

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
        def __init__(self, start: int, end: int, message: str, color: str | int):
            if end < start:
                raise ValueError(f'The end of a context snippet cannot come after its start. (start = {start}, end = {end})')
            self._start = start
            self._end = end
            self._message = message
            self._color = color

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

        def color(self, msg: str = None) -> str:
            if msg is None:
                msg = self._message

            if self._color is not None:
                return f'[{self._color}]' + msg + '[/]'
            else:
                return msg

    def __init__(self, code: str, *, gutter: bool = True, highlight: bool = True, language: LanguageID = LanguageID.QAFNY, growth_with_context: bool = False):
        # A string representing the code in this file
        self._code = code
        # A flag indicating whether the gutter should be shown
        self._gutter = gutter
        # A flag indicating whether the code should be highlighted
        self._highlight = highlight
        # An enum indicating the language to use for syntax highlighting
        self._language = language
        # A string representing the name of the file that the code came from
        self._filename = None
        # A line number to attach to the name of the file
        self._line = None
        # A column number to attach to the name of the file
        self._column = None
        # A range to limit the lines that are printed
        self._range = range(len(self._code))
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

    def addContext(self, line: int, start_column: int, end_column: int, context: str, color: str = None):
        '''Add a message to the source code output.'''
        if not line in self._context:
            self._context[line] = []

        bisect.insort(self._context[line], CodeSnippet.Context(start_column, end_column, context, color), key=lambda x: x.start())
        return self

    def limitToLines(self, start: int, end: int):
        self._range = range(start, end + 1)
        return self

    def __rich__(self) -> str:
        """Returns a str renderable by any rich.print(...) method."""

        # stores the return value for this method
        result = ''

        # how much horizontal space we have to work with
        # used to split long lines of code onto multiple terminal lines whilst maintaining the gutter (space with line numbers)
        term_y = get_terminal_size().columns

        # the first line, the edge of the box
        result += '╭─'

        # the first line should include the filename and line:column if it is set
        id_parts = []
        if self._filename is not None:
            id_parts.append(self._filename)

        if self._line is not None:
            id_parts.append(str(self._line))

        if self._column is not None:
            id_parts.append(str(self._column))

        identifier = ':'.join(id_parts)
        if identifier:
            identifier = f'[yellow]{identifier}[/]' # stylize(identifier, fore('yellow'))
            identifier = f'[{identifier}]'
            result += identifier + '\n'
        else:
            result += '─────'

        # fill in the code
        lines = self._code.splitlines()
        
        # determine the padding on the left
        gutter_size = 0
        if self._gutter:
            # the number of columns the largest number takes up (to keep the numbers aligned)
            gutter_size = len(str(len(lines)))

            term_y -= gutter_size + 3 # the three extra spaces come from ' │ '
        else:
            term_y -= 2 # to account for the box sizing

        # create a syntax highlighter (if needed)
        highlighter = None
        if self._highlight:
            if self._language == CodeSnippet.LanguageID.DAFNY:
                highlighter = DafnyHighlighter()
            elif self._language == CodeSnippet.LanguageID.QAFNY:
                highlighter = QafnyHighlighter()

        # for every requested line
        for line_no in self._range:
            line = lines[line_no]
            gutter_str = '[bright_black]' + str(line_no + 1).rjust(gutter_size) + '[/]' # stylize(str(line_no + 1).rjust(gutter_size), fore('dark_gray'))

            chunks = []
            if self._highlight:
                # apply syntax highlighting
                chunks = highlighter.chunks(line, term_y)
            else:
                # split long lines across multiple terminal outputs
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
                            result += context.color('▲')
                        elif context.stretch() == 1:
                            # two characters ├┘
                            result += context.color('├┘')
                        else:
                            # enough space for a full highlight: └──┬───┘
                            result += context.color('└' + '─' * (context.center() - (context.start() + 1)) + '┬' + '─' * (context.end() - (context.center() + 1)) + '┘')

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
                                result += contexts[i].color('╰──')
                                if contexts[i].stretch == 0:
                                    result += contexts[i].color('─')
                                else:
                                    result += contexts[i].color('►')
                                result += ' '
                                result += contexts[i].color() # returns a colored version of the message

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
        if (len(lines) - 1) in self._range:
            result += '╰───────'
        else:
            result += '.'

        return result

if __name__ == '__main__':

    rich.print(CodeSnippet('''//Simple Had application
method hadtest(n: nat, q: Q[n])
  requires { q[0, n) : nor ↦ |0⟩ }
  ensures  { q[0, n) : had ↦ ⊗ i ∈ [0,n) . |+⟩ }
{
  // induce superposition on the first qubit
  assert { q[0, n) : nor ↦ |0⟩ };
  q[0,n) *= H;
  assert { q[0, n) : had ↦ ⊗ i ∈ [0,n) . |+⟩ };
  //assert { q[0] : had ↦ ⊗ i . (1) }; 
  // always take the ith qubit as the controller 
  //if (q[0]) { q[1]  *= λ (x => |(x + 1) % 2 ⟩); } //can be remove the % 2, since q[1] is one qubit, why the type checking cannot infer that the % 2 is unnecessary
}

//|-⟩ == 1/\\sqrt{2} (|0> - |1>)--> omega(1,2) == exp(2 pi i * 1/2)
//(|0⟩)^n --->  x: seq<bv1>, x:[0,n) ---> [0,0,0,0]
//|+⟩ --- 1/\\sqrt{2} (|0> + |1>) --> omega(0,2) == exp(2 pi i * 0/2) ===> exp(i x ) == cos x + i sin x
''').withFilename('test1.qfy').withLineColumn(2, 1).limitToLines(0, 5).addContext(1, 0, 5, 'unknown keyword \'method\'', 'red').__rich__())