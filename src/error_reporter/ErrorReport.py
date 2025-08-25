from enum import Enum
from os import get_terminal_size
from textwrap import wrap
import rich
from rich.text import Text
from rich.console import Console, ConsoleOptions, RenderResult


class ErrorReport:
    """
    ErrorReport represents a color highlighted error to be printed to the command-line via rich.print(...).

    Errors are formatted like:

    Error: type mis-match
     × Cannot convert seq<real> to nat type
     ├─► See the type table in the paper
     ╰─► An extra long line of context that should wrap around the terminal. This will adapt to any terminal size,
         including the largest of terminals and the smallest of terminals.

    Errors have a short description (the message), a longer description, a list of optional help/context messages and a severity (either Severity.ERROR or Severity.WARNING).

    Example Usage:
    rich.print(ErrorReport('Couldn't verify the state of q', description='q should've been EN type but was Had', ['See line 45 for the original declaration of q.', 'See the paper for more information on quantum typing.']))
    """

    class Severity(Enum):
        """
        Represents the severity of this error report. Either warning or error.
        """
        WARNING = 1
        ERROR = 2

    def __init__(self, message: str, *, severity : Severity = Severity.ERROR, description: str = "", context: [str] = []):
        '''
        Creates a new error report given a string. Named args include severity, description, and context.
        
        Arugments:
        * message (required) a string with a short description of the error.
        * severity  the severity of the error, either error or warning
        * description  a longer description of the error
        * context  an array of strings that provide helpful feedback/extra context to the error    
        '''
        self.message = message
        self.severity = severity
        self.description = description
        self.context = context

    def withSeverity(self, severity: Severity):
        '''Returns the same error report with a different severity. Useful for inline instatiation.'''
        self.severity = severity
        return self

    def withDescription(self, description: str):
        '''Returns the same error report with a different description. Useful for inline instatiation.'''
        self.description = description
        return self

    def withContext(self, context_line: [str]):
        '''Returns the same error report with more context lines. Useful for inline instatiation.'''
        self.context.append(context_line)
        return self

    def _color(self, string: str) -> str:
        '''Stylizing a string corresponding to the accent color that corresponds to our severity'''
        if self.severity == ErrorReport.Severity.WARNING:
            style = 'yellow'
        else:
            style = 'red'

        return f'[{style}]{string}[/]'

    def __rich_console__(self, console: Console, options: ConsoleOptions) -> RenderResult:
        # get the number of columns (necessary for line wrapping)
        term_y = get_terminal_size().columns

        # determine the prefix based on the error type
        if self.severity == ErrorReport.Severity.WARNING:
            prefix = 'Warning:'
        else:
            prefix = 'Error:'

        # ensure that the indentation stays the same for the block of text, since this is the first one, it doesn't need to be preceeded by \n
        result = '\n'.join(wrap(self.message, term_y, initial_indent = self._color(prefix) + ' ', subsequent_indent = ' ' * (len(prefix) + 1)))
        
        # If we have a description, output it next to its symbol
        if self.description:
            if self.severity == ErrorReport.Severity.WARNING:
                # todo: choose a better symbol
                symbol = '⚠'
            else:
                symbol = '×'

            # ensure that the indentation stays the same for the block of text
            for chunk in wrap(self.description, term_y, initial_indent = ' ' + self._color(symbol) + ' ', subsequent_indent = '   '):
                result += '\n' + chunk

        for i, context in enumerate(self.context):
            indent_after = ''
            if i != len(self.context) - 1:
                # everything but the last context message
                prefix = '├─►'
                indent_after = '│  '
            else:
                # last context message
                prefix = '╰─►'
                indent_after = '   '

            # ensure that the indentation stays the same for the block of text
            for j, chunk in enumerate(wrap(context, term_y - 5)): # , initial_indent = ' ' + self._color(prefix) + ' ', subsequent_indent = ' ' + self._color(indent_after) + ' '):
                result += '\n'
                # use the prefix for the first chunk and indent_after for everything after that.
                if j == 0:
                    result += ' ' + self._color(prefix) + ' '
                else:
                    result += ' ' + self._color(indent_after) + ' '
                result += chunk

        yield Text.from_markup(result)


# testing
if __name__ == '__main__':
    rich.print(ErrorReport("type mis-match", severity=ErrorReport.Severity.ERROR, description = 'Cannot convert seq<real> to nat type', context = ['See the type table in the paper. This table includes a description of a variety of types, including the nat and real types, which were used in this error.', 'An extra long line of context that should wrap around the terminal. This will adapt to any terminal size, including the largest of terminals and the smallest of terminals.']))
    # term_y = get_terminal_size().columns
    # print('╞' + ('═' * (term_y - 2)) + '╡')
    # print(ErrorReport('precondition could not be verified'))