from enum import Enum
from os import get_terminal_size
from textwrap import wrap
from colored import fore, stylize


class ErrorReport:

    class Severity(Enum):
        WARNING = 1
        ERROR = 2

    def __init__(self, message: str, *, severity : Severity = Severity.ERROR, description: str = "", context: [str] = []):
        self.message = message
        self.severity = severity
        self.description = description
        self.context = context

    def withSeverity(self, severity: Severity):
        self.severity = severity
        return self

    def withDescription(self, description: str):
        self.description = description
        return self

    def withContext(self, context_line: [str]):
        self.context.append(context_line)
        return self

    def _color(self, string: str) -> str:
        """Stylizing a string corresponding to the accent color that corresponds to our severity"""
        if self.severity == ErrorReport.Severity.WARNING:
            style = fore('yellow')
        else:
            style = fore('red')

        return stylize(string, style)

    def __str__(self):
        term_y = get_terminal_size().columns

        if self.severity == ErrorReport.Severity.WARNING:
            prefix = 'Warning:'
        else:
            prefix = 'Error:'
        # ensure that the indentation stays the same for the block of text, since this is the first one, it doesn't need to be preceeded by \n
        result = '\n'.join(wrap(self.message, term_y, initial_indent = self._color(prefix) + ' ', subsequent_indent = ' ' * (len(prefix) + 1)))
        

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
            if i != len(self.context) - 1:
                prefix = '├─►'
            else:
                # last context message
                prefix = '╰─►'

            # ensure that the indentation stays the same for the block of text
            for chunk in wrap(context, term_y, initial_indent = ' ' + self._color(prefix) + ' ', subsequent_indent = '     '):
                result += '\n' + chunk

        return result


# testing
if __name__ == '__main__':
    print(ErrorReport("type mis-match", severity=ErrorReport.Severity.ERROR, description = 'Cannot convert seq<real> to nat type', context = ['See the type table in the paper', 'An extra long line of context that should wrap around the terminal. This will adapt to any terminal size, including the largest of terminals and the smallest of terminals.']))
    term_y = get_terminal_size().columns
    print('╞' + ('═' * (term_y - 2)) + '╡')
    print(ErrorReport('precondition could not be verified'))