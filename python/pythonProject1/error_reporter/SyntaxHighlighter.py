from enum import Enum
from colored import stylize, fore, back

class TokenType(Enum):
    INVALID = 0
    WHITESPACE = 1
    COMMENT = 2
    ERROR = 3
    KEYWORD = 4
    TYPE = 5
    LITERAL = 6
    IDENTIFIER = 7
    OPERATOR = 8

    def getForeground(self):
        return [None, None, 'dark_gray', 'white', 'orange_1', 'blue', 'light_magenta', 'white', 'yellow'][int(self.value)]

    def getBackground(self):
        return [None, None, None, 'red', None, None, None, None, None][int(self.value)]

class Token:
    def __init__(self, text: str, type: TokenType):
        self._text = text
        self._type = type

    def text(self) -> str:
        return self._text

    def type(self) -> TokenType:
        return self._type

    def len(self) -> int:
        return len(self._text)

    def highlighted(self) -> str:
        style = ''
        if (color := self._type.getForeground()) is not None:
            style += fore(color)

        if (color := self._type.getBackground()) is not None:
            style += back(color)

        if len(style) > 0:
            return stylize(self._text, style)
        else:
            return self._text

    def split(self, i: int):
        if 0 > i or i > len(self._text):
            raise IndexError(f'Split index out of range for Token.split(...). i={i}, len(self._text)={len(self._text)}')

        return (Token(self._text[0:i], self._type), Token(self._text[i:len(self._text)], self._type))

    def __repr__(self) -> str:
        return f'Token(text = {repr(self._text)}, type = {self._type})'


class SyntaxHighlighter:

    def next_token(self, input: str) -> (str, Token):
        pass

    def reset(self):
        pass

    def tokens(self, input: str) -> [Token]:
        tokens = []

        while len(input) > 0:
            input, token = self.next_token(input)
            tokens.append(token)

        return tokens

    def chunks(self, input: str, size: int) -> [str]:
        tokens = self.tokens(input)
        # print(tokens)

        # split tokens according to the size of the line
        result = []
        curr_str = ''
        curr_len = 0  # length needs to be stored separately because len(str) includes ANSI color codes
        while len(tokens) > 0:
            while curr_len < size and len(tokens) > 0:
                token = tokens.pop(0)
                if new_len := curr_len + token.len() > size:
                    token, replacement = token.split(token.len() - (new_len - size))
                    tokens.insert(0, replacement)

                curr_str += token.highlighted()

            result.append(curr_str)
            curr_str = ''
            curr_len = 0

        if len(result) == 0:
            result.append('')

        return result


class QafnyHighlighter(SyntaxHighlighter):

    _KEYWORDS = ['method', 'include', 'function', 'if', 'while', 'requires', 'ensures', 'invariant']
    _TYPES = ['had', 'nor', 'nat']
    _OPERATORS = ['+', '-', '*', '/', '=', '!', '<', '>', '.', '{', '}', '[', ']', '(', ')', ',', ':', '↦', '⊗', '∈']

    class Mode(Enum):
        DEFAULT = 1
        MULTILINE_COMMENT = 2

    def __init__(self):
        self._mode = QafnyHighlighter.Mode.DEFAULT

    def reset(self):
        self._mode = QafnyHighlighter.Mode.DEFAULT

    def next_token(self, input: str) -> (str, Token):
        token_text = ''
        token_type = None

        if self._mode == QafnyHighlighter.Mode.DEFAULT:
            if input[0].isspace():
                # consume all whitespace
                token_type = TokenType.WHITESPACE
                last_ws = 1
                while last_ws < len(input) and input[last_ws].isspace():
                    last_ws += 1

                token_text = input[0:last_ws]
            elif input[0].isdigit():
                # consume number
                token_type = TokenType.LITERAL
                last_digit = 1
                while last_digit < len(input) and input[last_digit] in '0123456789.':
                    last_digit += 1

                token_text = input[0:last_digit]
            elif input[0] in self._OPERATORS:
                if input[0] == '/' and len(input) > 1:
                    # check if it's the start of a comment
                    if input[1] == '/':
                        # start of comment, eat until EOI
                        token_type = TokenType.COMMENT
                        token_text = input
                    elif input[1] == '*':
                        # start of multiline comment, eat until */ or EOI
                        token_type = TokenType.COMMENT
                        self._mode = QafnyHighlighter.Mode.MULTILINE_COMMENT
                        while last < len(input):
                            if input[last] == '/' and last - 1 >= 0 and input[last - 1] == '*':
                                self._mode = QafnyHighlighter.Mode.DEFAULT
                                last += 1
                                break
                            last += 1
                        token_text = input[:last]
                    else:
                        token_type = TokenType.OPERATOR
                        token_text = input[0]
                else:
                    token_type = TokenType.OPERATOR
                    token_text = input[0]
            elif input[0].isalpha():
                # identifier
                token_type = TokenType.IDENTIFIER
                last = 1
                while last < len(input) and input[last].isalnum():
                    last += 1
                token_text = input[:last]
            else:
                # unknown
                token_text = input[0]
                token_type = TokenType.INVALID

        elif self._mode == QafnyHighlighter.Mode.MULTILINE_COMMENT:
            # eat until end of input or */
            token_type = TokenType.COMMENT
            last = 0
            while last < len(input):
                if input[last] == '/' and last - 1 >= 0 and input[last - 1] == '*':
                    self._mode = QafnyHighlighter.Mode.DEFAULT
                    last += 1
                    break
                last += 1
            token_text = input[:last]

        if token_type == TokenType.IDENTIFIER:
            if token_text in self._KEYWORDS:
                token_type = TokenType.KEYWORD
            elif token_text in self._TYPES:
                token_type = TokenType.TYPE

        return (input[len(token_text):], Token(token_text, token_type))
