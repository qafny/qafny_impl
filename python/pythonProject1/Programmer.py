import AbstractProgramVisitor

import utils # for make_repr(...)
from utils import hasmembervariable

from typing import (
    Callable,
    Optional,
    Tuple,
    Type,
    TypeVar,
    Union
)

import antlr4 # for antlr4.tree.Tree.TerminalNodeImpl

import rich.repr # for pretty printing (see qafny.rich.repr.auto)
import inspect # for auto-generating __rich_repr__


# ────────── Types ──────────
id_t = Union[str, antlr4.tree.Tree.TerminalNodeImpl]
T = TypeVar("T")
V = TypeVar("V")


# ────────── Helpers/Utilities ──────────
class qafny:
    '''qafny namespace'''
    class auto:
        '''auto namespace'''

        # A class decorator that generates the required methods for rich.repr
        def rich_repr(cls: Optional[Type[TypeVar("T")]]) -> Union[Type[TypeVar("T")], Callable[[Type[TypeVar("T")]], Type[TypeVar("T")]]]:
            '''
            Class decorator to create __repr__ from __rich_repr__.
            Copied from https://github.com/Textualize/rich/blob/master/rich/repr.py
            '''
            def do_replace(cls: Type[TypeVar("T")]) -> Type[TypeVar("T")]:
                '''Actual function that replaces the member functions of a class, could be returned as a partial from rich_repr'''
                def auto_repr(self: TypeVar("T")) -> str:
                    '''Create repr string from __rich_repr__'''
                    repr_str: List[str] = []
                    append = repr_str.append
                    for arg in self.__rich_repr__():  # type: ignore[attr-defined]
                        if isinstance(arg, tuple):
                            if len(arg) == 1:
                                append(repr(arg[0]))
                            else:
                                key, value, *default = arg
                                if key is None:
                                    append(repr(value))
                                else:
                                    if default and default[0] == value:
                                        continue
                                    append(f"{key}={value!r}")
                        else:
                            append(repr(arg))
                    return f"{self.__class__.__name__}({', '.join(repr_str)})"

                def auto_rich_repr(self: Type[TypeVar("T")]) -> rich.repr.Result:
                    '''Auto generate __rich_rep__ from signature of __init__'''
                    try:
                        signature = inspect.signature(self.__init__)
                        for name, param in signature.parameters.items():
                            if param.kind == param.POSITIONAL_ONLY:
                                if hasmembervariable(self, name):
                                    yield getattr(self, name)
                                elif hasmembervariable(self, '_' + name):
                                    yield getattr(self, '_' + name)
                            elif param.kind in (
                                param.POSITIONAL_OR_KEYWORD,
                                param.KEYWORD_ONLY,
                            ):
                                if param.default is param.empty:
                                    if hasmembervariable(self, param.name):
                                        yield getattr(self, param.name)
                                    elif hasmembervariable(self, '_' + param.name):
                                        yield getattr(self, '_' + param.name)
                                else:
                                    if hasmembervariable(self, param.name):
                                        yield param.name, getattr(self, param.name), param.default
                                    elif hasmembervariable(self, '_' + param.name):
                                        yield param.name, getattr(self, '_' + param.name), param.default
                    except Exception as error:
                        raise ValueError(
                            f"Failed to auto generate __rich_repr__; {error}"
                        ) from None
                if not hasattr(cls, "__rich_repr__"):
                    auto_rich_repr.__doc__ = "Build a rich repr"
                    cls.__rich_repr__ = auto_rich_repr  # type: ignore[assignment]
                if not hasattr(cls, "__repr__"):
                    auto_repr.__doc__ = "Return repr(self)"
                    cls.__repr__ = auto_repr  # type: ignore[assignment]
                return cls
            if cls is None:
                return partial(do_replace)
            else:
                return do_replace(cls)

        # A decorator to mark certain values as unnecessary for the auto-equality decorator
        def equality_ignore(*args: str) -> Union[Type[T], Callable[[Type[T]], Type[T]]]:
            # collect all arguments into an array
            ignored_cases = args

            def _equality_ignore(cls: Optional[Type[T]]) -> Union[Type[T], Callable[[Type[T]], Type[T]]]:
                def do_insert(cls: Type[T]) -> Type[T]:
                    '''The actual function that inserts information on the class'''
                    if not hasattr(cls, '__EQ_IGNORE_MEMBERS_oGiHKHnSCvOLx50N__'):
                        cls.__EQ_IGNORE_MEMBERS_oGiHKHnSCvOLx50N__ = ignored_cases
                    else:
                        cls.__EQ_IGNORE_MEMBERS_oGiHKHnSCvOLx50N__.append(ignored_cases)

                    return cls

                if cls is None:
                    return partial(do_insert)
                else:
                    return do_insert(cls)

            return _equality_ignore


        # A decorator that generates __eq__ and __ne__
        def equality(cls: Optional[Type[T]]) -> Union[Type[T], Callable[[Type[T]], Type[T]]]:
            '''Generates an equality (__eq__) member function for custom AST types'''
            def do_replace(cls: Type[T]) -> Type[T]:
                '''The actual function that interacts with the type to update its methods'''
                
                def eq(self: T, other: V) -> bool:
                    if not isinstance(other, self.__class__):
                        # classes differ
                        return False

                    # go through every property (unless marked with skip (i.e. source location information))
                    try:
                        def is_acceptable_member_variable(obj: T, name: str):
                            # check if this obj has member variables marked as ignored
                            if hasattr(obj, '__EQ_IGNORE_MEMBERS_oGiHKHnSCvOLx50N__') and name in obj.__EQ_IGNORE_MEMBERS_oGiHKHnSCvOLx50N__:
                                return False
                            return not callable(getattr(obj, name)) and not (name.startswith('__') and name.endswith('__'))

                        members = [attr for attr in dir(self) if is_acceptable_member_variable(self, attr)]
                        for member in members:
                            if not (getattr(self, member) == getattr(other, member)):
                                return False

                    except Exception as error:
                        raise ValueError(f'Failed to auto generate __eq__; {error}') from None

                    return True

                def ne(self: T, other: V) -> bool:
                    return not (self == other)

                eq.__doc__ = 'Check equality between two objects.'
                cls.__eq__ = eq # type: ignore[attr-defined]

                ne.__doc__ = 'Check inequality between two objects'
                cls.__ne__ = ne # type: ignore[attr-defined]

                return cls

            if cls is None:
                return partial(do_replace)
            else:
                return do_replace(cls)


def isAntlrNode(obj):
    return isinstance(obj, antlr4.tree.Tree.TerminalNodeImpl)


def coerceStr(obj):
    return obj.getText() if isAntlrNode(obj) else str(obj)


# ────────── Qafny's AST ──────────

@qafny.auto.equality_ignore('_line', '_col', '_range')
class QXTop:
    '''
    Parent class of all Qafny tree nodes.
    All nodes contain information about their location in the source file.
    This is stored in line() and col() to give the first line and column number in the source file. These both start at one
    range() will return a tuple of the first index (in the source string) and the last index (in the source string) that indicates where this tree node originates from.
    '''

    def __init__(self, line: Optional[int] = None, col: Optional[int] = None, range: Tuple[int, int] = None, source: str = None, parser_context: Optional[Union[antlr4.ParserRuleContext, antlr4.TerminalNode]] = None):
        '''If source and range is not none, line and col can be calculated. If antlr_parser_context is provided, line, col, and range can be autogenerated'''
        if parser_context is not None:
            if isinstance(parser_context, antlr4.ParserRuleContext):
                start_token = parser_context.start
                end_token = parser_context.stop
            elif isinstance(parser_context, antlr4.TerminalNode):
                start_token = parser_context.getSymbol()
                end_token = parser_context.getSymbol()
            else:
                raise ValueError(f'parser_context should be either antlr4.ParserRuleContext or antlr4.TerminalNode but was: {type(parser_context)}')

            if line is None:
                line = start_token.line
            if col is None:
                col = start_token.column
            if range is None:
                range = (start_token.start, end_token.stop)

        if line is None and range is not None and source is not None:
            line = source.count('\n', 0, range[0]) + 1

        if col is None and range is not None and source is not None:
            col = len(source[source.rfind('\n', range[0]):range[0]])

        self._line = line
        self._col = col
        self._range = range

    def line(self):
        '''Returns the line number that this AST node starts on. If it is a multiline node, this returns the first line number.'''
        return self._line

    def setLine(self, line: int):
        '''A setter for the line property.'''
        self._line = line

    def col(self):
        '''Returns the column that this AST node starts at. If it inhabits multiple columns (anything over one character) it returns the first column.'''
        return self._col

    def setCol(self, col: int):
        '''A setter for the column property.'''
        self._col = col

    def range(self) -> Tuple[int, int]:
        '''Returns the index of the first and last character associated with this AST node in the original source string.'''
        return self._range

    def setRange(self, range: Tuple[int, int]):
        '''A setter for the range property.'''
        self._range = range

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class QXType(QXTop):
    '''
    QXType refers kinds.
    This is the base class for all qafny types.
    '''

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class QXQExp(QXTop):
    '''This is the base class for all qafny ket expressions'''

    def accept(self, visitor: AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXHad(QXQExp):
    '''A hadmard state (+ or -)'''

    def __init__(self, state: str, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._state = state.getText() if isAntlrNode(state) else state

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitHad(self)

    def state(self):
        return self._state

    def __repr__(self):
        return f"QXHad(state={repr(str(self._state))})"


class QXAExp(QXQExp, QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class TyArray(QXType):
    '''
    Represents the array type, which contains an inner type and potentially a size.
    '''

    def __init__(self, type: QXType, flag: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._type = type
        self._flag = flag

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitArrayT(self)

    def type(self):
        return self._type

    def num(self):
        return self._flag

    def __repr__(self):
        return f"TyArray(ty={self._type}, flag={self._flag})"

    def __rich_repr__(self) -> rich.repr.Result:
        yield self._type
        yield self._flag


@qafny.auto.rich_repr
@qafny.auto.equality
class TySet(QXType):
    '''
    Represents a set in Qafny: set<xxx>, directly analogous to a Dafny set.
    '''

    def __init__(self, type: QXType, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._type = type

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitTySet(self)

    def type(self):
        return self._type

    def __repr__(self):
        return f'TySet(ty={self._type})'


@qafny.auto.rich_repr
@qafny.auto.equality
class TySingle(QXType):
    '''
    Represents non-quantum types as a string indicating the type. i.e. 'nat' or 'bool'
    '''

    def __init__(self, name: str, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._name = name.getText() if isAntlrNode(name) else name

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSingleT(self)

    def type(self):
        return str(self._name) if self._name else None

    def __repr__(self):
        return f"TySingle(name={repr(str(self._name))})"


@qafny.auto.rich_repr
@qafny.auto.equality
class TyQ(QXType):
    '''
    Represents the q-bit string type.
    '''

    def __init__(self, flag: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._flag = flag

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQ(self)

    def flag(self):
        return self._flag

    def __repr__(self):
        return f"TyQ(flag={self._flag})"


@qafny.auto.rich_repr
@qafny.auto.equality
class TyFun(QXType):
    '''
    Represents a function type, which has a number of parameters and a single return type.
    '''

    def __init__(self, parameters: [QXType], return_type: QXType, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._parameters = parameters
        self._return_type = return_type

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitFun(self)

    def params(self):
        return self._parameters

    def return_type(self):
        return self._return_type

    def __repr__(self):
        return f"TyFun(parameters={self._parameters}, return_type={self._return_type})"


class QXQTy(QXTop):
    '''QXQTy refers to actual quantum types. This is the base class for all custom quantum types'''

    def accept(self, visitor: AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class TyHad(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitTyHad(self)

    def __repr__(self):
        return f"TyHad()"


@qafny.auto.rich_repr
@qafny.auto.equality
class TyEn(QXQTy):

    def __init__(self, flag: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._flag = flag

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitEn(self)

    def flag(self):
        return self._flag

    def __repr__(self):
        return f"TyEn(flag={self._flag})"


# Specialized version of the EN type where the grouping of basis vectors are important
@qafny.auto.rich_repr
@qafny.auto.equality
class TyAA(QXQTy):

    def __init__(self, qrange = None, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._qrange = qrange

    def qrange(self):
        return self._qrange

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitAA(self)

    def __repr__(self):
        return f"TyAA(qrange={self._qrange})"


@qafny.auto.rich_repr
@qafny.auto.equality
class TyNor(QXQTy):

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitNor(self)

    def __repr__(self):
        return f"TyNor()"


class QXBExp(QXTop):

    def accept(self, visitor: AbstractProgramVisitor):
        pass


class QXSpec(QXTop):

    def accept(self, visitor: AbstractProgramVisitor):
        pass

class QXCond(QXTop):

    def accept(self, visitor: AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXBind(QXAExp):

    def __init__(self, id: str, type: QXType = None, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = coerceStr(id)
        self._type = type

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitBind(self)

    def ID(self):
        # return self._id if self._id is str else self._id.getText()
        return self._id

    def type(self):
        return self._type

    def __repr__(self):
        return utils.make_repr('QXBind', {'id': self._id, 'ty': self._type})


class QXBool(QXBExp, QXSpec):

    def accept(self, visitor: AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXLogic(QXBool):

    def __init__(self, op: str, left: QXBool, right: QXBool, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._op = op.getText() if isAntlrNode(op) else op
        self._left = left
        self._right = right

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitLogic(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"QXLogic(op={repr(str(self._op))}, left={self._left}, right={self._right})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXCNot(QXBool):

    def __init__(self, next: QXBool, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._next = next

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitCNot(self)

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXCNot(next={self._next})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXComp(QXBool):

    def __init__(self, op: str, left: QXAExp, right: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._op = op.getText() if isAntlrNode(op) else op
        self._left = left
        self._right = right

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitBool(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"QXComp(op={repr(str(self._op))}, left={self._left}, right={self._right})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXAll(QXSpec):

    def __init__(self, bind: QXBind, bounds: QXComp, next: QXSpec, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._bind = bind
        self._bounds = bounds
        self._next = next

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitAll(self)

    def bind(self):
        return self._bind

    def bounds(self):
        return self._bounds

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXAll(bind={self._bind}, bounds={self._bounds} next={self._next})"


class QXQBool(QXBExp):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXQIndex(QXQBool, QXAExp):

    def __init__(self, id: str, index: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._index = index

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQIndex(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def index(self):
        return self._index

    def __repr__(self):
        return f"QXQindex(id={repr(str(self._id))}, index={self._index})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXIfExp(QXAExp):

    def __init__(self, bexp: QXBExp, left: QXAExp, right: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._bexp = bexp
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitIfExp(self)

    def bexp(self):
        return self._bexp

    def left(self):
        return self._left

    def right(self):
        return self._right


@qafny.auto.rich_repr
@qafny.auto.equality
class QXBin(QXAExp):

    def __init__(self, op: str, left: QXAExp, right: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._op = op.getText() if isAntlrNode(op) else op
        self._left = left
        self._right = right

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitBin(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def __repr__(self):
        return f"QXBin(op={repr(str(self._op))}, left={self._left}, right={self._right})"

@qafny.auto.rich_repr
@qafny.auto.equality
class QXCRange(QXTop):

    def __init__(self, left: Optional[QXAExp], right: Optional[QXAExp], parser_context: Optional[antlr4.ParserRuleContext] = None):
        super().__init__(parser_context=parser_context)
        self._left = left
        self._right = right

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCRange(self)

    def left(self) -> Optional[QXAExp]:
        '''
        The start (inclusive) of this range. If none, it starts at the first element of the respective sequence.

        A range with no start nor end would indicate all of the elements in the sequence.
        '''
        return self._left

    def right(self) -> Optional[QXAExp]:
        '''
        The end (exclusive) of this range. If none, the range ends at the last element of the respective seqence + 1.
        
        A range with no start nor end would indicate all of the elements in the sequence.
        '''
        return self._right

    def __repr__(self) -> str:
        return f"QXCRange(left={self._left}, right={self._right})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXQRange(QXTop):

    def __init__(self, location: str, index: QXAExp = None, crange: QXCRange = None, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._location = location
        self._index = index
        self._crange = crange

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQRange(self)

    def location(self):
        '''Can be a str or a QXCall'''
        return self._location

    def index(self):
        '''If the range includes an index, i.e. q[0][0, n), it is accessed here. Note: this member is only set for 2-d arrays, q[0] will transform into a crange {q[0, 1)}'''
        return self._index

    def crange(self):
        '''The crange associated with this QXQRange'''
        return self._crange

    def __repr__(self):
        return f"QXQRange(location={repr(str(self._location))}, crange={self._crange})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXCon(QXTop):

    def __init__(self, id: str, crange: QXCRange, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._crange = crange
        # self._condition = condition

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def range(self):
        return self._crange

    # def condition(self):
    #    return self._condition

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCon(self)

    def __repr__(self):
        return f"QXCon(id={repr(str(self._id))}, crange={self._crange})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXUni(QXAExp):

    def __init__(self, op: str, next:QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._op = op.getText() if isAntlrNode(op) else op
        self._next = next

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitUni(self)

    def op(self):
        return self._op

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXUni(op={repr(str(self._op))}, next={self._next})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXNum(QXAExp):
    '''
    Represents a number literal. Can either be an integer number or a float.
    '''

    def __init__(self, num: Union[float, int], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._num = num

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitNum(self)

    def num(self) -> Union[float, int]:
        return self._num

    def __repr__(self):
        return f"QXNum(num={self._num})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXBoolLiteral(QXAExp):

    def __init__(self, value: bool, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._value = value

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitBoolLiteral(self)

    def value(self):
        return self._value

    def __repr__(self):
        return f'QXBoolLiteral(value={self._value})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXSet(QXAExp):

    def __init__(self, members: [QXAExp], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._members = members

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitSet(self)

    def members(self):
        return self._members

    def __repr__(self):
        return f'QXSet(members={self._members})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXMemberAccess(QXAExp):
    '''
    Represents accessing a variables member variable, separated by a dot.
    IDs represent each id in the chain (i.e. a line reading map.Keys.Length would transform to an ids array of ['map', 'Keys', 'Length']).

    example:
    ╭────────────────────────╮
    │ requires a.Length == n │
    ╰────────────────────────╯
    '''

    def __init__(self, ids: [str], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._ids = ids

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitMemberAccess(self)

    def ids():
        return self._ids

    def __repr__(self):
        return f'QXMemberAccess(ids={self._ids})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXNegation(QXAExp):

    def __init__(self, aexp: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._aexp = aexp

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitNegation(self)

    def aexp(self):
        return self._aexp

    def __repr__(self):
        return f'QXNegation(aexp={self._aexp})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXSumAExp(QXAExp):

    def __init__(self, sum: QXCon, aexp: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._sum = sum
        self._aexp = aexp

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitSumAExp(self)

    def sum(self):
        return self._sum

    def aexp(self):
        return self._aexp

    def __repr__(self):
        return f'QXSumAExp(sum={self._sum}, aexp={self._aexp})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXQComp(QXQBool):

    def __init__(self, op: str, left:QXAExp, right: QXAExp, index: QXQIndex, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._op = op.getText() if isAntlrNode(op) else op
        self._left = left
        self._right = right
        self._index = index

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitQComp(self)

    def op(self):
        return self._op

    def left(self):
        return self._left

    def right(self):
        return self._right

    def index(self):
        return self._index

    def __repr__(self):
        return f"QXQComp(op={repr(str(self._op))}, left={self._left}, right={self._right}, index={self._index})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXQNot(QXQBool):

    def __init__(self, next: QXQBool, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._next = next

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitQNot(self)

    def next(self):
        return self._next

    def __repr__(self):
        return f"QXQNot(next={self._next})"


class QXExp(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXSingle(QXExp):

    def __init__(self, op: str, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._op = op.getText() if isAntlrNode(op) else op

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSingle(self)

    def op(self):
        return self._op

    def __repr__(self):
        return f"QXSingle(op={repr(str(self._op))})"


class QXKet(QXTop):

    def accept(self, visitor : AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXSKet(QXKet):

    def __init__(self, vector: QXQExp, negative: bool = False, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._vector = vector
        self._negative = negative

    def vector(self):
        return self._vector

    def negative(self):
        return self._negative

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitSKet(self)

    def __repr__(self):
        return f"QXSKet(vector={self._vector})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXVKet(QXKet):

    def __init__(self, vector: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._vector = vector

    def vector(self):
        return self._vector

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitVKet(self)

    def __repr__(self):
        return f"QXVKet(vector={self._vector})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXOracle(QXExp):

    def __init__(self, bindings: [QXBind], amp: QXAExp, kets: [QXKet], inverse: bool = False, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._bindings = bindings
        self._amp = amp
        self._kets = kets
        self._inverse = inverse

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitOracle(self)

    def bindings(self):
        return self._bindings

    def phase(self):
        return self._amp

    def amp(self):
        return self._amp

    def vectors(self):
        return self._kets

    def inverse(self):
        return self._inverse

    def __repr__(self):
        return f"QXOracle(bindings={self._bindings}, amp={self._amp}, kets={self._kets}, inverse={self._inverse})"


class QXQState(QXTop):

    def accept(self, visitor: AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXTensor(QXQState):

    def __init__(self, kets: [QXKet], id: str = None, crange: QXCRange = None, amp: QXAExp = None, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._kets = kets
        self._id = id.getText() if isAntlrNode(id) else id
        self._crange = crange
        self._amp = amp

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitTensor(self)

    def ID(self):
        return str(self._id) if self._id else None

    def range(self):
        return self._crange

    def kets(self):
        return self._kets

    def amp(self):
        return self._amp

    def __repr__(self):
        return f"QXTensor(kets={self._kets}, id={repr(str(self._id))}, crange={self._crange}, amp={self._amp})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXSum(QXQState):

    def __init__(self, sums: [QXCon], amp: QXAExp, kets: [QXKet], condition: QXBExp = None, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._sums = sums
        self._amp = amp
        self._kets = kets
        self._condition = condition

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitSum(self)

    def sums(self):
        return self._sums

    def amp(self):
        return self._amp

    def kets(self):
        return self._kets

    def condition(self):
        '''Returns the condition of this sum, indicating when it should be tested against. Most conditions will include variables from the QXCon'''
        return self._condition

    def __repr__(self):
        return f"QXSum(sums={self._sums}, amp={self._amp}, kets={self._kets}, condition={self._condition})"

class QXStmt(QXTop):
    '''Parent class of all statements.'''

    def accept(self, visitor: AbstractProgramVisitor):
        pass


@qafny.auto.rich_repr
@qafny.auto.equality
class QXAssert(QXStmt):

    def __init__(self, spec: QXSpec, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._spec = spec

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitAssert(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXAssert(spec={self._spec})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXCast(QXStmt):

    def __init__(self, qty :QXQTy, locus: [QXQRange], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._qty = qty
        self._locus = locus

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCast(self)

    def qty(self):
        return self._qty

    def locus(self):
        return self._locus

    def __repr__(self):
        return f"QXCast(qty={self._qty}, locus={self._locus})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXInit(QXStmt):

    def __init__(self, binding: QXBind, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._binding = binding

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitInit(self)

    def binding(self):
        return self._binding

    def __repr__(self):
        return f"QXInit(binding={self._binding})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXCAssign(QXStmt):

    def __init__(self, ids: [Union[QXBind, QXQIndex]], expr : QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._ids = ids
        self._expr = expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCAssign(self)

    def ids(self):
        return self._ids

    def aexp(self):
        return self._expr

    def __repr__(self):
        return f"QXCAssign(id={repr(str(self._ids))}, expr={self._expr})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXQAssign(QXStmt):
    '''
    Represents a quantum assignment operation.
    '''

    def __init__(self, location: Union[list[QXQRange], str], expr : QXExp, parser_context: antlr4.ParserRuleContext = None):
        '''
        location - either a QXQRange or an identifier indicating the variable to transform.
        expr - the operation to apply to the variable
        '''
        super().__init__(parser_context=parser_context)
        self._location = location
        self._expr = expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitQAssign(self)

    def locus(self):
        '''DEPRECATED'''
        return self._location

    def location(self):
        return self._location

    def exp(self):
        return self._expr

    def __repr__(self):
        return f"QXQAssign(location={self._location}, expr={self._expr})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXQCreate(QXStmt):
    '''
    Represents a statement that creates a q-bit string of a certain length.

    example:
    ╭────────────────────────╮
    │ var p[0,n) *= init(n); │
    ╰────────────────────────╯
    '''

    def __init__(self, qrange: QXQRange, size: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._qrange = qrange
        self._size = size

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitQCreate(self)

    def qrange(self):
        return self._qrange

    def size(self):
        return self._size

    def __repr__(self):
        return f'QXQCreate(locus={self._qrange}, size={self._size})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXMeasure(QXStmt):

    def __init__(self, ids: [str | QXQIndex], locus: QXQRange, res: QXAExp = None, parser_context: antlr4.ParserRuleContext = None):
    def __init__(self, ids: [QXBind | QXQIndex], locus: Union[str, list[QXQRange]], res: QXAExp = None, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._ids = ids
        self._locus = locus
        self._res = res

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitMeasure(self)

    def ids(self):
        return self._ids

    def locus(self):
        return self._locus

    def res(self):
        return self._res

    def __repr__(self):
        return f"QXMeasure(ids={self._ids}, locus={self._locus}, res={self._res})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXMeasureAbort(QXStmt):
    '''
    A specific measure statement used in SWAPTest.qfy. Aborts the other qubits.

    example (SWAPTest.qfy):
    ...
    ╭──────────────────────╮
    │ y, prob *= measA(r); │ //we need a special measurement that will abort the other qubits and the main product is to compute the probablity of y.
    ╰──────────────────────╯
    ...

    '''

    def __init__(self, ids: str, locus: list[QXQRange], res: QXAExp = None, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._ids = ids
        self._locus = locus
        self._res = res

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitMeasureAbort(self)

    def ids(self):
        return self._ids

    def locus(self):
        return self._locus

    def res(self):
        return self._res

    def __repr__(self):
        return f'QXMeasureAbort(ids={self._ids}, locus={self._locus}, res={self._res})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXIf(QXStmt):

    def __init__(self, bexp: QXBExp | QXBoolLiteral, stmts: [QXStmt], else_branch: [QXStmt], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._bexp = bexp
        self._stmts = stmts
        self._else_branch = else_branch

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitIf(self)

    def bexp(self):
        return self._bexp

    def stmts(self):
        return self._stmts

    def else_stmts(self):
        return self._else_branch

    def __repr__(self):
        return f"QXIf(bexp={self._bexp}, stmts={self._stmts})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXFor(QXStmt):

    def __init__(self, id: str, crange: QXCRange, conds: [QXCond], stmts: [QXStmt], parser_context: Optional[antlr4.ParserRuleContext] = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._crange = crange
        self._conds = conds
        self._stmts = stmts

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitFor(self)

    def ID(self) -> str:
        return self._id if isinstance(self._id, str) else self._id.getText()

    def crange(self) -> QXCRange:
        return self._crange

    def conds(self) -> [QXCond]:
        '''Returns an array of loop conditions used to prove correctness.'''
        return self._conds

    def inv(self) -> [QXSpec]:
        return list(map(lambda inv: inv.spec(), filter(lambda cond: isinstance(cond, QXInvariant), self._conds)))

    def sep(self) -> [[QXQRange]]:
        '''Returns all the loci used in separates clauses from the conditions array'''
        return list(map(lambda sep: sep.locus(), filter(lambda cond: isinstance(cond, QXSeparates), self._conds)))

    def dec(self) -> [QXAExp]:
        '''Returns all the arith expressions used in decreases clauses from the conditions array'''
        return list(map(lambda sep: sep.aexp(), filter(lambda cond: isinstance(cond, QXDecreases), self._conds)))

    def stmts(self) -> [QXStmt]:
        return self._stmts

    def __repr__(self):
        return f"QXFor(id={repr(str(self._id))}, crange={self._crange}, conds={self._conds}, stmts={self._stmts})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXWhile(QXStmt):

    def __init__(self, bexp: QXBExp, conds: [QXCond], stmts: [QXStmt], parser_context: Optional[antlr4.ParserRuleContext] = None):
        super().__init__(parser_context=parser_context)
        self._bexp = bexp
        self._conds = conds
        self._stmts = stmts

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitWhile(self)

    def bexp():
        return self._bexp

    def conds():
        return self._conds

    def stmts():
        return self._stmts

    def __repr__(self):
        return f'QXWhile(bexp={self._bexp}, conds={self._conds}, stmts={self._stmts})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXCall(QXStmt, QXAExp):

    def __init__(self, id: str, exps: [QXAExp], inverse: bool = False, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._exps = exps
        self._inverse = inverse

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitCall(self)

    def ID(self) -> str:
        return self._id if isinstance(self._id, str) else self._id.getText()

    def exps(self) -> [QXAExp]:
        return self._exps

    def inverse(self) -> bool:
        return self._inverse

    def __repr__(self) -> str:
        return f"QXCall(id={repr(str(self._id))}, exps={self._exps}, inverse={self._inverse})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXReturn(QXStmt):
    '''
    Represents a return statement, with a number of ids indicating which variables to return.

    example:
    ...
        ╭──────────────────╮
        │ return a_l, a_u; │
        ╰──────────────────╯
    }
    '''

    def __init__(self, ids: [str], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._ids = ids

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitReturn(self)

    def ids(self):
        return self._ids

    def __repr__(self):
        return f'QXReturn(ids={self._ids})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXBreak(QXStmt):
    '''
    Represents a break statement.

    example:
    ...
    for i in [0, N)
        ...
    {
        if (y == 1){
            ╭────────╮
            │ break; │
            ╰────────╯
        }
    ...
    '''
    
    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitBreak(self)

    def __repr__(self):
        return 'QXBreak()'

########################################################################
# Partitions:
#   The next section deals with partitions. This is particularly
#   convoluted because of the myriad of ways that qafny can declare
#   different partitions.
#
# Suggestion:
#  Partitions turn into a list of a class specifying:
#   - amplitude
#   - arith expression or function identifier that acts as the predicate
#   - whether that predicate should evaluate to true or false
########################################################################


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPartPredicate(QXTop):
    '''
    Represents a predicate used inside a partition function.

    example:
              ╭─────────────────────────╮
    part(2^n, │ sin theta : f(|k⟩) == 1 │ , cos theta : f(|k⟩) == 0)
              ╰─────────────────────────╯
    '''

    def __init__(self, amplitude: QXAExp, predicate: QXBExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._amplitude = amplitude
        self._predicate = predicate

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPartPredicate(self)

    def amplitude(self):
        return self._amplitude

    def predicate(self):
        return self._predicate

    def __repr__(self) -> str:
        return f'QXPartPredicate(amplitude={self._amplitude}, predicate={self._predicate})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPartsection(QXTop):
    '''
    Represents a part of a predicate used inside a partition function

    example (from SimpleAmpEstimate.qfy):
                                                                               ╭────────────────────────────────────╮
    assert { q[0, j), p[0, n), r[0] : En ↦ ∑ v ∈ [0, 2^j) . 1/sqrt(2^j) part( │ sin (2*v+1) * theta : |v⟩ f(|k⟩, 1) │ + cos (2*v+1) * theta : |v⟩ f(|k⟩, 0)) |-⟩ };
                                                                               ╰────────────────────────────────────╯
    '''

    def __init__(self, amplitude: QXAExp, ket: QXSKet, predicate: QXCall, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._amplitude = amplitude
        self._ket = ket
        # predicate should be of type QXCall
        self._predicate = predicate

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPartsection(self)

    def amplitude(self) -> QXAExp:
        return self._amplitude

    def ket(self) -> QXSKet:
        return self._ket

    def predicate(self) -> QXCall:
        return self._predicate

    def __str__(self) -> str:
        return f'{self._amplitude} : {self._ket} {self._predicate}'

    def __repr__(self) -> str:
        return f'QXPartsection(amplitude={self._amplitude}, ket={self._ket}, predicate={self._predicate})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPart(QXQState):
    '''
    Represents a call to the "partition" function inside of a quantum specification.
    This is the original partition method, however, 4 other ways also exist.

    see also:
    - QXPartWithPredicates
    - QXPartGroup
    - QXPartLambda
    - QXPartWithSections

    example (from Grovers.qfy):
                           ╭─────────────────────────────────────────────────────────────────╮
    assert { q[0,n) : aa ↦│ part(n,f , sin (sumFun(f,2^n) / 2^n), cos(sumFun(f,2^n) / 2^n)) │ };
                           ╰─────────────────────────────────────────────────────────────────╯
    '''

    def __init__(self, num : QXAExp, fname: QXAExp, tamp: QXAExp, famp : QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._num = num
        self._fname = fname
        self._tamp = tamp
        self._famp = famp

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPart(self)

    def qnum(self):
        return self._num

    def fname(self):
        return self._fname

    def trueAmp(self):
        return self._tamp

    def falseAmp(self):
        return self._famp

    def __repr__(self) -> str:
        return f"QXPart(num={self._num}, fname={self._fname}, tamp={self._tamp}, famp={self._famp})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPartWithPredicates(QXQState):
    '''
    Represents a call to the "partition" function inside of a quantum specification.
    The number is optional.

    see also:
    - QXPart
    - QXPartGroup
    - QXPartLambda
    - QXPartWithSections

    example (from FixedPointSearch.qfy):
                            ╭────────────────────────────────────────────────────────────╮
    assert { p[0,n) : en ↦ │ part(2^n, sin theta : f(|k⟩) == 1, cos theta : f(|k⟩) == 0) │ };
                            ╰────────────────────────────────────────────────────────────╯
    
    '''

    def __init__(self, num: QXAExp | None, true_predicate: QXPartPredicate, false_predicate: QXPartPredicate, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._num = num
        self._true_predicate = true_predicate
        self._false_predicate = false_predicate

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPartWithPredicates(self)

    def qnum(self) -> QXAExp:
        return self._num

    def truePred(self) -> QXPartPredicate:
        return self._true_predicate

    def falsePred(self) -> QXPartPredicate:
        return self._false_predicate

    def __repr__(self) -> str:
        return f"QXPartWithPredicates(num={self._num}, true_predicate={self._true_predicate}, false_predicate={self._false_predicate})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPartGroup(QXQState):
    '''
    Represents a call to the "partition" function inside of a quantum specification.

    see also:
    - QXPart
    - QXPartWithPredicates
    - QXPartLambda
    - QXPartWithSections

    example (from test15.qfy):
                               ╭─────────────────────────────────────────────────────────╮
    requires { q[0, n) : aa ↦ │ part(f, true, sin (arcsin(sqrt(sumFun(f, 2^n) / 2^n)))) │ + part(f, false, cos(arcsin(sqrt(sumFun(f, 2^n) / 2^n)))) }
                               ╰─────────────────────────────────────────────────────────╯
    '''
    
    def __init__(self, fpred: str, bool_lit: QXBoolLiteral, amplitude: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._fpred = fpred.getText() if isAntlrNode(fpred) else fpred
        self._bool_lit = bool_lit
        self._amplitude = amplitude

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPartGroup(self)

    def fpred(self):
        '''Returns the function predicate associated with this partition call'''
        return self._fpred if isinstance(self._fpred, str) else self._fpred.getText()

    def bool(self):
        '''Returns the boolean associated with this partition'''
        return self._bool_lit

    def amplitude(self):
        return self._amplitude

    def __repr__(self) -> str:
        return f'QXPartGroup(id={self._fpred}, bool_lit={self._bool_lit}, amplitude={self._amplitude})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPartLambda(QXQState):
    '''
    Represents a call to the "partition" function inside of a quantum specification.

    see also:
    - QXPart
    - QXPartWithPredicates
    - QXPartGroup
    - QXPartWithSections

    example (from test16.qfy):
                                                                         ╭─────────────────────────╮
    requires { p[0, 2), q[0, n) : aa ↦ ∑ k ∈ [0, 2) . 1 / sqrt(2) |k⟩|k⟩ │ part(f, sin(arcsin(r))) │ + ∑ k ∈ [0, 2) . 1/sqrt(2) |k⟩|k⟩ part(f, cos(arcsin(r))) } 
                                                                         ╰─────────────────────────╯
    '''

    def __init__(self, fpred: str, amplitude: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._fpred = fpred.getText() if isAntlrNode(fpred) else fpred
        self._amplitude = amplitude

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPartLambda(self)

    def fpred(self):
        '''Returns the function predicate associated with this partition call'''
        return self._fpred

    def amplitude(self):
        return self._amplitude

    def __repr__(self) -> str:
        return f'QXPartLambda(predicate={self._fpred}, amplitude={self._amplitude})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPartWithSections(QXQState):
    '''
    Represents a call to the "partition" function inside of a quantum specification.

    see also:
    - QXPart
    - QXPartWithPredicates
    - QXPartGroup
    - QXPartLambda

    example (from SimpleAmpEstimate.qfy):
                                                                         ╭───────────────────────────────────────────────────────────────────────────────╮
    assert { q[0, j), p[0, n), r[0] : En ↦ ∑ v ∈ [0, 2^j) . 1/sqrt(2^j) │ part(sin (2*v+1) * theta : |v⟩ f(|k⟩, 1) + cos (2*v+1) * theta : |v⟩ f(|k⟩, 0)) │ |-⟩ };
                                                                         ╰───────────────────────────────────────────────────────────────────────────────╯
    '''

    def __init__(self, sections: [QXPartsection], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._sections = sections

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPartWithSections(self)

    def sections(self) -> [QXPartsection]:
        return self._sections

    def __repr__(self) -> str:
        return f'QXPartWithSections(sections={self._sections})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXQSpec(QXSpec):

    def __init__(self, locus: [QXQRange], qty: QXType, states: [QXQState], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._locus = locus
        self._qty = qty
        self._states = states

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitQSpec(self)

    def locus(self):
        return self._locus

    def qty(self):
        return self._qty

    def states(self):
        return self._states

    def __repr__(self):
        return f"QXQSpec(locus={self._locus}, qty={self._qty}, states={self._states})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXRequires(QXCond):

    def __init__(self, spec: QXSpec, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._spec = spec

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitRequires(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXRequires(spec={self._spec})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXEnsures(QXCond):

    def __init__(self, spec: QXSpec, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._spec = spec

    def accept(self, visitor : AbstractProgramVisitor):
        return visitor.visitEnsures(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXEnsures(spec={self._spec})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXInvariant(QXCond):

    def __init__(self, spec: QXSpec, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._spec = spec

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitInvariant(self)

    def spec(self):
        return self._spec

    def __repr__(self):
        return f"QXInvariant(spec={self._spec})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXDecreases(QXCond):
    '''
    Represents a decreases loop invariant.
    example:
    ...
    while ci_ub - ci_lb > 2 * eps
        ╭─────────────────────────╮
        │ decreases ci_ub - ci_lb │
        ╰─────────────────────────╯
        invariant i < |K| && i < |k|
    ...
    '''

    def __init__(self, arith_expr: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._arith_expr = arith_expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitDecreases(self)

    def aexp(self):
        return self._arith_expr

    def __repr__(self):
        return f"QXDecreases(arith_expr={self._arith_expr})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXSeparates(QXCond):
    '''
    Represents a separates loop invariant.
    example:
    ...
    for i in [0, n) with q[i]
        ╭────────────────────────────╮
        │ separates q[0, i), p[0, n) │
        ╰────────────────────────────╯
        invariant {
    ...
    '''

    def __init__(self, locus: [QXQRange], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._locus = locus

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitEnsures(self)

    def locus(self):
        return self._locus

    def __repr__(self):
        return f"QXSeparates(locus={self._locus})"


########################################################################
# Top-level Qafny nodes
#   All of these nodes can be declared directly at the beginning of the
#   program.
########################################################################


@qafny.auto.rich_repr
@qafny.auto.equality
class QXInclude(QXTop):
    '''
    Represents an include top-level statment. The file to be included is provided as path.
    example (from Superdense.qfy):
    //Superdense Coding
    ╭──────────────────────╮
    │ include BellPair.qfy │
    ╰──────────────────────╯
    ...
    '''

    def __init__(self, path: str, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._path = path.getText() if isAntlrNode(path) else path

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitInclude(self)

    def path(self) -> str:
        '''Path to the file to be included'''
        return self._path

    def __repr__(self):
        return f'QXInclude(path={self._path})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXMethod(QXTop):

    def __init__(self, id: str, axiom: bool, bindings: [QXBind], returns: [QXBind], conds: [QXCond], stmts: [QXStmt], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._axiom = axiom
        self._bindings = bindings
        self._returns = returns
        self._conds = conds
        self._stmts = stmts

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitMethod(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def axiom(self):
        return self._axiom

    def bindings(self):
        return self._bindings

    def returns(self):
        return self._returns

    def conds(self):
        return self._conds

    def stmts(self):
        return self._stmts

    def __repr__(self):
        return f"QXMethod(id={repr(str(self._id))}, axiom={self._axiom}, bindings={self._bindings}, returns={self._returns}, conds={self._conds}, stmts={self._stmts})"


@qafny.auto.rich_repr
@qafny.auto.equality
class QXFunction(QXTop):

    def __init__(self, id: str, axiom: bool, bindings: [QXBind], return_type: QXQTy, arith_expr: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._axiom = axiom
        self._bindings = bindings
        self._return_type = return_type
        self._arith_expr = arith_expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitFunction(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def axiom(self):
        return self._axiom

    def bindings(self):
        return self._bindings

    def return_type(self):
        return self._return_type

    def arith_expr(self):
        return self._arith_expr

    def __repr__(self):
        return f'QXFunction(id={self._id}, axiom={self._axiom}, bindings={self._bindings}, return_type={self._return_type}, arith_expr={self._arith_expr})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXLemma(QXTop):

    def __init__(self, id: str, axiom: bool, bindings: [QXBind], conds: [QXCond], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._axiom = axiom
        self._bindings = bindings
        self._conds = conds

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitLemma(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def axiom(self):
        return self._axiom

    def bindings(self):
        return self._bindings

    def conds(self):
        return self._conds

    def __repr__(self):
        return f'QXLemma(id={self._id}, axiom={self._axiom}, bindings={self._bindings}, conds={self._conds})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXPredicate(QXTop):

    def __init__(self, id: str, bindings: [QXBind], arith_expr: QXAExp, parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._id = id.getText() if isAntlrNode(id) else id
        self._bindings = bindings
        self._arith_expr = arith_expr

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitPredicate(self)

    def ID(self):
        return self._id if isinstance(self._id, str) else self._id.getText()

    def bindings(self):
        return self._bindings

    def arith_expr(self):
        return self._arith_expr

    def __repr__(self):
        return f'QXPredicate(id={self._id}, bindings={self._bindings}, arith_expr={self._arith_expr})'


@qafny.auto.rich_repr
@qafny.auto.equality
class QXProgram(QXTop):

    def __init__(self, exps: [QXTop], parser_context: antlr4.ParserRuleContext = None):
        super().__init__(parser_context=parser_context)
        self._exps = exps

    def accept(self, visitor: AbstractProgramVisitor):
        return visitor.visitProgram(self)

    def topLevelStmts(self):
        return self._exps

    def __repr__(self):
        return f"QXProgram(exps={self._exps})"