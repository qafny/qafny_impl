from typing import (
    Any,
    Callable,
    Dict,
    Tuple,
    Type,
    TypeVar
)

import inspect

import builtins
import collections
from array import array
import os
from collections import Counter, UserDict, UserList, defaultdict, deque
from types import MappingProxyType



BRACES: Dict[type, Callable[[Any], Tuple[str, str, str]]] = {
    os._Environ: lambda _object: ("environ({", "})", "environ({})"),
    array: lambda _object: (f"array({_object.typecode!r}, [", "])", f"array({_object.typecode!r})"),
    defaultdict: lambda _object: (f"defaultdict({_object.default_factory!r}, {{", "})", f"defaultdict({_object.default_factory!r}, {{}})"),
    Counter: lambda _object: ("Counter({", "})", "Counter()"),
    deque: lambda _object: ("deque([", "])", "deque()") if _object.maxlen is None else ("deque([", f"], maxlen={_object.maxlen})", f"deque(maxlen={_object.maxlen})"),
    dict: lambda _object: ("{", "}", "{}"),
    UserDict: lambda _object: ("{", "}", "{}"),
    frozenset: lambda _object: ("frozenset({", "})", "frozenset()"),
    list: lambda _object: ("[", "]", "[]"),
    UserList: lambda _object: ("[", "]", "[]"),
    set: lambda _object: ("{", "}", "set()"),
    tuple: lambda _object: ("(", ")", "()"),
    MappingProxyType: lambda _object: ("mappingproxy({", "})", "mappingproxy({})"),
}
CONTAINERS = tuple(BRACES.keys())
MAPPING_CONTAINERS = (dict, os._Environ, MappingProxyType, UserDict)

def make_repr(class_name: str, props: {str: any}) -> str:
    '''Returns a string representation of an object closely matching python construction syntax.
    None types are hidden by default'''
    property_list = ", ".join(
        f'{k}={v}' for k, v in props.items() if v is not None)
    return f"{class_name}({property_list})"


def is_sequence(potential_seq) -> bool:
    '''Returns true if the object is a list, i.e. is iterable, but not a string'''
    return (not hasattr(potential_seq, "strip") and
            hasattr(potential_seq, "__iteritems__") or
            hasattr(potential_seq, "__iter__"))


def listify(potential_list):
    '''Wraps scalar objects in a list; passes through lists without alteration'''
    if is_sequence(potential_list) and not isinstance(potential_list, dict):
        return potential_list

    return [potential_list]


def hasmembervariable(cls: Type[TypeVar("T")], name: str):
    '''Helper function to determine whether a member variable exists on a class.'''
    return hasattr(cls, name) and not inspect.ismethod(getattr(cls, name))
