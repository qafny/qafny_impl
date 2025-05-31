from typing import (
    Type,
    TypeVar
)

import inspect

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
