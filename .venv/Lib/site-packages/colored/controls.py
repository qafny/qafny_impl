#!/usr/bin/env python
# -*- coding: utf-8 -*-

from typing import Optional

from .exceptions import InvalidControl
from .library import Library
from .utilities import Utilities

# pylint: disable=invalid-name


class Controls:  # pylint: disable=[R0903]

    """ Controls class managing the cursor moves.
    """

    def __init__(self) -> None:
        self._utils = Utilities()
        self._ESC: str = Library.ESC
        self._CONTROLS: dict[str, str] = Library.CONTROLS

    def nav(self, name: str, row: int, column: Optional[int] = None) -> str:
        """Args:
            name (str): Type of cursor move. (See library.py for available moves).
            row (int): Number of rows to move.
            column (Optional[int]): Number of columns to move. Required for 'position' move.

        Returns:
            str: An ANSI escape sequence string that performs the specified cursor movement.

        Examples:
            ESC[ row B: Cursor Down
            ESC[ row C: Cursor Forward
            ESC[ row D: Cursor Back
            ESC[ row E: Cursor Next Line
            ESC[ row F: Cursor Previous Line
            ESC[ row G: Cursor Horizontal Absolute
            ESC[ row; col; H: Cursor Position
            ESC[ row J: Erase in Display
            ESC[ row K: Erase in Line
            ESC[ row S: Scroll Up
            ESC[ row T: Scroll Down
        Raises:
             InvalidControl: Raises an Exception.
        """
        if not isinstance(name, str):
            raise TypeError(f"The 'name' argument must be a string, got {type(name).__name__}")
        self._utils.is_control_exist(name)

        if column:
            if name.lower() == 'position':
                return f'{self._ESC}{row};{column}{self._CONTROLS[name.lower()]}'

            raise InvalidControl(f"InvalidControl: {name}: Expected 'position'")

        return f'{self._ESC}{row}{self._CONTROLS[name.lower()]}'
