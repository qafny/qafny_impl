#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import annotations

from .library import Library


class Hex:

    def find(self, color: str | int) -> str:
        """ Contribution by Fredrik Klasson """

        # Extend shorthand #ABC -> #AABBCC, like in CSS
        if len(color) == 4:
            color: str = '#' + color[1] * 2 + color[2] * 2 + color[3] * 2

        # Try an exact lookup, trying to favor lower numbers
        # (e.g. find 10 instead of 46 for #00FF00)
        for code, hex_color in Library.HEX_COLORS.items():
            if hex_color == color:
                return code

        # Try to find nearest match using a simple least squares fit.
        # We could try to factor in human perception bias by weighting
        # as suggested by <https://stackoverflow.com/a/1847112> but for
        # now lets just KISS and make upp our minds later, no?
        # (we do skip the sqrt since we just care for the relative value)

        # The reference color
        r, g, b = (int(color[1:3], 16), int(color[3:5], 16), int(color[5:7], 16))

        min_cube_d: int = self.cube(0xFFFFFF)
        nearest: str = '15'

        for code, hex_color in Library.HEX_COLORS.items():
            cube_d: int = self.fit(hex_color[1:3], r) + self.fit(hex_color[3:5], g) + self.fit(hex_color[5:7], b)
            if cube_d < min_cube_d:
                min_cube_d = cube_d
                nearest = code

        return nearest

    @staticmethod
    def cube(x: int) -> int:
        """Calculates the square of a given integer.

        Note: The method is named 'cube' but currently calculates the square (x*x).
        If the intention is to calculate the cube, the implementation should be x * x * x.

        Args:
            x (int): The integer number.

        Returns:
            int: The square of the input number.
        """
        # Do not assign a lambda expression, use a def (E731)
        # cube = lambda x: x * x
        return x * x

    def fit(self, hex_val: str, ref: int) -> int:
        """Calculates a component of the color difference for nearest match.

        This method converts a hexadecimal color component string to an integer,
        calculates the difference with a reference integer, and then applies
        the 'cube' method to this difference. It's typically used as part of
        a least squares fit to find the closest color match.

        Args:
            hex_val (str): A string representing a two-digit hexadecimal color component (e.g., 'FF', '00').
            ref (int): An integer representing the reference color component (0-255).

        Returns:
            int: The result of applying the 'cube' method to the difference
                 between the integer value of `hex_val` and `ref`.
        """
        # Do not assign a lambda expression, use a def (E731)
        # f = lambda hex_val, ref: cube(int(hex_val, 16) - ref)
        return self.cube(int(hex_val, 16) - ref)
