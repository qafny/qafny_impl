// RUN: %verify "%s"

/*******************************************************************************
 *  Copyright by the contributors to the Dafny Project
 *  SPDX-License-Identifier: MIT 
 *******************************************************************************/

module {:options "-functionSyntax:4"} Dafny.Math {
  function Min(a: int, b: int): int
  {
    if a < b
    then a
    else b
  }

  function Max(a: int, b: int): int
  {
    if a < b
    then b
    else a
  }

  function Abs(a: int): int
  {
    if a < 0
    then -a
    else a
  }
}
