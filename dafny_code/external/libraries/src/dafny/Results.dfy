// RUN: %verify "%s"

/*******************************************************************************
 *  Copyright by the contributors to the Dafny Project
 *  SPDX-License-Identifier: MIT 
 *******************************************************************************/

module {:options "--function-syntax:4"} Dafny.Results {

  datatype Option<+T> = None | Some(value: T) {
    predicate IsFailure() {
      None?
    }

    function PropagateFailure<U>(): Option<U>
      requires None?
    {
      None
    }

    function Extract(): T
      requires Some?
    {
      value
    }

    function UnwrapOr(default: T): T {
      match this
      case Some(v) => v
      case None() => default
    }

    function ToResult<E>(error: E): Result<T, E> {
      match this
      case Some(v) => Success(v)
      case None() => Failure(error)
    }

    function ToOutcome<E>(error: E): Outcome<E> {
      match this
      case Some(v) => Pass
      case None() => Fail(error)
    }

    function Map<FC>(rewrap: Option<T> -> FC): FC
    {
      rewrap(this)
    }
  }

  datatype Result<+R, +E> = | Success(value: R) | Failure(error: E) {

    predicate IsFailure() {
      Failure?
    }

    function PropagateFailure<U>(): (r: Result<U, E>)
      requires Failure?
    {
      Failure(this.error)
    }

    function Extract(): R
      requires Success?
    {
      value
    }

    function GetOr(default: R): R
    {
      match this
      case Success(s) => s
      case Failure(e) => default
    }

    function ToOption(): Option<R>
    {
      match this
      case Success(s) => Some(s)
      case Failure(e) => None()
    }

    function ToOutcome(): Outcome<E>
    {
      match this
      case Success(s) => Pass
      case Failure(e) => Fail(e)
    }

    function Map<FC>(rewrap: Result<R,E> -> FC): FC
    {
      rewrap(this)
    }

    function MapFailure<NewE>(reWrap: E -> NewE): Result<R, NewE>
    {
      match this
      case Success(s) => Success(s)
      case Failure(e) => Failure(reWrap(e))
    }
  }

  datatype Outcome<+E> = Pass | Fail(error: E) {
    predicate IsFailure() {
      Fail?
    }

    function PropagateFailure(): Outcome<E>
      requires Fail?
    {
      Fail(this.error)
    }
    // Note: no Extract method, intentionally

    function ToOption<R>(r: R): Option<R>
    {
      match this
      case Pass => Some(r)
      case Fail(e) => None()
    }

    function ToResult<R>(r: R): Result<R,E>
    {
      match this
      case Pass => Success(r)
      case Fail(e) => Failure(e)
    }

    function Map<FC>(rewrap: Outcome<E> -> FC): FC
    {
      rewrap(this)
    }

    function MapFailure<T,NewE>(rewrap: E-> NewE, default: T): Result<T, NewE>
    {
      match this
      case Pass => Success(default)
      case Fail(e) => Failure(rewrap(e))
    }
  }

  // A helper function to ensure a requirement is true at runtime
  // :- Need(5 == |mySet|, "The set MUST have 5 elements.")
  function Need<E>(condition: bool, error: E): (result: Outcome<E>)
  {
    if condition then Pass else Fail(error)
  }
}
