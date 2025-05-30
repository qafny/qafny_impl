
## The `Relations` module {#sec-relations}

The `Relations` module defines a number of properties of functions.

Properties of binary predicates:
- Reflexive: `R(x,x)` is true
- Irreflexive: `R(x,x)` is false
- AntiSymmetric: `R(x,y) && R(y,x) ==> x==y`
- Transitive : `R(x,y) && R(y,z) ==> R(x,z)`
- Connected : `x != y ==> R(x,y) || R(y,x)`
- StronglyConnected : `R(x,y) || R(y,x)`
- TotalOrdering : Reflexive, AntiSymmetric, Transitive, StronglyConnected (e.g., `<=` on integers)
- StrictTotalOrdering : Irreflexive, AntiSymmetric, Transitive, Connected (e.g., `<` on integers)

A property of unary functions:
- Injective : `f(x) == f(y) ==> x == y`

These properties are sometimes required for other functions. For example, 
if one wants to sort a sequence by some relation `R`, one must establish that `R` is a _Total Ordering_
or a _Strict Total Ordering_.
In fact, that is part of the precondition of a sorting function.

As a simple example, you might define a predicate like this:
<!-- %check-resolve -->
```dafny
  const IntLT := ((i: int, j: int) => (i < j))
```

and then need to proof this lemma to use it in a sorting routine:
<!-- %check-verify -->
```dafny
  lemma IntLTisStrictTotalOrder()
    ensures StrictTotalOrdering(IntLT) {}
```

Fortunately, dafny proves this without aid.

All these definitions are ghost predicates; they are used as part of proofs rather than in compiled code.
