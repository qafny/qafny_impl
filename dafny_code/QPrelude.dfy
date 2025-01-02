include "./external/libraries/src/NonlinearArithmetic/Power2.dfy"
include "./external/libraries/src/NonlinearArithmetic/Power.dfy"
include "./external/libraries/src/NonlinearArithmetic/DivMod.dfy"
include "./external/libraries/src/NonlinearArithmetic/Mul.dfy"

// import opened Power
// import opened DivMod
// import opened Mul

// Binary to Natural Number Conversion 
module B2N {
  function b2n(a : seq<nat>) : nat
    requires (forall k | 0 <= k < |a| :: a[k] == 0 || a[k] == 1)
  {
    if |a| == 0 then 0 else a[0] + 2 * b2n(a[1..])
  }

  lemma LemmaB2NAllZeros(a : seq<nat>)
    requires (forall k | 0 <= k < |a| :: a[k] == 0)
    decreases a
    ensures b2n(a) == 0
  {
  }

  lemma LemmaB2NTailingZeros(a : seq<nat>, i : nat)
    requires 0 <= i < |a|
    requires (forall k | 0 <= k < i :: a[k] == 0 || a[k] == 1)
    requires (forall k | i <= k < |a| :: a[k] == 0)
    ensures b2n(a) == b2n(a[0..i])
  {
    if (i == 0) {
      LemmaB2NAllZeros(a[i..]);
      assert b2n(a[i..]) == 0;
    } else {
      LemmaB2NTailingZeros(a[1..], i - 1);
      assert b2n(a[1..]) == b2n(a[1..][0..i-1]);
      assert (a[1..][0..i-1] == a[1..i]);
      assert b2n(a[1..]) == b2n(a[1..i]);
      assert a[0] + 2 * b2n(a[1..]) == a[0] + 2 * b2n(a[1..i]);
      assert b2n(a) == b2n(a[0..i]);
    }
  }
  
}

module QPrelude {
  import opened Power2
  import opened B2N
  datatype Mode =
    | Nor(b : seq<nat>)
    | Had(h : seq<int>)
    // c : a session
    // a session is a sequence of q registers
    // a q register in CH mode is a seq tuple representing the summation
    // fst pos in tuple is the ket, snd pos is the coef
    | CH(dof : nat, c : map<Qubits, seq<(nat, int)>>)
    
  class {:autocontracts} Qubits {
    var m : Mode;
    var card : nat; 
    
    predicate Valid()
      reads this
    {
      // card > 0 &&
      match m {
        case Nor(b) => |b| == card && forall i | 0 <= i < |b| :: b[i] == 0 || b[i] == 1
        case Had(h) => |h| == card
        case CH(dof, c) =>
          dof <= Pow2(card) && this in c &&
          forall q | q in c :: |c[q]| == dof
      }
    }
  
    // Constructor
    // constructor EmptyCH()
    //   ensures card == 0
    //   ensures m.CH?
    //   ensures m.dof == 1;
    //   ensures m.c[0] == (0, 1);
    // {
    //   var empty := seq(1, _ => (0, 1));
    //   card := 0;
    //   m := CH(1, empty);
    // }
  
    constructor PrepareAny(n : nat, s : seq<nat>)
      requires |s| == n
      requires forall k | 0 <= k < n :: s[k] == 0 || s[k] == 1
      // ensures Wf()
      ensures m.Nor?
      ensures card == n
      ensures forall k | 0 <= k < n :: m.b[k] == s[k]
    {
      card := n;
      m := Nor(s);
    }

    // Mode Gates
    method H()
      requires m.Nor?
      ensures m.Had?
      ensures |m.h| == old(|m.b|) == card
      ensures forall i | 0 <= i < card :: old(m.b[i]) == 0 ==> m.h[i] == 1
      modifies this
  
    method X()
      requires m.Nor?
      ensures card == old(card)
      ensures m.Nor?
      ensures forall k | 0 <= k < |m.b| :: old(m.b[k] == 1) ==> m.b[k] == 0
      ensures forall k | 0 <= k < |m.b| :: old(m.b[k] == 0) ==> m.b[k] == 1
      modifies this
    // {
    //   m := Nor(seq(card, i reads this requires forall e | e in m.b :: e == 0 || e == 1 => 1 - m.b[i]));
    // }
  
    method XSlice(l : nat, r : nat)
      requires m.Nor?
      requires 0 <= l < r <= card
      ensures card == old(card)
      ensures m.Nor?
      ensures forall k | 0 <= k < l :: m.b[k] == old(m.b[k])
      ensures forall k | l <= k < r :: old(m.b[k] == 0) ==> m.b[k] == 1
      ensures forall k | l <= k < r :: old(m.b[k] == 1) ==> m.b[k] == 0
      ensures forall k | r <= k < card :: m.b[k] == old(m.b[k])
      modifies this
      
    // TODO : I may need to customize the trigger to make the automation smarter
    method HSplit(n : nat) returns (q : Qubits)
      requires m.Had? && 1 <= n <= card
      ensures m.Had?
      ensures fresh(q)
      ensures q.Valid() && q.m.Had? && q.card == n && q.card + card == old(card)
      ensures forall i | 0 <= i < n :: q.m.h[i] == old(m.h[i])
      ensures forall i | 0 <= i < card :: m.h[i] == old(m.h[n + i])
      modifies this
  

    // TODO: How to deal with the coefficient?
    method Classical(s : seq<nat>)
      requires m.CH? && this in m.c && m.dof == |s|
      ensures m.CH? && this in m.c && m.dof == |s| && card == old(card)
      ensures forall i | 0 <= i < m.dof :: s[i] == m.c[this][i].0
      ensures forall i | 0 <= i < m.dof :: old(m.c[this][i].1) == m.c[this][i].1
      ensures m.c.Keys == old(m.c.Keys)
      modifies this
  
    // Is there anyway to avoid stating [m], [card] are unchanged? 
    // method Replicate() returns (s : Qubits)
    //   requires m.CH?
    //   requires this in m.c
    //   ensures fresh(s) && s.Valid() && s != this
    //   ensures unchanged(this)
    //   ensures s.m.CH?
    //   ensures s.m.c == (this.m.c - { this })[s := this.m.c[this]]
    //   ensures s.card == this.card && s.m.dof == this.m.dof
    // {
    //   s := new Qubits.PrepareAny(0, []);
    //   s.card := card;
    //   s.m := CH(m.dof, (this.m.c - { this })[s := this.m.c[this]]);
    // }

    method Replicate(s : Qubits) // returns (s : Qubits)
      requires Valid() && m.CH?
      requires this in m.c && this != s
      // ensures fresh(s) && s.Valid() && s != this
      ensures unchanged(this)
      ensures s.m.CH?
      ensures s.Valid()
      ensures s.m.c == (this.m.c - { this })[s := this.m.c[this]]
      ensures s.card == this.card && s.m.dof == this.m.dof
      modifies s, s.Repr
    {
      s.card := card;
      var ss := this.m.c - { this };
      ss := ss[s := this.m.c[this]];
      s.m := CH(m.dof, ss);
      s.Repr := { s };
    }

    method MergeWith(q : Qubits, x : Qubits)
      requires q.Valid() && x.Valid() && q != x && x != this && q != this
      requires x.m.Had? && m.CH? && q.m.CH? && card == q.card && m.dof == q.m.dof && x.card == 1
      requires m.c.Keys - { this } == q.m.c.Keys - { q }
      requires this in m.c.Keys && q in q.m.c.Keys
      ensures unchanged(q) && q.m.c == old(q.m.c)
      ensures x.Valid() && Valid() && q.Valid()
      ensures m.CH? && x.m.CH? && x.m.dof == m.dof == 2 * old(m.dof)
      // cardinality check
      ensures card == old(card) && x.card == old(x.card)
      // include [x] into entanglement set
      ensures m.c.Keys == old(m.c.Keys) + { x } == x.m.c.Keys
      // merge [this] with [q]
      ensures m.c[this] == old(m.c[this]) + old(q.m.c[q])
      // merge the rest
      ensures forall p | p in old(m.c.Keys) - { this } ::  m.c[p] == old(m.c[p]) + old(q.m.c[p])
      // construct [x]
      ensures forall i | 0 <= i < old(m.dof) :: x.m.c[this][i] == (0, 1)
      ensures forall i | old(m.dof) <= i < m.dof :: x.m.c[this][i] == (1, old(x.m.h[0]))
      modifies this, x


    method Merge(q : Qubits)
      requires q.Valid() && q != this
      requires m.CH? && q.m.CH? && card == q.card && m.dof == q.m.dof
      ensures unchanged(q)
      ensures m.CH? && m.dof == 2 * old(m.dof) && card == old(card)
      ensures Valid()
      ensures m.c.Keys == old(m.c.Keys)
      ensures m.c[this] == old(m.c[this]) + old(q.m.c[q])
      modifies this


    // Cast Operator
    method NorToCH()
      requires m.Nor?
      ensures card == old(card)
      ensures m.CH? && m.dof == 1 && card == old(card) && Valid()
      ensures |m.c| == 1 && m.c.Keys == { this }
      ensures m.c[this][0] == (b2n(old(m.b)), 1)
      modifies this
  
    // method SplitPlus(n : nat) returns (q : Qubits)
    //   requires m.Had? 
    //   requires 0 < n <= card
    //   ensures q.m.Had? && q.card == n
    //   ensures m.Had?
    //   ensures card == (old(card) - n) && fresh(q)
    //   ensures q.Valid()
    //   ensures forall k | 0 <= k < n :: q.m.h[k] == old(m.h[k])
    //   ensures forall k | 0 <= k < |m.h| :: m.h[k] == old(m.h[k + n])
    //   modifies this
  
    // Apply H to switch Qubits to Hadamard Basis
    // method PlusRetCH()
    //   requires m.Had?
    //   requires forall i | 0 <= i < card :: m.h[i] == 1
    //   ensures m.CH?
    //   ensures m.dof == Pow2(card)
    //   ensures forall i | 0 <= i < m.dof :: m.c[i] == (i, 1) // amounts to say it's a sum
    //   modifies this
    // {
    //   var dof := Pow2(card);
    //   var c := seq(dof, i => (i, 1));
    //   assert (forall i | 0 <= i < dof :: c[i] == (i, 1));
    //   m := CH(dof, c);
    //   assert (forall i | 0 <= i < m.dof :: m.c[i] == (i, 1));
    // }
  
  
    // {
    //   var j := b2n(m.b);
    //   var t := seq(1, _ => (j, 1));
    //   m := CH(1, t);
    // }
  
    // method CatPlusCH(q : Qubits)
    //   requires m.CH? && q.m.Had?
    //   requires forall i | 0 <= i < m.dof :: m.c[i] == (i, 1)
    //   requires q.card == 1
    //   requires q.Valid()
    //   requires q.m.h[0] == 1
    //   requires m.dof == Pow2(card)
    //   modifies this
    //   ensures m.CH? && m.dof == 2 * old(m.dof)
    //   ensures card == old(card) + 1
    //   ensures forall i | 0 <= i < m.dof :: m.c[i] == (i, 1)
    // {
    //   reveal Pow2();
    //   var offset : nat := Pow2(card);
    //   var newH : seq<(nat, int)> :=
    //     seq(offset, i => m.c[i]) +
    //     seq(offset, i => (offset + m.c[i].0, 1));
    //   m := CH(2 * offset, newH);
    //   card := card + 1;
    // }
    
    // predicate PSumMod(l : nat, r : nat, a : nat, N : nat)
    //   reads this, Repr
    //   requires N > 0
    //   requires m.CH?
    //   requires 0 <= l < r <= m.dof
    // {
    //   forall k | l <= k < r :: (k, Pow(a, k) % N) == m.c[k]
    // }
  
    // predicate Saturated()
    //   requires m.CH?
    //   reads this, Repr
    // {
    //   m.dof == Pow2(card) && forall i | 0 <= i < |m.c| :: m.c[i] == (i, 1)
    // }
  }
}
