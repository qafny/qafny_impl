include "../libraries/src/NonlinearArithmetic/Power2.dfy"
include "../libraries/src/NonlinearArithmetic/Power.dfy"
include "../libraries/src/NonlinearArithmetic/DivMod.dfy"
include "../libraries/src/NonlinearArithmetic/Mul.dfy"

// import opened Power
// import opened DivMod
// import opened Mul

predicate boundedSame (x : seq<bv1>, y : seq<bv1> , n:nat) 
  requires n <= |x|
  requires n <= |y|
{
  forall k :: 0 <= k < n ==> x[k] == y[k]
}

function method {:axiom} omega(n:nat, a:nat): real

lemma {:axiom} omegaplus(a:nat, b:nat, c:nat)
  ensures omega(b+c,a) == omega(b) * omega(c,a)
  
method hadH(x:bv1) returns (y : int) 
  ensures y == x as int
{
  y := x as int;
}

//applying H to every x, where x is Nor type qubits. Each item in y is an int representation of omega(x, 2), maybe we can just do omega(x,2)
//please review the trigger in Dafny, so that the lemma omegaplus can automatically apply.
method hadHall(x:seq<bv1>) returns (y : seq<real>) 
  ensures y.Length == |x|
  ensures forall k :: 0 <= k < |x| ==> y[k] == omega(x[k] as int,2)
{
  var i := 0;
  y := [];
  while i < |x| 
    invariant 0 <= i <= |x|
    invariant forall k :: 0 <= k < i ==> y[k] == x[k] as int
  {
    y := y + [omega(x[i] as int, 2)];
    i := i + 1;
  }
  return y;
}


//this function will perform casting. The {1.0} is the place for performing template. If we are casting n Had from Had to EN,
//we can make 1.0 to be omega(a,2), where a has a pattern, such as a = 0 for all case. 
method nHEnPre(n:nat) returns (y : seq<(real,real,seq<bv1>)>)
  ensures |y| == pow2(n)
  ensures 0 < |y|;
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == sqrt(pow2(n) as real)
  ensures forall k :: 0 <= k < |y| ==> y[k].1 == {1.0}
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == n
  ensures allzero(y[0].2)
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == n
{
   y := [(sqrt(pow2(n) as real),1.0,[])];
   var i := 0;
   while (i < n)
     invariant i <= n
     invariant |y| == pow2(i)
     invariant 0 < |y|;
     invariant forall k :: 0 <= k < |y| ==> y[k].0 == sqrt(pow2(n) as real)
     invariant forall k :: 0 <= k < |y| ==> y[k].1 == {1.0}
     invariant forall k :: 0 <= k < |y| ==> |y[k].2| == i
     invariant allzero(y[0].2)
   {
    y := ncat(y,0,i) + ncat(y,1,i);
    i := i + 1;
   }

}


//below is an example of applying lambda term.
//a[0,n),b[0,m) *= lambda(x,y => omega(x,y) * |f(x,y)>|g(x,y)>)
//For this one, the compiler should be smart enough to split out the lambda variable term, so that x <-> a[0,n) and y <-> b[0,m),
//then, we also notice that the |f(x,y)> will take over the place of x, and |g(x,y)> will take over the place for y.
// omega(x,y) will be the place for phases.
//This lambda term expression is suitable for applying oracle functions on en type state.
//when we write a[0,n),b[0,m), we assume that the ranges a[0,n) and b[0,m) are together, and the whole locus for them
//look like locus = a[0,n),b[0,m),c[q,t),...,
//the lambda application is to apply on the first two ranges of the locus.
//Remember how locus state is represented in Dafny:
//they look like locus -> (phase, a_seq, b_seq, c_seq, ...), where phase is a list of M of phases, and a_seq, b_seq,c_seq are lists of M basis-vectors.
//where a_seq corresponds to a[0,n), b_seq corresponses to range b[0,m), etc
//In this case, |a_seq| = n for each element, and |b_seq| = m for each element.
//So essentially, in Dafny, the basis-vectors are separated into different ranges, but we just manage it as locus
//in the qafny level
function method {:axiom} multmod(x: seq<bv1>, a: nat, n: nat) :seq<bv1>
  requires 1 < a < n
  ensures |multmod(x,a,n)| == |x| 
  ensures b2n(multmod(x,a,n), |x|) == a * b2n(x, |x|) % n


  lemma {:axiom} b2nEqSeq(a:nat, N:nat, i:nat, x : array<(seq<bv1>,seq<bv1>)>, n:nat, ni:nat)
    requires i < x.Length 
    requires 1 < a < N
    requires |x[i].1| == n
    requires |x[i].0| == ni + 1
    ensures  b2n(x[i].1, n) == Pow(a, b2n(x[i].0, ni + 1)) % N



//the following is a template function for quantum conditional where the e part contains no Hadamard operations.
//if b(z[i]) { e } 
method ctrU({z0:seq<seq<bv1>> ,z1: seq<<bv1>>}  //this part needs to be in template. You will have at least two sequences, depending on the length of the locus.
         ,ni: nat , n:nat, a : nat,  N : nat) returns (... )//return the same number of sequences as the input
  requires 1 < a < N
  requires 0 < n
  requires forall k :: 0 <= k < z.Length ==> |z[k].0| == ni + 1
  requires forall k :: 0 <= k < z.Length ==> |z[k].1| == n
  requires forall k :: 0 <= k < z.Length ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni)) % N
  ensures forall k :: 0 <= k < z.Length ==> |z[k].0| == ni + 1
  ensures forall k :: 0 <= k < z.Length ==> |z[k].1| == n
  ensures forall k :: 0 <= k < z.Length ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni + 1)) % N
{
  var i := 0;
  while i < z.Length
    modifies z
    invariant i <= z.Length
    invariant forall k :: 0 <= k < z.Length ==> |z[k].0| == ni + 1
    invariant forall k :: 0 <= k < z.Length ==> |z[k].1| == n
    invariant forall k :: i <= k < z.Length ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni)) % N
    invariant forall k :: 0 <= k < i ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni + 1)) % N
  {
    if z[i].0[ni] == 1 {
      z[i] := (z[i].0,multmod(z[i].1, a, N)); //depending on the call, we will have some z0[i] part being applied, and some z1[i] part has no effect
                // the constraint would look like, forall k :: 0 <= k < |z| ==> z[k][i] == 1 (* i is the z[i] being controlled *) ==> P'
                //                                 forall k :: 0 <= k < |z| ==> z[k][i] == 0 (* i is the z[i] being controlled *) ==> P
                //P' and P are the effects of applying/not-applying the e part of the quantum conditional
    }
    else {
    }
    assert |z[i].0| == ni + 1;
    assert |z[i].1| == n;
    {assert (b2n(z[i].1, n) == Pow(a, b2n(z[i].0, ni + 1)) % N) by {b2nEqSeq(a,N,i,z,n,ni); }} //this part belongs to tempelate
    i := i+1;
  }
}

// For a quantum conditional with H in the middle, please refers to qwalk.dfy
