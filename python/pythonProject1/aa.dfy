//In this implementation, assume N = 2^n. assume M << N.
//include "test.dfy"
function method{:axiom} f(x:seq<bv1>) : bv1
function method{:axiom} sqrt(a:real): real
//omega(a,b) = exp(i 2pi * a/b)
function method{:axiom} omega(n:nat, a:nat): real

method hadNorHad(x:seq<bv1>) returns (y : seq<real>) 
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |x| ==> y[k] == omega(x[k] as int,2)
{//for validating them in Dafny
  var i := 0;
  y := [];
  while i < |x| 
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> y[k] == omega(x[k] as int,2)
  {
    y := y + [omega(x[i] as int, 2)];
    i := i + 1;
  }
}

function method pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0

lemma {:axiom} sqSqrt(a:real)
  requires a > 0.0
  ensures sqrt(a) * sqrt(a) == a

lemma {:axiom} omegaplus(a:nat, b:nat, c:nat)
  ensures omega(b+c,a) == omega(b,a) * omega(c,a)
  
method hadH(x:bv1) returns (y : int) 
  ensures y == x as int
{
  y := x as int;
}

function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}

function b2n (x:seq<bv1>, i:nat) : nat
  requires i <= |x|
{
  if (i==0) then 0 else (x[i-1] as nat) * pow2(i-1) + b2n(x[..i-1],i-1)
}



method {:axiom} hadEn(x: seq<real>)
            returns (y : seq<seq<bv1>>, ampy: seq<real>, py: seq<real>) 
  requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)
  ensures |y| == |ampy| == |py|
  ensures forall k :: 0 <= k < |y| ==> |y[k]| == |x|
  ensures |y| == pow2(|x|)
  ensures forall k :: 0 <= k < |y| ==> castBVInt(y[k]) == k 
  ensures forall k :: 0 <= k < |ampy| ==> 
                            assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
                            ampy[k] == 1.0 / (sqrt(pow2(|x|) as real))
  ensures forall k :: 0 <= k < |py| ==> py[k] == omega(0,2)

method amplitude(x:seq<seq<bv1>>, n: nat) returns (cx: nat, xa: real, cy: nat, ya: real) 
//x is hadtype. secret initial a, unknow to the algorithm. 
    requires n > 0
    requires |x| == pow2(n)
    ensures cx + cy == |x|
    ensures xa*xa*cx as real + ya*ya*cy as real == 1.0
{
    cx := 0;
    var i := 0;
    while (i < |x|)
        invariant 0 <= i <= |x|
        invariant 0 <= cx <= i
    {
        if f(x[i]) == 1
        {
            cx := cx + 1;
        }
        i := i + 1;
    }
    cy := |x| - cx;
 //   assert pow2(n) > 0;
    assert sqrt(|x| as real) > 0.0 by {SqrtGt(pow2(n) as real);}
    xa := 1.0/sqrt(|x| as real);
     
    ya := xa;
    assert ya*ya*(cy as real) >= 0.0;
    assert xa*xa*(cx as real) >= 0.0;
    assert xa*xa == 1.0/(|x| as real) by {sqSqrt(1.0/(sqrt(|x| as real)));}
    assert xa >= 0.0;
    assert ya >= 0.0;
    assert cx + cy == |x|;
    assert xa*xa*cx as real > 0.0;
    assert xa*xa*cx as real + ya*ya*cy as real <= 1.01;
    
    return cx, xa, cy, ya;
}



//Q = -A^(-1)X_0AX_f
//function{:axiom} Q(A, X): seq<seq<bv1>>

predicate equalBv (x : seq<seq<bv1>>, target:bv1) 
{
  forall k :: 0 <= k < |x| ==> f(x[k])==target
}

method flipsign (xa:real) returns (xa_:real)
    requires xa > 0.0
    ensures xa_ == - xa
{
    xa_ := - xa;
}


method inversion (cx: int, xa:real, cy: int, ya: real) returns (mu: real, xa_:real, ya_:real)
    requires xa < 0.0
    requires xa*xa*cx as real + ya*ya*cy as real == 1.0
    ensures xa_ / cx as real == 2.0 * mu - xa
    ensures ya_ / cx as real == 2.0 * mu - ya
    ensures xa_*xa_*cx as real + ya_*ya_*cy as real == 1.0
    ensures xa_ > xa
    ensures ya_ < ya

{
    //compute mu
    mu := cx as real *xa + cy as real *ya;
    xa_ := 2.0 * mu - xa;
    ya_ := 2.0 * mu - ya;
}


//how to assume that the number of good states m is less than N/4? But with unknow m, make it a dummy variable?
method qstep(q: seq<bv1>, n: nat, t: nat) returns (xa_:real, ya_:real)
    requires forall i :: 0 <= i < |q| ==> q[i] == 0
    requires |q| == n
 //   requires n >= 4*m
    ensures xa_ > ya_
{
    var hadq := hadNorHad(q);
    var enq, _, _ := hadEn(hadq);
    var cx, xa, cy, ya:= amplitude(enq, n);
    ghost var m: int := cx;
    assume{:axiom} n >= 4*m;
    var i := 0;
    while (i < t)
        invariant 0 <= i <= t
        invariant xa_ >= xa
        invariant ya_ >= ya

    {
        xa := xa_;
        ya := ya_;
        var tmp := flipsign(xa);
        var mu, xa_, ya_ := inversion(cx, xa, cy, ya);
    }


}
//maybe we can resuse q_op

// method qstep(A, X, q<bv1>, j) returns (theta, a, updated_a)
//     ensures sin(2j+1)theta = sqrt(updated_a)
//     ensures a <= 1
//     ensures updated_a > a if a != 1 else updated_a == 1
// {
// }

// method ctrlq(p, q) returns()
//     ensures the eigenvalues of q in p
//  {

//  }

// method QFT(q) return (updated_q)
//     ensures |q| = |updated_q|
// {
// }

// //how to distinguish between superposition and computational basis?
// method measure(q: seq<bv1>) returns (y: seq<bv1>, p)
//     ensures 
// {
// }

