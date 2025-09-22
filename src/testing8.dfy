function {:axiom} omega(n:nat, a:nat): real


/*lemma {:axiom} omega0(a:real)
  requires a == omega(0, 2)
  ensures a == 1.0*/

lemma {:axiom} omega0()
  ensures forall k : nat :: omega(0, k) == 1.0



function {:axiom} sqrt(a:real): real
  requires a > 0.0
  ensures sqrt(a) > 0.0
function castBVInt(x : seq<bv1>) : nat
ensures castBVInt(x) == if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
{
   if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
}
function b2n (x:seq<bv1>, i:nat) : nat
  requires i <= |x|
{
  if (i==0) then 0 else (x[i-1] as nat) * pow2(i-1) + b2n(x[..i-1],i-1)
}
/*function {:axiom} b2n(x : seq<bv1>, n:int) : nat
  requires n <= |x|
  ensures b2n(x, n) == if |x| == 0 then 0 else (x[0] as nat) + 2 * b2n(x[1..], n-1)*/
function pow2(N:nat):nat
  ensures pow2(N) > 0
{
    if (N==0) then 1 else 2 * pow2(N-1)
}
function powN(N:nat, k: nat) : int
  ensures powN(N, k) > 0
{
    if (N == 0) then 1 else if (k == 0) then 1 else N * powN(N, k-1)
}
lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0
//lemma {:axiom} Pow2Up(n:nat)
//  ensures pow2(n) * 2 == pow2(n + 1)
method hadNorHad(x:seq<bv1>) returns (y : seq<real>)
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |x| ==> y[k] == omega(x[k] as int,2)
{
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
method {:axiom} hadEn(x: seq<real>, y: seq<real>)
            returns (amp: seq<real>, x1: seq<seq<bv1>>, y1 : seq<seq<bv1>>) 
  requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)
  ensures |x1| == |y1| == |amp| == |x|
  ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == pow2(|y|)
  ensures forall k :: 0 <= k < |y1| ==> |y1[k]| == pow2(|y|) 
  ensures forall k :: 0 <= k < |y1| ==> castBVInt(y1[k]) == k
  ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
  ensures forall k :: 0 <= k < |amp| ==> amp[k] == (1.0 / (sqrt(pow2(|x|) as real))) * omega(0,2)
method {:axiom} hadNorEn(x: seq<real>, y: seq<bv1>) returns (amp1: seq<real>, x1: seq<seq<bv1>>, y1: seq<seq<bv1>>)
    ensures |x1| == |y1| == |amp1| == pow2(|x|)
    ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |x|
    ensures forall k :: 0 <= k < |y1| ==> y1[k] == y
    ensures forall k :: 0 <= k < |amp1| ==> amp1[k] == (1.0 / sqrt(pow2(|x|) as real))
method {:axiom} partialcastEn1toEn2(x: seq<bv1>) returns (x1 : seq<seq<bv1>>)
  ensures |x1| == pow2(|x|)
  ensures forall k :: 0 <= k < pow2(|x|) ==> |x1[k]| == |x|
  ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
method {:axiom} duplicateSeq(x: seq<bv1>, n:nat) returns (x1:seq<seq<bv1>>)
    ensures |x1| == n
    ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
method {:axiom} createAmp(n:nat) returns(amp:seq<real>)
  ensures |amp| == n
  ensures forall k :: 0 <= k < |amp| ==> assert sqrt(n as real) > 0.0 by {SqrtGt(n as real);}
                                          amp[k] == 1.0/sqrt(n as real)
function samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
    requires |x| >= n
    requires |y| >= n
{
  forall k :: 0 <= k < n ==> x[k] == y[k]
}
method {:axiom} cutHad(x: seq<real>) returns (x1: seq<real>)
    requires 0 < |x|
    ensures |x1| == |x| - 1
    ensures forall k :: 0 <= k < |x1| ==> x1[k] == x[k+1]
method {:axiom} mergeBitEn(x: seq<seq<bv1>>, n : nat) returns (x1: seq<seq<bv1>>)
    requires forall k :: 0 <= k < |x| ==> |x[k]| == n
    ensures |x1| == |x| * 2
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
    ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k][0..n], n)
    ensures forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k][0..n], n) 
    ensures forall k :: 0 <= k < |x| ==> x1[k][n] == 0
    ensures forall k :: |x| <= k < |x1| ==> x1[k][n] == 1
method {:axiom} mergeAmpEn(amp: seq<real>, q : real) returns (amp1: seq<real>)
    ensures |amp1| == |amp| * 2
    ensures forall k :: 0 <= k < |amp| ==> amp1[k] == 1.0 / sqrt(2.0) * amp[k]
    ensures forall k :: |amp| <= k < |amp1| ==> amp1[k] == 1.0 / sqrt(2.0) * amp[k-|amp|] * q
method {:axiom} doubleEn(x: seq<seq<bv1>>, n: nat) returns (x1:seq<seq<bv1>>)
    requires forall k :: 0 <= k < |x| ==> |x[k]| == n
    ensures |x1| == |x|*2
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == n
    ensures forall k :: 0 <= k < |x| ==> samebit(x1[k], x[k], n)
    ensures forall k :: |x| <= k < |x1| ==> samebit(x1[k], x[k - |x|], n)
method {:axiom} doubleAmpEn(amp: seq<real>) returns (amp1: seq<real>)
    ensures |amp1| == |amp| * 2
    ensures forall k :: 0 <= k < |amp| ==> amp1[k] == amp[k] * (1.0/sqrt(2 as real))
    ensures forall k :: |amp| <= k < |amp1| ==> amp1[k] == amp[k-|amp|] * (1.0/sqrt(2 as real))
method {:axiom} lambda (x: seq<bv1>, base: nat, N: nat, i : nat) returns (x1: seq<bv1>)
    requires N > 0
    ensures |x| == |x1|
    ensures castBVInt(x1) == ((powN(base, pow2(i)) * (castBVInt(x)))% N)
lemma {:axiom} trigger1(x: seq<seq<bv1>>, x1: seq<seq<bv1>>, n:nat)
    requires forall k :: 0 <= k < |x| ==> |x[k]| == n
    requires forall k :: 0 <= k < |x| ==> castBVInt(x[k]) == k
    requires |x1| == |x| * 2
    requires forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
    requires forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k][0..n], n)
    requires forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k][0..n], n) 
    requires forall k :: 0 <= k < |x| ==> x1[k][n] == 0
    requires forall k :: |x| <= k < |x1| ==> x1[k][n] == 1
    ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
    
lemma {:axiom} trigger2(x1: seq<seq<bv1>>, m:nat, n:nat)
    requires |x1| == 2 * m
    requires forall k :: 0 <= k < |x1| ==> |x1[k]| == n
    ensures forall k :: 0 <= k < m ==> samebit(x1[k], x1[k+m], n)
    ensures forall k :: 0 <= k < m ==> castBVInt(x1[k]) == castBVInt(x1[k+m])
lemma {:axiom} triggerPowMul(x : seq<seq<bv1>>, i:nat, base:nat, N:nat)
    requires 0 < N
    requires |x| == pow2(i+1)
    requires forall k :: 0 <= k < pow2(i) ==> castBVInt(x[k]) == powN(base, k) % N
    requires forall k :: pow2(i) <= k < |x| ==> castBVInt(x[k]) == powN(base, k+pow2(i)) % N
    ensures forall k :: 0 <= k < |x| ==> castBVInt(x[k]) == powN(base, k) % N
/*lemma omega0List(amp:seq<real>)
  requires forall k :: 0 <= k < |amp| ==> amp[k] == omega(0,2)
  ensures forall k :: 0 <= k < |amp| ==> amp[k] == 1.0 
{
  var i := 0;
  while (i < |amp|)
    invariant 0 <= i <= |amp|
    invariant forall k :: 0 <= k < i ==> amp[k] == 1.0
  {
    omega0(amp[i]);
    i := i + 1;
  }
}*/
//the following can be some trigger rule, we just need to say sqrt(a) * sqrt(b) == sqrt(a*b)
lemma {:axiom} triggerSqrtMul(amp:seq<real>, i:nat)
  requires forall k :: 0 <= k < |amp| ==> amp[k] == (1.0 / (sqrt(pow2(i) as real) * sqrt(2 as real)))
  ensures forall k :: 0 <= k < |amp| ==> amp[k] == (1.0 / (sqrt((pow2(i) * 2) as real)))




lemma {:axiom} powNTimes(a:seq<bv1>, base:nat, x:nat, y:nat, N:nat)
  requires 0 < N
  requires castBVInt(a) == ((powN(base, pow2(x)) * (powN(base, y) % N))% N)
  ensures castBVInt(a) == powN(base, pow2(x) + y) % N
method {:axiom} mergeBitSeq(a:seq<seq<bv1>>, b:seq<seq<bv1>>, base:nat, N:nat, i:nat)
  returns (c:seq<seq<bv1>>)
  requires 0 < N
  requires |a| == |b| == pow2(i)
  requires forall k :: 0 <= k < |a| ==> castBVInt(a[k]) == powN(base, k) % N
  requires forall k :: 0 <= k < |b| ==> castBVInt(b[k]) == powN(base, pow2(i) + k) % N
  ensures |c| == pow2(i+1)
  ensures forall k :: 0 <= k < |c| ==> castBVInt(c[k]) == powN(base, k) % N
method conditionaltest1(n: nat, q: seq<seq<bv1>>, q1: seq<real>,  p: seq<seq<bv1>>, amp: seq<real>, i: nat, base: nat, N : nat)
    returns (q2: seq<seq<bv1>>, p2: seq<seq<bv1>>, amp2: seq<real>, q3: seq<real>)
  requires 0 < base < N 
  requires 0 <= i < n
  requires |p| == |q| == |amp| == pow2(i)
  requires forall k :: 0 <= k < |p| ==> |p[k]| == n
  requires forall k ::0 <= k < |q| ==> |q[k]| == i
  requires |q1| == n - i
  requires forall k :: 0 <= k < pow2(i) ==> castBVInt(p[k]) == powN(base, k) % N
  requires forall k :: 0 <= k < pow2(i) ==> castBVInt(q[k]) == k
  requires forall k :: 0 <= k < pow2(i) ==> amp[k] == (1.0 / sqrt(pow2(i) as real))
  requires forall k :: 0 <= k < (n-i) ==> q1[k] == omega(0, 2)
  ensures |q2| == pow2(i+1)
  ensures |p2| == pow2(i+1)
  ensures |amp2| == pow2(i+1)
  ensures |q3| == n - (i+1)
  ensures forall k :: 0 <= k < pow2(i+1) ==> castBVInt(p2[k]) == powN(base, k) % N
  ensures forall k :: 0 <= k < pow2(i+1) ==> castBVInt(q2[k]) == k
  ensures forall k :: 0 <= k < pow2(i+1) ==> amp2[k] == (1.0 / (sqrt(pow2(i+1) as real)))
  ensures forall k :: 0 <= k < (n-(i+1)) ==> q3[k] == omega(0, 2)
{
    q3 := cutHad(q1);
    var q10 := q1[0];
    var q4 := mergeBitEn(q, i);
    trigger1(q, q4, i);
    omega0();
    //omega0(q10);
    //omega0List(q1);
    amp2 := mergeAmpEn(amp, q10);
    triggerSqrtMul(amp2, i);
    var p3 := [];
    var tmp := 0;
    while (tmp < |p|)
      invariant 0 <= tmp <= |p|
      invariant |p3| == tmp
      invariant forall k :: 0 <= k < |p3| ==> |p3[k]| == |p[k]|
      invariant forall k :: 0 <= k < |p3| ==> castBVInt(p3[k]) == powN(base, pow2(i) + k) % N
    {
       var tmmp := lambda(p[tmp], base, N, i);
       powNTimes((tmmp), base, i, tmp, N);
       p3 := p3 + [tmmp];
       tmp := tmp + 1;
    }
   q2 := q4;
   p2 := mergeBitSeq(p, p3, base, N, i);

}