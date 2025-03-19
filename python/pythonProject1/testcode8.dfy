function {:axiom} omega(n:nat, a:nat): real

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
{
    if (k == 0) then 1 else N * powN(N, k-1)
}


lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0




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

function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
    requires |x| >= n
    requires |y| >= n
    ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]


method {:axiom} cutHad(x: seq<real>) returns (x1: seq<real>)
    ensures |x1| == |x| - 1
    ensures forall k :: 0 <= k < |x1| ==> x1[k] == x[k+1]


method {:axiom} mergeBitEn(x: seq<seq<bv1>>, n : nat) returns (x1: seq<seq<bv1>>)
    requires forall k :: 0 <= k < |x| ==> |x[k]| == n
    ensures |x1| == |x| * 2
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
    ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k][1..], n)
    ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k + |x|][1..], n)
    ensures forall k :: 0 <= k < |x1|/2 ==> x1[k][0] == 0
    ensures forall k :: |x1|/2 <= k < |x1| ==> x1[k][0] == 1



method {:axiom} doubleEn(x: seq<seq<bv1>>, n: nat) returns (x1:seq<seq<bv1>>)
    requires forall k :: 0 <= k < |x| ==> |x[k]| == n
    ensures |x1| == |x|*2
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == n
    ensures forall k :: 0 <= k < |x| ==> samebit(x1[k], x[k], n)
    ensures forall k :: 0 <= k < |x| ==> castBVInt(x1[k]) == castBVInt(x[k])
    ensures forall k :: 0 <= k < |x| ==> samebit(x1[k], x1[k + |x|], n)
    ensures forall k :: 0 <= k < |x| ==> castBVInt(x1[k]) == castBVInt(x1[k + |x|])

method {:axiom} doubleAmpEn(amp: seq<real>) returns (amp1: seq<real>)
    ensures |amp1| == |amp| * 2
    ensures forall k :: 0 <= k < |amp| ==> amp1[k] == amp[k] * (1.0/sqrt(2 as real))
    ensures forall k :: |amp| <= k < |amp1| ==> amp1[k] == amp[k-|amp|] * (1.0/sqrt(2 as real))


method {:axiom} lambda (x: seq<bv1>, base: nat, N: nat, i : nat) returns (x1: seq<bv1>)
    requires N > 0
    ensures |x| == |x1|
    ensures castBVInt(x1) == ((powN(base, pow2(i)) * (castBVInt(x)))% N)

lemma {:axiom} trigger1(x1: seq<seq<bv1>>, m:nat, n:nat)
    requires |x1| == 2 * m
    requires forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
    requires forall k :: 0 <= k < m ==> samebit(x1[k][1..], x1[k+m][1..], n)
    requires forall k :: 0 <= k < m ==> x1[k][0] == 0
    requires forall k :: m <= k < |x1| ==> x1[k][0] == 1
    //ensures forall k :: 0 <= k < |x1| ==> b2n(x1[k], n + 1) == k
    ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k

lemma {:axiom} trigger2(x1: seq<seq<bv1>>, m:nat, n:nat)
    requires |x1| == 2 * m
    requires forall k :: 0 <= k < |x1| ==> |x1[k]| == n
    ensures forall k :: 0 <= k < m ==> samebit(x1[k], x1[k+m], n)
    ensures forall k :: 0 <= k < m ==> castBVInt(x1[k]) == castBVInt(x1[k+m])


method conditionaltest1(n: nat, q: seq<seq<bv1>>, q1: seq<real>,  p: seq<seq<bv1>>, amp: seq<real>, i: nat, base: nat, N : nat)
    returns (q2: seq<seq<bv1>>, p2: seq<seq<bv1>>, amp2: seq<real>, q3: seq<real>)
  requires N > 0
  requires base > 0
  requires 0 < i < n
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
  //ensures forall k :: 0 <= k < pow2(i+1) ==> castBVInt(p2[k]) == powN(base, k) % N
  ensures forall k :: 0 <= k < pow2(i+1) ==> castBVInt(q2[k]) == k
  ensures forall k :: 0 <= k < pow2(i+1) ==> amp2[k] == (1.0 / sqrt(pow2(i) as real)) * (1.0 / sqrt(2 as real))
  ensures forall k :: 0 <= k < (n-(i+1)) ==> q3[k] == omega(0, 2)
{

    var q5 := cutHad(q1);
    var q4 := mergeBitEn(q, |q[0]|);
    var p4 := doubleEn(p, |p[0]|);
    var amp4 := doubleAmpEn(amp);
    trigger1(q4, |q|, |q[0]|);
    trigger2(p4, |q|, |p[0]|);

    var tmp := 0;
    var p5: seq<seq<bv1>> := [];
    while (tmp < |q4|)
    invariant 0 <= tmp && tmp <= |q4|
    invariant 0 <= tmp && tmp <= |p4|
    invariant |p5| == tmp
    invariant forall k :: 0 <= k < |p5| ==> |p5[k]| == n
    invariant forall k :: 0 <= k < |p5| ==> if q4[k][0] == 1 then castBVInt(p5[k]) == ((powN(base, pow2(i)) * (castBVInt(p4[k]))) % N) else castBVInt(p5[k]) == castBVInt(p4[k])
    {
        var tmmp := p4[tmp];
        if (q4[tmp][0] == 1){
            tmmp := lambda(tmmp, base, N, i);
        }
        p5 := p5 + [tmmp];
        tmp := tmp + 1;
    }
    q2, p2, amp2 := q4, p5, amp4;
    q3 := q5;
}
