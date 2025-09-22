function {:axiom} omega(n:nat, a:nat): real

function {:axiom} sqrt(a:real): real
  requires a > 0.0
  ensures sqrt(a) > 0.0


function {:axiom} castBVInt(x : seq<bv1>) : nat
  ensures castBVInt(x) >= 0
/*function castBVInt(x : seq<bv1>) : nat
ensures castBVInt(x) == if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
{
   if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
}*/

lemma {:axiom} omega0()
  ensures forall k : nat :: omega(0, k) == 1.0


function pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
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

method {:axiom} ampMul(amp : seq<real>, x: nat, y : seq<bv1>) returns (amp1: seq<real>)
    requires x > 0
    ensures |amp| == |amp1|
    ensures forall k :: 0 <= k < |amp| ==> amp1[k] == (amp[k] * (1.0/sqrt(x as real))) * omega(castBVInt(y), 2)


function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
    requires |x| >= n
    requires |y| >= n
    ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]

method {:axiom} duplicateSeq(x: seq<bv1>, n:nat) returns (x1:seq<seq<bv1>>)
    ensures |x1| == n
    ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
    ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x)
    ensures forall k :: 0 <= k < |x1| ==> samebit(x1[k], x, |x|)
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |x|

method {:axiom} duplicateAmp(x: real, n:nat) returns (x1:seq<real>)
    ensures |x1| == n
    ensures forall k :: 0 <= k < |x1| ==> x1[k] == x

method {:axiom} createAmp(n:nat) returns(amp:seq<real>)
  ensures |amp| == n
  ensures forall k :: 0 <= k < |amp| ==> assert sqrt(n as real) > 0.0 by {SqrtGt(n as real);}
                                          amp[k] == 1.0/sqrt(n as real)

lemma {:axiom} pow2mul()
  ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)

lemma {:axiom} sqrtmul()
  ensures forall k : real, j : real :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)

method conditionaltest2(n: nat, base: nat, N: nat, q: seq<real>, p1: seq<bv1>) returns (amp7: seq<seq<real>>, q7: seq<seq<seq<bv1>>>, p7: seq<seq<seq<bv1>>>)
  requires |q| == 1
  requires |p1| == n
  requires forall tmp3 :: 0 <= tmp3 < 1 ==> q[tmp3] == omega(0, 2)
  requires castBVInt(p1) == 0
  requires forall tmp4 :: 0 <= tmp4 < n ==> p1[tmp4] == 0
  ensures |q7| == pow2(1)
  ensures forall tmp8 :: 0 <= tmp8 < pow2(1) ==> |q7[tmp8]| == pow2(n)
  ensures |p7| == pow2(1)
  ensures forall tmp8 :: 0 <= tmp8 < pow2(1) ==> |p7[tmp8]| == pow2(n)
  ensures |amp7| == pow2(1)
  ensures forall tmp8 :: 0 <= tmp8 < pow2(1) ==> |amp7[tmp8]| == pow2(n)
  ensures forall k :: 0 <= k < |q7| ==> forall j :: 0 <= j < |q7[k]| ==> |q7[k][j]| == 1
  ensures forall k :: 0 <= k < 2 ==> forall j :: 0 <= j < pow2(n) ==> amp7[k][j] == if q7[k][j][0] == 1 then (1.0 / sqrt((pow2((n + 1)) as real))) * omega(0, 2) else (1.0 / sqrt((2 as real)))
  ensures forall k :: 0 <= k < 2 ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(q7[k][j]) == k
  ensures forall k :: 0 <= k < 2 ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(p7[k][j]) == if q7[k][j][0] == 1 then j else 0
{
  var amp5, q5, p5 := hadNorEn(q, p1);
  var amp6 := [];
  var q6 := [];
  var p6 := [];
  var nvar7 := 0;
  while(nvar7 < |q5|)
  invariant 0 <= nvar7 && nvar7 <= |amp5|
  invariant 0 <= nvar7 && nvar7 <= |q5|
  invariant 0 <= nvar7 && nvar7 <= |p5|
  invariant |amp6| == nvar7
  invariant |q6| == nvar7
  invariant |p6| == nvar7
  invariant forall tmp8 :: 0 <= tmp8 < |amp6| ==> |amp6[tmp8]| == pow2(n)
  invariant forall tmp9 :: 0 <= tmp9 < |q6| ==> |q6[tmp9]| == pow2(n)
  invariant forall tmp10 :: 0 <= tmp10 < |p6| ==> |p6[tmp10]| == pow2(n)
  invariant forall k :: 0 <= k < |q6| ==> forall j :: 0 <= j < |q6[k]| ==> |q6[k][j]| == 1
  invariant forall tmp8 :: 0 <= tmp8 < |amp6| ==> forall tmp9 :: 0 <= tmp9 < |amp6[tmp8]| ==> amp6[tmp8][tmp9] == if q6[tmp8][tmp9][0] == 1 then ((1.0 / sqrt((pow2((1 + n)) as real))) * omega(0, 2)) else (1.0 / sqrt((pow2(1) as real)))
  invariant forall tmp8 :: 0 <= tmp8 < |q6| ==> forall tmp9 :: 0 <= tmp9 < |q6[tmp8]| ==> castBVInt(q6[tmp8][tmp9]) == tmp8
  invariant forall tmp8 :: 0 <= tmp8 < |p6| ==> forall tmp9 :: 0 <= tmp9 < |p6[tmp8]| ==> castBVInt(p6[tmp8][tmp9]) == if q6[tmp8][tmp9][0] == 1 then tmp9 else 0

{
  var tmp_amp;
  var tmp_q;
  tmp_q := duplicateSeq(q5[nvar7], pow2(|p5[nvar7]|));
  tmp_amp := duplicateAmp(amp5[nvar7], pow2(|p5[nvar7]|));
  q6 := (q6 + [tmp_q]);
  if (q5[nvar7][0] == 1){
  var tmp_p;
  tmp_p := partialcastEn1toEn2(p5[nvar7]);
  tmp_amp := ampMul(tmp_amp, pow2(|p5[nvar7]|), p5[nvar7]);
  pow2mul();
  sqrtmul();
  p6 := (p6 + [tmp_p]);
}
else {
  var tmp_p;
  tmp_p := duplicateSeq(p5[nvar7], pow2(|p5[nvar7]|));

  p6 := (p6 + [tmp_p]);
  
}
  amp6 := (amp6 + [tmp_amp]);
  nvar7 := (nvar7 + 1);
}
  amp7, q7, p7 := amp6, q6, p6;
}

