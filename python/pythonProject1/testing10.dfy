function {:axiom} omega(n:nat, a:nat): real

function {:axiom} sqrt(a:real): real
  requires a > 0.0
  ensures sqrt(a) > 0.0

lemma {:axiom} omega0()
  ensures forall k : nat :: omega(0, k) == 1.0
/*function castBVInt(x : seq<bv1>) : nat
ensures castBVInt(x) == if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
{
   if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
}*/

function {:axiom} castBVInt(x : seq<bv1>) : nat
  ensures castBVInt(x) >= 0

function {:axiom} castIntBV(x: int) : seq<bv1>
  ensures castBVInt(castIntBV(x)) == x

/*function pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}*/

function {:axiom} pow2(N:nat): int
  ensures pow2(N) > 0


lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0

function {:axiom} abs(n : int) : nat
  ensures abs(n) == if n >= 0 then n else -n


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


method conditionaltest3(n: nat, amp: seq<real>, q: seq<seq<bv1>>, p: seq<seq<bv1>>) returns (amp3: seq<seq<real>>, q3: seq<seq<seq<bv1>>>, p3: seq<seq<seq<bv1>>>)
  requires n > 0
  requires |q| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |q[tmp2]| == n
  requires |p| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |p[tmp2]| == 1
  requires |amp| == pow2(n)
  requires forall k :: 0 <= k < pow2(n) ==> amp[k] == (1.0 / sqrt((pow2(n) as real)))
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(q[k]) == k
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(p[k]) == (k % 2)
  ensures |q3| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < pow2(n) ==> |q3[tmp4]| == pow2(1)
  ensures forall tmp4 :: 0 <= tmp4 < pow2(n) ==> forall tmp5 :: 0 <= tmp5 < pow2(1) ==> |q3[tmp4][tmp5]| == n
  ensures |p3| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < pow2(n) ==> |p3[tmp4]| == pow2(1)
  ensures |amp3| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < pow2(n) ==> |amp3[tmp4]| == pow2(1)
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(1) ==> amp3[k][j] == if q3[k][j][0] == 1 then ((1.0 / sqrt((pow2((n + 1)) as real))) * omega((k % 2), abs(2))) else (1.0 / sqrt((pow2(n) as real)))
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(q3[k][j]) == k
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(p3[k][j]) == if q3[k][j][0] == 1 then j else (k % 2)
{
  var amp2 := [];
  var q2 := [];
  var p2 := [];
  var nvar3 := 0;
  while(nvar3 < |q|)
  invariant 0 <= nvar3 && nvar3 <= |amp|
  invariant 0 <= nvar3 && nvar3 <= |q|
  invariant 0 <= nvar3 && nvar3 <= |p|
  invariant |amp2| == nvar3
  invariant |q2| == nvar3
  invariant |p2| == nvar3
  invariant forall tmp4 :: 0 <= tmp4 < |amp2| ==> |amp2[tmp4]| == pow2(1)
  invariant forall tmp5 :: 0 <= tmp5 < |q2| ==> |q2[tmp5]| == pow2(1)
  invariant forall tmp6 :: 0 <= tmp6 < |p2| ==> |p2[tmp6]| == pow2(1)
  invariant forall k :: 0 <= k < |q2| ==> forall j :: 0 <= j < |q2[k]| ==> |q2[k][j]| == |q[k]|
  invariant forall tmp4 :: 0 <= tmp4 < |amp2| ==> forall tmp5 :: 0 <= tmp5 < |amp2[tmp4]| ==> amp2[tmp4][tmp5] == if q2[tmp4][tmp5][0] == 1 then ((1.0 / sqrt((pow2((n + 1)) as real))) * omega((tmp4 % 2), 2)) else (1.0 / sqrt((pow2(n) as real)))   
  invariant forall tmp4 :: 0 <= tmp4 < |q2| ==> forall tmp5 :: 0 <= tmp5 < |q2[tmp4]| ==> castBVInt(q2[tmp4][tmp5]) == tmp4
  invariant forall tmp4 :: 0 <= tmp4 < |p2| ==> forall tmp5 :: 0 <= tmp5 < |p2[tmp4]| ==> castBVInt(p2[tmp4][tmp5]) == if q2[tmp4][tmp5][0] == 1 then tmp5 else (tmp4 % 2)

{
  var tmp_amp;
  var tmp_q;
  tmp_q := duplicateSeq(q[nvar3], pow2(|p[nvar3]|));
  tmp_amp := duplicateAmp(amp[nvar3], pow2(|p[nvar3]|));
  q2 := (q2 + [tmp_q]);
  if (q[nvar3][0] == 1){
  var tmp_p;
  tmp_p := partialcastEn1toEn2(p[nvar3]);
  tmp_amp := ampMul(tmp_amp, pow2(|p[nvar3]|), p[nvar3]);
  pow2mul();
  sqrtmul();
  p2 := (p2 + [tmp_p]);
}
else {
  var tmp_p;
  tmp_p := duplicateSeq(p[nvar3], pow2(|p[nvar3]|));
  p2 := (p2 + [tmp_p]);
}
  amp2 := (amp2 + [tmp_amp]);
  nvar3 := (nvar3 + 1);
  
  omega0();
}
  
  amp3, q3, p3 := amp2, q2, p2;
}
