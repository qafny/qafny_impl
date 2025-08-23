lemma {:axiom} pow2add()
                      ensures forall k : nat :: pow2(k) * 2 == pow2(k + 1)

lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0


      method {:axiom} hadNorEn(x: seq<real>, y: seq<bv1>) returns (amp1: seq<real>, x1: seq<seq<bv1>>, y1: seq<seq<bv1>>)
        ensures |x1| == |y1| == |amp1| == pow2(|x|)
        ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
        ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |x|
        ensures forall k :: 0 <= k < |y1| ==> y1[k] == y
        ensures forall k :: 0 <= k < |amp1| ==> amp1[k] == (1.0 / sqrt(pow2(|x|) as real))



      function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
          requires |x| >= n
          requires |y| >= n
          ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]


function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

function {:axiom} omega(n:nat, a:nat): real

lemma {:axiom} triggerSqrtMul()
                      ensures forall k, j :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)

function {:axiom} powN(N:nat, k: nat) : int
                    ensures powN(N, k) > 0

method {:axiom} ampMul(amp : seq<real>, x: nat, y : seq<bv1>) returns (amp1: seq<real>)
          requires x > 0
          ensures |amp| == |amp1|
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == (amp[k] * (1.0/sqrt(x as real))) * omega(castBVInt(y), 2)



    method {:axiom} duplicateAmp(x: real, n:nat) returns (x1:seq<real>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x


lemma {:axiom} powNTimesMod()
          ensures forall k: nat, j: nat, l : nat, N:nat :: N > 0 ==> powN(k, j) * (powN(k, l) % N) % N == powN(k, j + l) % N

function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)

method {:axiom} partialcastEn1toEn2(x: seq<bv1>) returns (x1 : seq<seq<bv1>>)
          ensures |x1| == pow2(|x|)
          ensures forall k :: 0 <= k < pow2(|x|) ==> |x1[k]| == |x|
          ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k



      method {:axiom} duplicateSeq(x: seq<bv1>, n:nat) returns (x1:seq<seq<bv1>>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
        ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x)
        ensures forall k :: 0 <= k < |x1| ==> samebit(x1[k], x, |x|)


method conditionaltest2(n: nat, base: nat, N: nat, q: seq<real>, p1: seq<bv1>) returns (amp7: seq<seq<real>>, q7: seq<seq<seq<bv1>>>, p7: seq<seq<seq<bv1>>>)
  requires |q| == 1
  requires n > 0
  requires |p1| == n
  requires forall tmp3 :: 0 <= tmp3 < 1 ==> q[tmp3] == omega(0, 2)
  requires forall tmp3 :: 0 <= tmp3 < 1 ==> q[tmp3] == omega(0, 2)
  requires castBVInt(p1) == 0
  requires forall tmp4 :: 0 <= tmp4 < n ==> p1[tmp4] == 0
  ensures |q7| == pow2(1)
  ensures forall tmp8 :: 0 <= tmp8 < |q7| ==> |q7[tmp8]| == pow2(n)
  ensures forall tmp8 :: 0 <= tmp8 < |q7| ==> forall tmp9 :: 0 <= tmp9 < |q7[tmp8]| ==> |q7[tmp8][tmp9]| == 1
  ensures |p7| == pow2(1)
  ensures forall tmp8 :: 0 <= tmp8 < |p7| ==> |p7[tmp8]| == pow2(n)
  ensures forall tmp8 :: 0 <= tmp8 < |p7| ==> forall tmp9 :: 0 <= tmp9 < |p7[tmp8]| ==> |p7[tmp8][tmp9]| == n
  ensures |amp7| == pow2(1)
  ensures forall tmp8 :: 0 <= tmp8 < |amp7| ==> |amp7[tmp8]| == pow2(n)
  ensures forall k :: 0 <= k < pow2(1) ==> forall j :: 0 <= j < pow2(n) ==> amp7[k][j] == if q7[k][j][0] == 1 then (1.0 / sqrt((pow2((n + 1)) as real))) else (1.0 / sqrt((2 as real)))
  ensures forall k :: 0 <= k < pow2(1) ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(q7[k][j]) == k
  ensures forall k :: 0 <= k < pow2(1) ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(p7[k][j]) == if q7[k][j][0] == 1 then j else 0
{
  var amp5, q5, p5 := hadNorEn(q, p1);
  var q6 := [];
  var p6 := [];
  var amp6 := [];
  var tmp := 0;
  while(tmp < |q5|)
  invariant 0 <= tmp <= |q5|
  invariant 0 <= tmp <= |p5|
  invariant 0 <= tmp <= |amp5|
  invariant |q6| == tmp
  invariant |p6| == tmp
  invariant |amp6| == tmp
  invariant forall tmp7 :: 0 <= tmp7 < |q6| ==> |q6[tmp7]| == pow2(n)
  invariant forall tmp7 :: 0 <= tmp7 < |p6| ==> |p6[tmp7]| == pow2(n)
  invariant forall tmp7 :: 0 <= tmp7 < |amp6| ==> |amp6[tmp7]| == pow2(n)
  invariant forall tmp7 :: 0 <= tmp7 < |q6| ==> forall tmp8 :: 0 <= tmp8 < |q6[tmp7]| ==> |q6[tmp7][tmp8]| == 1
  invariant forall tmp7 :: 0 <= tmp7 < |p6| ==> forall tmp8 :: 0 <= tmp8 < |p6[tmp7]| ==> |p6[tmp7][tmp8]| == n
  invariant forall tmp7 :: 0 <= tmp7 < |q6| ==> forall tmp8 :: 0 <= tmp8 < |q6[tmp7]| ==> castBVInt(q6[tmp7][tmp8]) == if q5[tmp7][0] == 1 then castBVInt(q5[tmp7]) else castBVInt(q5[tmp7])
  invariant forall tmp7 :: 0 <= tmp7 < |p6| ==> forall tmp8 :: 0 <= tmp8 < |p6[tmp7]| ==> castBVInt(p6[tmp7][tmp8]) == if q5[tmp7][0] == 1 then tmp8 else castBVInt(p5[tmp7])
  invariant forall tmp7 :: 0 <= tmp7 < |amp6| ==> forall tmp8 :: 0 <= tmp8 < |amp6[tmp7]| ==> amp6[tmp7][tmp8] == if q5[tmp7][0] == 1 then (amp5[tmp7] * ((1.0 / sqrt((pow2(|p6[tmp7][tmp8]|) as real))) * omega(castBVInt(p5[tmp7]), 2))) else amp5[tmp7]
  invariant forall tmp7 :: 0 <= tmp7 < |q6| ==> forall tmp8 :: 0 <= tmp8 < |q6[tmp7]| ==> samebit(q6[tmp7][tmp8], q5[tmp7], |q5[tmp7]|)
{
  var tmp_q := duplicateSeq(q5[tmp], pow2(|p5[tmp]|));
  var tmp_p := duplicateSeq(p5[tmp], pow2(|p5[tmp]|));
  var tmp_amp := duplicateAmp(amp5[tmp], pow2(|p5[tmp]|));
  if (q5[tmp][0] == 1){
  tmp_p := partialcastEn1toEn2(p5[tmp]);
  tmp_amp := ampMul(tmp_amp, pow2(|p5[tmp]|), p5[tmp]);
}
  q6 := (q6 + [tmp_q]);
  p6 := (p6 + [tmp_p]);
  amp6 := (amp6 + [tmp_amp]);
  omega0();
  tmp := (tmp + 1);
}
  powNTimesMod();
  pow2add();
  triggerSqrtMul();
  pow2mul();
  omega0();
  q7, p7, amp7 := q6, p6, amp6;
}