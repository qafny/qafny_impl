lemma {:axiom} pow2add()
                      ensures forall k : nat :: pow2(k) * 2 == pow2(k + 1)


      method {:axiom} duplicateSeq(x: seq<bv1>, n:nat) returns (x1:seq<seq<bv1>>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
        ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x)
        ensures forall k :: 0 <= k < |x1| ==> samebit(x1[k], x, |x|)


lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

lemma {:axiom} triggerSqrtMul()
                      ensures forall k, j :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)


      function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
          requires |x| >= n
          requires |y| >= n
          ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]


method {:axiom} ampMul(amp : seq<real>, x: nat, y : seq<bv1>) returns (amp1: seq<real>)
          requires x > 0
          ensures |amp| == |amp1|
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == (amp[k] * (1.0/sqrt(x as real))) * omega(castBVInt(y), 2)


function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

method {:axiom} partialcastEn1toEn2(x: seq<bv1>) returns (x1 : seq<seq<bv1>>)
          ensures |x1| == pow2(|x|)
          ensures forall k :: 0 <= k < pow2(|x|) ==> |x1[k]| == |x|
          ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k



    method {:axiom} duplicateAmp(x: real, n:nat) returns (x1:seq<real>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x


function {:axiom} omega(n:nat, a:nat): real

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} powN(N:nat, k: nat) : int
                    ensures powN(N, k) > 0

lemma {:axiom} powNTimesMod()
          ensures forall k: nat, j: nat, l : nat, N:nat :: N > 0 ==> powN(k, j) * (powN(k, l) % N) % N == powN(k, j + l) % N

method conditionaltest3(n: nat, amp: seq<real>, q: seq<seq<bv1>>, p: seq<seq<bv1>>) returns (amp4: seq<seq<real>>, q4: seq<seq<seq<bv1>>>, p4: seq<seq<seq<bv1>>>)
  requires n > 0
  requires |q| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |q[tmp2]| == n
  requires |p| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |p[tmp2]| == 1
  requires |amp| == pow2(n)
  requires forall k :: 0 <= k < pow2(n) ==> amp[k] == (1.0 / sqrt((pow2(n) as real)))
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(q[k]) == k
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(p[k]) == (k % 2)
  ensures |q4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |q4| ==> |q4[tmp5]| == pow2(1)
  ensures forall tmp5 :: 0 <= tmp5 < |q4| ==> forall tmp6 :: 0 <= tmp6 < |q4[tmp5]| ==> |q4[tmp5][tmp6]| == n
  ensures |p4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |p4| ==> |p4[tmp5]| == pow2(1)
  ensures forall tmp5 :: 0 <= tmp5 < |p4| ==> forall tmp6 :: 0 <= tmp6 < |p4[tmp5]| ==> |p4[tmp5][tmp6]| == 1
  ensures |amp4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |amp4| ==> |amp4[tmp5]| == pow2(1)
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(1) ==> amp4[k][j] == if q4[k][j][0] == 1 then ((1.0 / sqrt((pow2((n + 1)) as real))) * omega((k % 2), abs(2))) else (1.0 / sqrt((pow2(n) as real)))
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(q4[k][j]) == k
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(p4[k][j]) == if q4[k][j][0] == 1 then j else (k % 2)
{
  var q2 := q;
  var p2 := p;
  var amp2 := amp;
  var q3 := [];
  var p3 := [];
  var amp3 := [];
  var tmp := 0;
  while(tmp < |q2|)
  invariant 0 <= tmp <= |q2|
  invariant 0 <= tmp <= |p2|
  invariant 0 <= tmp <= |amp2|
  invariant |q3| == tmp
  invariant |p3| == tmp
  invariant |amp3| == tmp
  invariant forall tmp4 :: 0 <= tmp4 < |q3| ==> |q3[tmp4]| == pow2(1)
  invariant forall tmp4 :: 0 <= tmp4 < |p3| ==> |p3[tmp4]| == pow2(1)
  invariant forall tmp4 :: 0 <= tmp4 < |amp3| ==> |amp3[tmp4]| == pow2(1)
  invariant forall tmp4 :: 0 <= tmp4 < |q3| ==> forall tmp5 :: 0 <= tmp5 < |q3[tmp4]| ==> |q3[tmp4][tmp5]| == n
  invariant forall tmp4 :: 0 <= tmp4 < |p3| ==> forall tmp5 :: 0 <= tmp5 < |p3[tmp4]| ==> |p3[tmp4][tmp5]| == 1
  invariant forall tmp4 :: 0 <= tmp4 < |q3| ==> forall tmp5 :: 0 <= tmp5 < |q3[tmp4]| ==> castBVInt(q3[tmp4][tmp5]) == if q2[tmp4][0] == 1 then castBVInt(q2[tmp4]) else castBVInt(q2[tmp4])
  invariant forall tmp4 :: 0 <= tmp4 < |p3| ==> forall tmp5 :: 0 <= tmp5 < |p3[tmp4]| ==> castBVInt(p3[tmp4][tmp5]) == if q2[tmp4][0] == 1 then tmp5 else castBVInt(p2[tmp4])
  invariant forall tmp4 :: 0 <= tmp4 < |amp3| ==> forall tmp5 :: 0 <= tmp5 < |amp3[tmp4]| ==> amp3[tmp4][tmp5] == if q2[tmp4][0] == 1 then (amp2[tmp4] * ((1.0 / sqrt((pow2(|p3[tmp4][tmp5]|) as real))) * omega(castBVInt(p2[tmp4]), 2))) else amp2[tmp4]
  invariant forall tmp4 :: 0 <= tmp4 < |q3| ==> forall tmp5 :: 0 <= tmp5 < |q3[tmp4]| ==> samebit(q3[tmp4][tmp5], q2[tmp4], |q2[tmp4]|)
{
  var tmp_q := duplicateSeq(q2[tmp], pow2(|p2[tmp]|));
  var tmp_p := duplicateSeq(p2[tmp], pow2(|p2[tmp]|));
  var tmp_amp := duplicateAmp(amp2[tmp], pow2(|p2[tmp]|));
  if (q2[tmp][0] == 1){
  tmp_p := partialcastEn1toEn2(p2[tmp]);
  tmp_amp := ampMul(tmp_amp, pow2(|p2[tmp]|), p2[tmp]);
}
  q3 := (q3 + [tmp_q]);
  p3 := (p3 + [tmp_p]);
  amp3 := (amp3 + [tmp_amp]);
  omega0();
  tmp := (tmp + 1);
}
  powNTimesMod();
  pow2add();
  triggerSqrtMul();
  pow2mul();
  omega0();
  q4, p4, amp4 := q3, p3, amp3;
}
