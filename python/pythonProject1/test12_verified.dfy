function {:axiom} powN(N:nat, k: nat) : int
                    ensures powN(N, k) > 0

lemma {:axiom} pow2add()
                      ensures forall k : nat :: pow2(k) * 2 == pow2(k + 1)


      function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
          requires |x| >= n
          requires |y| >= n
          ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]


function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

method {:axiom} ampMul(amp : seq<real>, x: nat, y : seq<bv1>) returns (amp1: seq<real>)
          requires x > 0
          ensures |amp| == |amp1|
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == (amp[k] * (1.0/sqrt(x as real))) * omega(castBVInt(y), 2)


function {:axiom} omega(n:nat, a:nat): real


        method {:axiom} norEn2(x: seq<bv1>, y: seq<seq<seq<bv1>>>)
        returns (x1: seq<seq<seq<bv1>>>)
          ensures |x1| == |y|
          ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |y[k]|
          ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| == |x1[k][j]| == |x|
          ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> castBVInt(x1[k][j]) == castBVInt(x)
          ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> samebit(x1[k][j], x, |x|)



    method {:axiom} duplicateAmp(x: real, n:nat) returns (x1:seq<real>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x


lemma {:axiom} powNTimesMod()
          ensures forall k: nat, j: nat, l : nat, N:nat :: N > 0 ==> powN(k, j) * (powN(k, l) % N) % N == powN(k, j + l) % N

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0


      method {:axiom} duplicateSeq(x: seq<bv1>, n:nat) returns (x1:seq<seq<bv1>>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
        ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x)
        ensures forall k :: 0 <= k < |x1| ==> samebit(x1[k], x, |x|)


lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

function {:axiom} bool2BV1(b : bool) : seq<bv1>
            ensures castBVInt(bool2BV1(b)) == if b then 1 else 0
            ensures |bool2BV1(b)| == 1

lemma {:axiom} triggerSqrtMul()
                      ensures forall k, j :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)

method {:axiom} partialcastEn1toEn2(x: seq<bv1>) returns (x1 : seq<seq<bv1>>)
          ensures |x1| == pow2(|x|)
          ensures forall k :: 0 <= k < pow2(|x|) ==> |x1[k]| == |x|
          ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k


lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

method {:axiom} qif_lambda8(p1: seq<bv1>, amp1: real) returns (p2: seq<bv1>, amp2: real)
  ensures |p2| == |p1|
  ensures castBVInt(p2) == (castBVInt(p1) + 1)
  ensures amp2 == (amp1 * omega(0, 1))
  ensures |p1| == |p2|

method conditionaltest5(n: nat, N: nat, amp: seq<seq<real>>, q: seq<seq<seq<bv1>>>, p: seq<seq<seq<bv1>>>, r1: seq<bv1>, u2: seq<bv1>) returns (amp8: seq<seq<seq<real>>>, q8: seq<seq<seq<seq<bv1>>>>, p8: seq<seq<seq<seq<bv1>>>>, u8: seq<seq<seq<seq<bv1>>>>, r8: seq<seq<seq<seq<bv1>>>>)
  requires n > 0
  requires |q| == pow2(n)
  requires forall tmp4 :: 0 <= tmp4 < pow2(n) ==> |q[tmp4]| == pow2(n)
  requires forall tmp4 :: 0 <= tmp4 < pow2(n) ==> forall tmp5 :: 0 <= tmp5 < pow2(n) ==> |q[tmp4][tmp5]| == n
  requires n > 0
  requires |p| == pow2(n)
  requires forall tmp4 :: 0 <= tmp4 < pow2(n) ==> |p[tmp4]| == pow2(n)
  requires forall tmp4 :: 0 <= tmp4 < pow2(n) ==> forall tmp5 :: 0 <= tmp5 < pow2(n) ==> |p[tmp4][tmp5]| == n
  requires |amp| == pow2(n)
  requires forall tmp4 :: 0 <= tmp4 < pow2(n) ==> |amp[tmp4]| == pow2(n)
  requires |r1| == 1
  requires |u2| == 1
  requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> amp[k][j] == (1.0 / (pow2(n) as real))
  requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(q[k][j]) == k
  requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(p[k][j]) == j
  requires castBVInt(r1) == 0
  requires forall tmp4 :: 0 <= tmp4 < 1 ==> r1[tmp4] == 0
  requires castBVInt(u2) == 0
  requires forall tmp5 :: 0 <= tmp5 < 1 ==> u2[tmp5] == 0
  ensures |q8| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |q8| ==> |q8[tmp9]| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |q8| ==> forall tmp10 :: 0 <= tmp10 < |q8[tmp9]| ==> |q8[tmp9][tmp10]| == pow2(1)
  ensures forall tmp9 :: 0 <= tmp9 < |q8| ==> forall tmp10 :: 0 <= tmp10 < |q8[tmp9]| ==> forall tmp11 :: 0 <= tmp11 < |q8[tmp9][tmp10]| ==> |q8[tmp9][tmp10][tmp11]| == n
  ensures |p8| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |p8| ==> |p8[tmp9]| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |p8| ==> forall tmp10 :: 0 <= tmp10 < |p8[tmp9]| ==> |p8[tmp9][tmp10]| == pow2(1)
  ensures forall tmp9 :: 0 <= tmp9 < |p8| ==> forall tmp10 :: 0 <= tmp10 < |p8[tmp9]| ==> forall tmp11 :: 0 <= tmp11 < |p8[tmp9][tmp10]| ==> |p8[tmp9][tmp10][tmp11]| == n
  ensures |u8| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |u8| ==> |u8[tmp9]| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |u8| ==> forall tmp10 :: 0 <= tmp10 < |u8[tmp9]| ==> |u8[tmp9][tmp10]| == pow2(1)
  ensures forall tmp9 :: 0 <= tmp9 < |u8| ==> forall tmp10 :: 0 <= tmp10 < |u8[tmp9]| ==> forall tmp11 :: 0 <= tmp11 < |u8[tmp9][tmp10]| ==> |u8[tmp9][tmp10][tmp11]| == 1
  ensures |r8| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |r8| ==> |r8[tmp9]| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |r8| ==> forall tmp10 :: 0 <= tmp10 < |r8[tmp9]| ==> |r8[tmp9][tmp10]| == pow2(1)
  ensures forall tmp9 :: 0 <= tmp9 < |r8| ==> forall tmp10 :: 0 <= tmp10 < |r8[tmp9]| ==> forall tmp11 :: 0 <= tmp11 < |r8[tmp9][tmp10]| ==> |r8[tmp9][tmp10][tmp11]| == 1
  ensures |amp8| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |amp8| ==> |amp8[tmp9]| == pow2(n)
  ensures forall tmp9 :: 0 <= tmp9 < |amp8| ==> forall tmp10 :: 0 <= tmp10 < |amp8[tmp9]| ==> |amp8[tmp9][tmp10]| == pow2(1)
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall t :: 0 <= t < pow2(1) ==> amp8[k][j][t] == if k < N then ((1.0 / (pow2(n) as real)) * (1.0 / sqrt((pow2(1) as real)))) else (1.0 / (pow2(n) as real))  
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall t :: 0 <= t < pow2(1) ==> castBVInt(q8[k][j][t]) == k
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall t :: 0 <= t < pow2(1) ==> castBVInt(p8[k][j][t]) == if k < N then if r8[k][j][t][0] == 1 then (j + 1) else j else j
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall t :: 0 <= t < pow2(1) ==> castBVInt(u8[k][j][t]) == if k < N then 1 else 0
  ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall t :: 0 <= t < pow2(1) ==> castBVInt(r8[k][j][t]) == if k < N then t else 0
{
  var q6 := q;
  var p6 := p;
  var amp6 := amp;
  var u6 := norEn2(u2, q);
  var r6 := norEn2(r1, q);
  var q7 := [];
  var p7 := [];
  var u7 := [];
  var r7 := [];
  var amp7 := [];
  var tmp := 0;
  while(tmp < |q6|)
  invariant 0 <= tmp <= |q6|
  invariant 0 <= tmp <= |p6|
  invariant 0 <= tmp <= |u6|
  invariant 0 <= tmp <= |r6|
  invariant 0 <= tmp <= |amp6|
  invariant |q7| == tmp
  invariant |p7| == tmp
  invariant |u7| == tmp
  invariant |r7| == tmp
  invariant |amp7| == tmp
  invariant forall tmp8 :: 0 <= tmp8 < |q7| ==> |q7[tmp8]| == pow2(n)
  invariant forall tmp8 :: 0 <= tmp8 < |p7| ==> |p7[tmp8]| == pow2(n)
  invariant forall tmp8 :: 0 <= tmp8 < |u7| ==> |u7[tmp8]| == pow2(n)
  invariant forall tmp8 :: 0 <= tmp8 < |r7| ==> |r7[tmp8]| == pow2(n)
  invariant forall tmp8 :: 0 <= tmp8 < |amp7| ==> |amp7[tmp8]| == pow2(n)
  invariant forall tmp8 :: 0 <= tmp8 < |q7| ==> forall tmp9 :: 0 <= tmp9 < |q7[tmp8]| ==> |q7[tmp8][tmp9]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |p7| ==> forall tmp9 :: 0 <= tmp9 < |p7[tmp8]| ==> |p7[tmp8][tmp9]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |u7| ==> forall tmp9 :: 0 <= tmp9 < |u7[tmp8]| ==> |u7[tmp8][tmp9]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |r7| ==> forall tmp9 :: 0 <= tmp9 < |r7[tmp8]| ==> |r7[tmp8][tmp9]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |amp7| ==> forall tmp9 :: 0 <= tmp9 < |amp7[tmp8]| ==> |amp7[tmp8][tmp9]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |q7| ==> forall tmp9 :: 0 <= tmp9 < |q7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |q7[tmp8][tmp9]| ==> |q7[tmp8][tmp9][tmp10]| == n
  invariant forall tmp8 :: 0 <= tmp8 < |p7| ==> forall tmp9 :: 0 <= tmp9 < |p7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |p7[tmp8][tmp9]| ==> |p7[tmp8][tmp9][tmp10]| == n
  invariant forall tmp8 :: 0 <= tmp8 < |u7| ==> forall tmp9 :: 0 <= tmp9 < |u7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |u7[tmp8][tmp9]| ==> |u7[tmp8][tmp9][tmp10]| == 1
  invariant forall tmp8 :: 0 <= tmp8 < |r7| ==> forall tmp9 :: 0 <= tmp9 < |r7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |r7[tmp8][tmp9]| ==> |r7[tmp8][tmp9][tmp10]| == 1
  invariant forall tmp8 :: 0 <= tmp8 < |q7| ==> forall tmp9 :: 0 <= tmp9 < |q7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |q7[tmp8][tmp9]| ==> castBVInt(q7[tmp8][tmp9][tmp10]) == if castBVInt(q6[tmp8][tmp9]) < N then castBVInt(q6[tmp8][tmp9]) else castBVInt(q6[tmp8][tmp9])
  invariant forall tmp8 :: 0 <= tmp8 < |p7| ==> forall tmp9 :: 0 <= tmp9 < |p7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |p7[tmp8][tmp9]| ==> castBVInt(p7[tmp8][tmp9][tmp10]) == if castBVInt(q6[tmp8][tmp9]) < N then if r7[tmp8][tmp9][tmp10][0] == 1 then (castBVInt(p6[tmp8][tmp9]) + 1) else castBVInt(p6[tmp8][tmp9]) else castBVInt(p6[tmp8][tmp9])
  invariant forall tmp8 :: 0 <= tmp8 < |u7| ==> forall tmp9 :: 0 <= tmp9 < |u7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |u7[tmp8][tmp9]| ==> castBVInt(u7[tmp8][tmp9][tmp10]) == if castBVInt(q6[tmp8][tmp9]) < N then 1 else castBVInt(u6[tmp8][tmp9])
  invariant forall tmp8 :: 0 <= tmp8 < |r7| ==> forall tmp9 :: 0 <= tmp9 < |r7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |r7[tmp8][tmp9]| ==> castBVInt(r7[tmp8][tmp9][tmp10]) == if castBVInt(q6[tmp8][tmp9]) < N then tmp10 else castBVInt(r6[tmp8][tmp9])
  invariant forall tmp8 :: 0 <= tmp8 < |amp7| ==> forall tmp9 :: 0 <= tmp9 < |amp7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |amp7[tmp8][tmp9]| ==> amp7[tmp8][tmp9][tmp10] == if castBVInt(q6[tmp8][tmp9]) < N then (amp6[tmp8][tmp9] * ((1.0 / sqrt((pow2(|r7[tmp8][tmp9][tmp10]|) as real))) * omega(castBVInt(r6[tmp8][tmp9]), 2))) else amp6[tmp8][tmp9]
  invariant forall tmp8 :: 0 <= tmp8 < |q7| ==> forall tmp9 :: 0 <= tmp9 < |q7[tmp8]| ==> forall tmp10 :: 0 <= tmp10 < |q7[tmp8][tmp9]| ==> samebit(q7[tmp8][tmp9][tmp10], q6[tmp8][tmp9], |q6[tmp8][tmp9]|)
{
  var tmp1q := [];
  var tmp1p := [];
  var tmp1u := [];
  var tmp1r := [];
  var tmp1amp := [];
  var tmp1 := 0;
  while(tmp1 < |q6[tmp]|)
  invariant 0 <= tmp1 <= |q6[tmp]|
  invariant 0 <= tmp1 <= |p6[tmp]|
  invariant 0 <= tmp1 <= |u6[tmp]|
  invariant 0 <= tmp1 <= |r6[tmp]|
  invariant 0 <= tmp1 <= |amp6[tmp]|
  invariant |tmp1q| == tmp1
  invariant |tmp1p| == tmp1
  invariant |tmp1u| == tmp1
  invariant |tmp1r| == tmp1
  invariant |tmp1amp| == tmp1
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1q| ==> |tmp1q[tmp8]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1p| ==> |tmp1p[tmp8]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1u| ==> |tmp1u[tmp8]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1r| ==> |tmp1r[tmp8]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1amp| ==> |tmp1amp[tmp8]| == pow2(1)
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1q| ==> forall tmp9 :: 0 <= tmp9 < |tmp1q[tmp8]| ==> |tmp1q[tmp8][tmp9]| == n
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1p| ==> forall tmp9 :: 0 <= tmp9 < |tmp1p[tmp8]| ==> |tmp1p[tmp8][tmp9]| == n
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1u| ==> forall tmp9 :: 0 <= tmp9 < |tmp1u[tmp8]| ==> |tmp1u[tmp8][tmp9]| == 1
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1r| ==> forall tmp9 :: 0 <= tmp9 < |tmp1r[tmp8]| ==> |tmp1r[tmp8][tmp9]| == 1
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1q| ==> forall tmp9 :: 0 <= tmp9 < |tmp1q[tmp8]| ==> castBVInt(tmp1q[tmp8][tmp9]) == if castBVInt(q6[tmp][tmp8]) < N then castBVInt(q6[tmp][tmp8]) else castBVInt(q6[tmp][tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1p| ==> forall tmp9 :: 0 <= tmp9 < |tmp1p[tmp8]| ==> castBVInt(tmp1p[tmp8][tmp9]) == if castBVInt(q6[tmp][tmp8]) < N then if tmp1r[tmp8][tmp9][0] == 1 then (castBVInt(p6[tmp][tmp8]) + 1) else castBVInt(p6[tmp][tmp8]) else castBVInt(p6[tmp][tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1u| ==> forall tmp9 :: 0 <= tmp9 < |tmp1u[tmp8]| ==> castBVInt(tmp1u[tmp8][tmp9]) == if castBVInt(q6[tmp][tmp8]) < N then 1 else castBVInt(u6[tmp][tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1r| ==> forall tmp9 :: 0 <= tmp9 < |tmp1r[tmp8]| ==> castBVInt(tmp1r[tmp8][tmp9]) == if castBVInt(q6[tmp][tmp8]) < N then tmp9 else castBVInt(r6[tmp][tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1amp| ==> forall tmp9 :: 0 <= tmp9 < |tmp1amp[tmp8]| ==> tmp1amp[tmp8][tmp9] == if castBVInt(q6[tmp][tmp8]) < N then (amp6[tmp][tmp8] * ((1.0 / sqrt((pow2(|tmp1r[tmp8][tmp9]|) as real))) * omega(castBVInt(r6[tmp][tmp8]), 2))) else amp6[tmp][tmp8]
  invariant forall tmp8 :: 0 <= tmp8 < |tmp1q| ==> forall tmp9 :: 0 <= tmp9 < |tmp1q[tmp8]| ==> samebit(tmp1q[tmp8][tmp9], q6[tmp][tmp8], |q6[tmp][tmp8]|)
{
  var res := bool2BV1(castBVInt(q6[tmp][tmp1]) < N);
  var tmp_u := duplicateSeq(res, pow2(|r6[tmp][tmp1]|));
  var tmp_q := duplicateSeq(q6[tmp][tmp1], pow2(|r6[tmp][tmp1]|));
  var tmp_p := duplicateSeq(p6[tmp][tmp1], pow2(|r6[tmp][tmp1]|));
  var tmp_r := duplicateSeq(r6[tmp][tmp1], pow2(|r6[tmp][tmp1]|));
  var tmp_amp := duplicateAmp(amp6[tmp][tmp1], pow2(|r6[tmp][tmp1]|));
  if (castBVInt(q6[tmp][tmp1]) < N){
  tmp_r := partialcastEn1toEn2(r6[tmp][tmp1]);
  tmp_amp := ampMul(tmp_amp, pow2(|r6[tmp][tmp1]|), r6[tmp][tmp1]);
  var tmp_2q2 := [];
  var tmp_2p2 := [];
  var tmp_2u2 := [];
  var tmp_2r2 := [];
  var tmp_2amp2 := [];
  var tmp_sub1 := 0;
  while(tmp_sub1 < |tmp_r|)
  invariant 0 <= tmp_sub1 <= |tmp_q|
  invariant 0 <= tmp_sub1 <= |tmp_p|
  invariant 0 <= tmp_sub1 <= |tmp_u|
  invariant 0 <= tmp_sub1 <= |tmp_r|
  invariant 0 <= tmp_sub1 <= |tmp_amp|
  invariant |tmp_2q2| == tmp_sub1
  invariant |tmp_2p2| == tmp_sub1
  invariant |tmp_2u2| == tmp_sub1
  invariant |tmp_2r2| == tmp_sub1
  invariant |tmp_2amp2| == tmp_sub1
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2q2| ==> |tmp_2q2[tmp8]| == n
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2p2| ==> |tmp_2p2[tmp8]| == n
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2u2| ==> |tmp_2u2[tmp8]| == 1
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2r2| ==> |tmp_2r2[tmp8]| == 1
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2q2| ==> castBVInt(tmp_2q2[tmp8]) == if tmp_r[tmp8][0] == 1 then castBVInt(tmp_q[tmp8]) else castBVInt(tmp_q[tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2p2| ==> castBVInt(tmp_2p2[tmp8]) == if tmp_r[tmp8][0] == 1 then (castBVInt(tmp_p[tmp8]) + 1) else castBVInt(tmp_p[tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2u2| ==> castBVInt(tmp_2u2[tmp8]) == if tmp_r[tmp8][0] == 1 then castBVInt(tmp_u[tmp8]) else castBVInt(tmp_u[tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2r2| ==> castBVInt(tmp_2r2[tmp8]) == if tmp_r[tmp8][0] == 1 then castBVInt(tmp_r[tmp8]) else castBVInt(tmp_r[tmp8])
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2amp2| ==> tmp_2amp2[tmp8] == if tmp_r[tmp8][0] == 1 then (tmp_amp[tmp8] * omega(0, 1)) else tmp_amp[tmp8]
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2q2| ==> samebit(tmp_2q2[tmp8], tmp_q[tmp8], |tmp_q[tmp8]|)
  invariant forall tmp8 :: 0 <= tmp8 < |tmp_2r2| ==> samebit(tmp_2r2[tmp8], tmp_r[tmp8], |tmp_r[tmp8]|)
{
  var tmp_q1:seq<bv1>;
  var tmp_p1:seq<bv1>;
  var tmp_u1:seq<bv1>;
  var tmp_r1:seq<bv1>;
  var tmp_amp1:real;
  if (tmp_r[tmp_sub1][0] == 1){
  tmp_p1, tmp_amp1 := qif_lambda8(tmp_p[tmp_sub1], tmp_amp[tmp_sub1]);
  tmp_q1 := tmp_q[tmp_sub1];
  tmp_u1 := tmp_u[tmp_sub1];
  tmp_r1 := tmp_r[tmp_sub1];
}
else {
  tmp_q1 := tmp_q[tmp_sub1];
  tmp_p1 := tmp_p[tmp_sub1];
  tmp_u1 := tmp_u[tmp_sub1];
  tmp_r1 := tmp_r[tmp_sub1];
  tmp_amp1 := tmp_amp[tmp_sub1];
}
  omega0();
  tmp_2q2 := (tmp_2q2 + [tmp_q1]);
  tmp_2p2 := (tmp_2p2 + [tmp_p1]);
  tmp_2u2 := (tmp_2u2 + [tmp_u1]);
  tmp_2r2 := (tmp_2r2 + [tmp_r1]);
  tmp_2amp2 := (tmp_2amp2 + [tmp_amp1]);
  omega0();
  tmp_sub1 := (tmp_sub1 + 1);
}
  tmp_q := tmp_2q2;
  tmp_p := tmp_2p2;
  tmp_u := tmp_2u2;
  tmp_r := tmp_2r2;
  tmp_amp := tmp_2amp2;
}
  tmp1q := (tmp1q + [tmp_q]);
  tmp1p := (tmp1p + [tmp_p]);
  tmp1u := (tmp1u + [tmp_u]);
  tmp1r := (tmp1r + [tmp_r]);
  tmp1amp := (tmp1amp + [tmp_amp]);
  omega0();
  tmp1 := (tmp1 + 1);
}
  q7 := (q7 + [tmp1q]);
  p7 := (p7 + [tmp1p]);
  u7 := (u7 + [tmp1u]);
  r7 := (r7 + [tmp1r]);
  amp7 := (amp7 + [tmp1amp]);
  tmp := (tmp + 1);
}
  powNTimesMod();
  pow2add();
  triggerSqrtMul();
  pow2mul();
  omega0();
  q8, p8, u8, r8, amp8 := q7, p7, u7, r7, amp7;
}
