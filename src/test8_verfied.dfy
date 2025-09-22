method {:axiom} mergeAmpEn(amp: seq<real>, q : real) returns (amp1: seq<real>)
          ensures |amp1| == |amp| * 2
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == 1.0 / sqrt(pow2(1) as real) * amp[k]
          ensures forall k :: |amp| <= k < |amp1| ==> amp1[k] == 1.0 / sqrt(pow2(1) as real) * amp[k-|amp|] * q

lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

lemma {:axiom} pow2add()
                      ensures forall k : nat :: pow2(k) * 2 == pow2(k + 1)


      predicate {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat)
          requires |x| >= n
          requires |y| >= n
          ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]


method {:axiom} mergeBitEn(x: seq<seq<bv1>>, n : nat) returns (x1: seq<seq<bv1>>)
          requires forall k :: 0 <= k < |x| ==> |x[k]| == n
          ensures |x1| == |x| * 2
          ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
          ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k], n)
          ensures forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k], n)
          ensures forall k :: 0 <= k < |x| ==> x1[k][n] == 0
          ensures forall k :: |x| <= k < |x1| ==> x1[k][n] == 1

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

function {:axiom} omega(n:nat, a:nat): real

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} powN(N:nat, k: nat) : int
                    ensures powN(N, k) > 0

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)

function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

lemma {:axiom} powNTimesMod()
          ensures forall k: nat, j: nat, l : nat, N:nat {:trigger powN(k, j) * (powN(k, l) % N)}:: N > 0 ==> powN(k, j) * (powN(k, l) % N) % N == powN(k, j + l) % N

lemma {:axiom} triggerSqrtMul()
                      ensures forall k, j :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)

method {:axiom} duplicateMergeBitEn(x: seq<seq<bv1>>) returns (x1: seq<seq<bv1>>)
          ensures |x1| == |x| * 2
          ensures forall k :: 0 <= k < |x| ==> |x1[k]| == |x[k]|
          ensures forall k :: |x| <= k < |x1| ==> |x1[k]| == |x[k - |x|]|
          ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k], |x[k]|)
          ensures forall k :: 0 <= k < |x| ==> castBVInt(x1[k]) == castBVInt(x[k])
          ensures forall k :: |x| <= k < |x1| ==> samebit(x[k - |x|], x1[k], |x[k - |x|]|)
          ensures forall k :: |x| <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x[k - |x|])

method {:axiom} cutHad(x: seq<real>) returns (x1: seq<real>)
          requires 0 < |x|
          ensures |x1| == |x| - 1
          ensures forall k :: 0 <= k < |x1| ==> x1[k] == x[k+1]

lemma {:axiom} mergeBitTrigger(x: seq<seq<bv1>>, x1: seq<seq<bv1>>, n:nat)
          requires forall k :: 0 <= k < |x| ==> |x[k]| == n
          //requires forall k :: 0 <= k < |x| ==> castBVInt(x[k]) == k
          requires |x1| == |x| * 2
          requires forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
          requires forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k], n)
          requires forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k], n)
          requires forall k :: 0 <= k < |x| ==> x1[k][n] == 0
          requires forall k :: |x| <= k < |x1| ==> x1[k][n] == 1
          ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k

method {:axiom} qif_lambda10(p1: seq<bv1>, amp1: real, base: nat, i: nat, N: nat) returns (p2: seq<bv1>, amp2: real)
  requires N > 0
  ensures |p2| == |p1|
  ensures castBVInt(p2) == ((powN(base, pow2(i)) * castBVInt(p1)) % N)
  ensures amp2 == (amp1 * omega(0, 1))
  ensures |p1| == |p2|

method conditionaltest1(n: nat, i: nat, base: nat, N: nat, amp: seq<real>, q: seq<seq<bv1>>, p: seq<seq<bv1>>, q1: seq<real>) returns (q10: seq<real>, amp11: seq<real>, q11: seq<seq<bv1>>, p11: seq<seq<bv1>>)
  requires i > 0
  requires |q| == pow2(i)
  requires forall tmp4 :: 0 <= tmp4 < pow2(i) ==> |q[tmp4]| == i
  requires n > 0
  requires |p| == pow2(i)
  requires forall tmp4 :: 0 <= tmp4 < pow2(i) ==> |p[tmp4]| == n
  requires |amp| == pow2(i)
  requires n > 0
  requires |q1| == (n - i)
  requires N > 0
  requires 0 <= i < n
  requires forall k :: 0 <= k < pow2(i) ==> amp[k] == (1.0 / sqrt((pow2(i) as real)))
  requires forall k :: 0 <= k < pow2(i) ==> castBVInt(q[k]) == k
  requires forall k :: 0 <= k < pow2(i) ==> castBVInt(p[k]) == (powN(base, k) % N)
  requires forall tmp4 :: 0 <= tmp4 < (n - i) ==> q1[tmp4] == omega(0, 2)
  requires forall tmp4 :: 0 <= tmp4 < (n - i) ==> q1[tmp4] == omega(0, 2)
  requires i <= n
  requires 0 <= i
  ensures 0 <= (i + 1)
  ensures (i + 1) <= n
  ensures |q10| == (n - (i + 1))
  ensures |q11| == pow2((i + 1))
  ensures forall tmp12 :: 0 <= tmp12 < |q11| ==> |q11[tmp12]| == (i + 1)
  ensures |p11| == pow2((i + 1))
  ensures forall tmp12 :: 0 <= tmp12 < |p11| ==> |p11[tmp12]| == n
  ensures |amp11| == pow2((i + 1))
  ensures forall tmp12 :: 0 <= tmp12 < (n - (i + 1)) ==> q10[tmp12] == omega(0, 2)
  ensures forall k :: 0 <= k < pow2((i + 1)) ==> amp11[k] == (1.0 / sqrt((pow2((i + 1)) as real)))
  ensures forall k :: 0 <= k < pow2((i + 1)) ==> castBVInt(q11[k]) == k
  ensures forall k :: 0 <= k < pow2((i + 1)) ==> castBVInt(p11[k]) == (powN(base, k) % N)
{
  var q5 := cutHad(q1);
  var q7 := q1[0];
  var q8 := mergeBitEn(q, i);
  var p8 := duplicateMergeBitEn(p);
  var amp8 := mergeAmpEn(amp, q7);
  omega0();
  mergeBitTrigger(q, q8, |q[0]|);
  triggerSqrtMul();
  var p9 := [];
  var q9 := [];
  var amp9 := [];
  var tmp := 0;
  while(tmp < |q8|)
  invariant 0 <= tmp <= |p8|
  invariant 0 <= tmp <= |q8|
  invariant 0 <= tmp <= |amp8|
  invariant |p9| == tmp
  invariant |q9| == tmp
  invariant |amp9| == tmp
  invariant forall tmp10 :: 0 <= tmp10 < |p9| ==> |p9[tmp10]| == n
  invariant forall tmp10 :: 0 <= tmp10 < |q9| ==> |q9[tmp10]| == (i + 1)
  invariant forall tmp10 :: 0 <= tmp10 < |p9| ==> castBVInt(p9[tmp10]) == if q8[tmp10][i] == 1 then ((powN(base, pow2(i)) * castBVInt(p8[tmp10])) % N) else castBVInt(p8[tmp10])
  invariant forall tmp10 :: 0 <= tmp10 < |q9| ==> castBVInt(q9[tmp10]) == if q8[tmp10][i] == 1 then castBVInt(q8[tmp10]) else castBVInt(q8[tmp10])
  invariant forall tmp10 :: 0 <= tmp10 < |amp9| ==> amp9[tmp10] == if q8[tmp10][i] == 1 then (amp8[tmp10] * omega(0, 1)) else amp8[tmp10]
  invariant forall tmp10 :: 0 <= tmp10 < |q9| ==> samebit(q9[tmp10], q8[tmp10], |q8[tmp10]|)
{
  var tmp_p:seq<bv1>;
  var tmp_q:seq<bv1>;
  var tmp_amp:real;
  if (q8[tmp][i] == 1){
  tmp_p, tmp_amp := qif_lambda10(p8[tmp], amp8[tmp], base, i, N);
  tmp_q := q8[tmp];
}
else {
  tmp_p := p8[tmp];
  tmp_q := q8[tmp];
  tmp_amp := amp8[tmp];
}

  p9 := (p9 + [tmp_p]);
  q9 := (q9 + [tmp_q]);
  amp9 := (amp9 + [tmp_amp]);

  tmp := (tmp + 1);
}
  powNTimesMod();
  pow2add();
  triggerSqrtMul();
  pow2mul();
  omega0();
  // assert |p9| == pow2(i + 1);
  // assert forall k :: 0 <= k < |p9| ==> |p9[k]| == n;
  // assert forall k :: 0 <= k < |p9| ==> castBVInt(p9[k]) == (powN(base, k) % N);
  q10 := q5;
  q11, p11, amp11 := q9, p9, amp9;
}