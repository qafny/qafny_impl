lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

function {:axiom} omega(n:nat, a:nat): real

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

method {:axiom} newFun2(q: seq<seq<bv1>>, p: seq<seq<bv1>>, amp: seq<real>, a: nat, N: nat) returns (q3: seq<seq<bv1>>, p3: seq<seq<bv1>>, amp3: seq<real>)
  requires N > 0
  ensures |amp3| == |amp| == |p3| == |p| == |q3| == |q|
  ensures forall tmp4 :: 0 <= tmp4 < |amp3| ==> |p[tmp4]| == |p3[tmp4]| == |q[tmp4]| == |q3[tmp4]|
  ensures forall tmp4 :: 0 <= tmp4 < |p3| ==> castBVInt(p3[tmp4]) == (a * (castBVInt(p[tmp4]) % N))
  ensures forall tmp4 :: 0 <= tmp4 < |q3| ==> castBVInt(q3[tmp4]) == castBVInt(q[tmp4])
  ensures forall tmp4 :: 0 <= tmp4 < |amp3| ==> amp3[tmp4] == (amp[tmp4] * omega(0, 1))

method lambdatest(n: nat, a: nat, N: nat, amp: seq<real>, q: seq<seq<bv1>>, p: seq<seq<bv1>>) returns (amp4: seq<real>, q4: seq<seq<bv1>>, p4: seq<seq<bv1>>)
  requires n > 0
  requires |q| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |q[tmp2]| == n
  requires n > 0
  requires |p| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |p[tmp2]| == n
  requires |amp| == pow2(n)
  requires N > 0
  requires forall k :: 0 <= k < pow2(n) ==> amp[k] == (1.0 / sqrt((pow2(n) as real)))
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(q[k]) == k
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(p[k]) == k
  ensures |q4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |q4| ==> |q4[tmp5]| == n
  ensures |p4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |p4| ==> |p4[tmp5]| == n
  ensures |amp4| == pow2(n)
  ensures forall k :: 0 <= k < pow2(n) ==> amp4[k] == (1.0 / sqrt((pow2(n) as real)))
  ensures forall k :: 0 <= k < pow2(n) ==> castBVInt(q4[k]) == k
  ensures forall k :: 0 <= k < pow2(n) ==> castBVInt(p4[k]) == (a * (k % N))
{
  var q3:seq<seq<bv1>>;
  var p3:seq<seq<bv1>>;
  var amp3:seq<real>;
  q3, p3, amp3 := newFun2(q, p, amp, a, N);
  omega0();
  q4, p4, amp4 := q3, p3, amp3;
}