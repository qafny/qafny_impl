function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

function {:axiom} omega(n:nat, a:nat): real

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

method {:axiom} newFun2(q: seq<seq<seq<bv1>>>, p: seq<seq<seq<bv1>>>, amp: seq<seq<real>>, n: nat) returns (q3: seq<seq<seq<bv1>>>, p3: seq<seq<seq<bv1>>>, amp3: seq<seq<real>>)
  ensures |amp3| == |amp| == |p3| == |p| == |q3| == |q|
  ensures forall tmp4 :: 0 <= tmp4 < |amp3| ==> |amp[tmp4]| == |amp3[tmp4]| == |p[tmp4]| == |p3[tmp4]| == |q[tmp4]| == |q3[tmp4]|
  ensures forall tmp4 :: 0 <= tmp4 < |amp3| ==> forall tmp5 :: 0 <= tmp5 < |amp3[tmp4]| ==> |p[tmp4][tmp5]| == |p3[tmp4][tmp5]| == |q[tmp4][tmp5]| == |q3[tmp4][tmp5]|
  ensures forall tmp4 :: 0 <= tmp4 < |p3| ==> forall tmp5 :: 0 <= tmp5 < |p3[tmp4]| ==> castBVInt(p3[tmp4][tmp5]) == castBVInt(p[tmp4][tmp5])
  ensures forall tmp4 :: 0 <= tmp4 < |q3| ==> forall tmp5 :: 0 <= tmp5 < |q3[tmp4]| ==> castBVInt(q3[tmp4][tmp5]) == castBVInt(q[tmp4][tmp5])
  ensures forall tmp4 :: 0 <= tmp4 < |amp3| ==> forall tmp5 :: 0 <= tmp5 < |amp3[tmp4]| ==> amp3[tmp4][tmp5] == (amp[tmp4][tmp5] * omega(castBVInt(p[tmp4][tmp5]), abs(pow2(n))))

method lambdatest(n: nat, k: nat, amp: seq<seq<real>>, q: seq<seq<seq<bv1>>>, p: seq<seq<seq<bv1>>>) returns (amp4: seq<seq<real>>, q4: seq<seq<seq<bv1>>>, p4: seq<seq<seq<bv1>>>)
  requires n > 0
  requires |q| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |q[tmp2]| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> forall tmp3 :: 0 <= tmp3 < pow2(n) ==> |q[tmp2][tmp3]| == n
  requires n > 0
  requires |p| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |p[tmp2]| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> forall tmp3 :: 0 <= tmp3 < pow2(n) ==> |p[tmp2][tmp3]| == n
  requires |amp| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |amp[tmp2]| == pow2(n)
  requires forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> amp[j][k] == (1.0 / (pow2(n) as real))
  requires forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> castBVInt(q[j][k]) == j
  requires forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> castBVInt(p[j][k]) == k
  ensures |q4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |q4| ==> |q4[tmp5]| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |q4| ==> forall tmp6 :: 0 <= tmp6 < |q4[tmp5]| ==> |q4[tmp5][tmp6]| == n
  ensures |p4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |p4| ==> |p4[tmp5]| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |p4| ==> forall tmp6 :: 0 <= tmp6 < |p4[tmp5]| ==> |p4[tmp5][tmp6]| == n
  ensures |amp4| == pow2(n)
  ensures forall tmp5 :: 0 <= tmp5 < |amp4| ==> |amp4[tmp5]| == pow2(n)
  ensures forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> amp4[j][k] == ((1.0 / (pow2(n) as real)) * omega(k, abs(pow2(n))))
  ensures forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> castBVInt(q4[j][k]) == j
  ensures forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> castBVInt(p4[j][k]) == k
{
  var q3:seq<seq<seq<bv1>>>;
  var p3:seq<seq<seq<bv1>>>;
  var amp3:seq<seq<real>>;
  q3, p3, amp3 := newFun2(q, p, amp, n);
  omega0();
  q4, p4, amp4 := q3, p3, amp3;
}