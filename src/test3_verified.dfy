lemma {:axiom} pow2sqrt()
        ensures forall k :nat  :: sqrt(pow2(2 * k) as real) == pow2(k) as real

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)

function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} omega(n:nat, a:nat): real

method {:axiom} En1toEn2_2(x: seq<seq<bv1>>, y: seq<seq<bv1>>, amp: seq<real>)
        returns (x1: seq<seq<seq<bv1>>>, y1: seq<seq<seq<bv1>>>, amp1: seq<seq<real>>)
        requires |x| == |y| == |amp|
        ensures |x1| == |y1| == |amp1| == |x|
        ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |y1[k]| == |amp1[k]| == pow2(|y[k]|)
        ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==>|x1[k][j]| == |x[k]|
        ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |y1[k]| ==>|y1[k][j]| == |y[k]|
        ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> castBVInt(x1[k][j]) == castBVInt(x[k])
        ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |y1[k]| ==> castBVInt(y1[k][j]) == j
        ensures forall k :: 0 <= k < |amp1| ==> forall j :: 0 <= j < |amp1[k]| ==> amp1[k][j] == amp[k] * (1.0/sqrt(pow2(|y[k]|) as real)) * omega(j * k, 2)


lemma {:axiom} ampeqtrigger()
          ensures forall k : nat :: (1.0/sqrt(pow2(k) as real)) * (1.0/sqrt(pow2(k) as real)) == 1.0/pow2(k) as real

lemma {:axiom} triggerSqrtMul()
                      ensures forall k, j :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)

method hadtest(n: nat, amp: seq<real>, q: seq<seq<bv1>>, p: seq<seq<bv1>>) returns (amp3: seq<seq<real>>, q3: seq<seq<seq<bv1>>>, p3: seq<seq<seq<bv1>>>)
  requires n > 0
  requires |q| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |q[tmp2]| == n
  requires n > 0
  requires |p| == pow2(n)
  requires forall tmp2 :: 0 <= tmp2 < pow2(n) ==> |p[tmp2]| == n
  requires |amp| == pow2(n)
  requires forall k :: 0 <= k < pow2(n) ==> amp[k] == (1.0 / sqrt((pow2(n) as real)))
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(q[k]) == k
  requires forall k :: 0 <= k < pow2(n) ==> castBVInt(p[k]) == k
  ensures |q3| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < |q3| ==> |q3[tmp4]| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < |q3| ==> forall tmp5 :: 0 <= tmp5 < |q3[tmp4]| ==> |q3[tmp4][tmp5]| == n
  ensures |p3| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < |p3| ==> |p3[tmp4]| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < |p3| ==> forall tmp5 :: 0 <= tmp5 < |p3[tmp4]| ==> |p3[tmp4][tmp5]| == n
  ensures |amp3| == pow2(n)
  ensures forall tmp4 :: 0 <= tmp4 < |amp3| ==> |amp3[tmp4]| == pow2(n)
  ensures forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> amp3[j][k] == ((1.0 / (pow2(n) as real)) * omega((j * k), abs(2)))
  ensures forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> castBVInt(q3[j][k]) == j
  ensures forall j :: 0 <= j < pow2(n) ==> forall k :: 0 <= k < pow2(n) ==> castBVInt(p3[j][k]) == k
{
  var q2, p2, amp2 := En1toEn2_2(q, p, amp);
  triggerSqrtMul();
  ampeqtrigger();
  pow2mul();
  pow2sqrt();
  q3, p3, amp3 := q2, p2, amp2;
}