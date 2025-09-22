function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

lemma {:axiom} SqrtGt(a:real)
                requires a > 0.0
                ensures sqrt(a) > 0.0

method {:axiom} hadEn(x: seq<real>)
            returns (amp: seq<real>, y : seq<seq<bv1>>)
            requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)

            ensures |y| == |amp|

            ensures forall k :: 0 <= k < |y| ==> |y[k]| == |x|
            ensures |y| == pow2(|x|)

            ensures forall k :: 0 <= k < |y| ==> castBVInt(y[k]) == k
            ensures forall k :: 0 <= k < |amp| ==>
                                      assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
                                      amp[k] == 1.0 / (sqrt(pow2(|x|) as real))

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

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

function {:axiom} omega(n:nat, a:nat): real

method hadtest(n: nat, q: seq<bv1>) returns (amp5: seq<real>, q5: seq<seq<bv1>>)
  requires n > 0
  requires |q| == n
  requires castBVInt(q) == 0
  requires forall tmp2 :: 0 <= tmp2 < n ==> q[tmp2] == 0
  ensures |q5| == pow2(n)
  ensures forall tmp6 :: 0 <= tmp6 < |q5| ==> |q5[tmp6]| == n
  ensures |amp5| == pow2(n)
  ensures forall k :: 0 <= k < pow2(n) ==> amp5[k] == (1.0 / sqrt((pow2(n) as real)))
  ensures forall k :: 0 <= k < pow2(n) ==> castBVInt(q5[k]) == k
{
  var q3:seq<real>;
  q3 := hadNorHad(q);
  var q4:seq<seq<bv1>>;
  var amp4:seq<real>;
  amp4, q4 := hadEn(q3);
  q5, amp5 := q4, amp4;
}