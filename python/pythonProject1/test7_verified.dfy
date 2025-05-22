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

function {:axiom} omega(n:nat, a:nat): real

function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

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

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

lemma {:axiom} SqrtGt(a:real)
                requires a > 0.0
                ensures sqrt(a) > 0.0

method {:axiom} newFun5(q4: seq<seq<bv1>>, amp4: seq<real>, n: nat) returns (q6: seq<seq<bv1>>, amp6: seq<real>)
  ensures |amp6| == |amp4| == |q6| == |q4|
  ensures forall tmp7 :: 0 <= tmp7 < |amp6| ==> |q4[tmp7]| == |q6[tmp7]|
  ensures forall tmp7 :: 0 <= tmp7 < |q6| ==> castBVInt(q6[tmp7]) == ((castBVInt(q4[tmp7]) + 1) % pow2(n))
  ensures forall tmp7 :: 0 <= tmp7 < |amp6| ==> amp6[tmp7] == (amp4[tmp7] * omega(0, 1))

method lambdatest(n: nat, k: nat, q: seq<bv1>) returns (amp7: seq<real>, q7: seq<seq<bv1>>)
  requires n > 0
  requires |q| == n
  requires castBVInt(q) == 0
  requires forall tmp2 :: 0 <= tmp2 < n ==> q[tmp2] == 0
  ensures |q7| == pow2(n)
  ensures forall tmp8 :: 0 <= tmp8 < |q7| ==> |q7[tmp8]| == n
  ensures |amp7| == pow2(n)
  ensures forall j :: 0 <= j < pow2(n) ==> amp7[j] == (1.0 / sqrt((pow2(n) as real)))
  ensures forall j :: 0 <= j < pow2(n) ==> castBVInt(q7[j]) == ((j + 1) % pow2(n))
{
  var q3:seq<real>;
  q3 := hadNorHad(q);
  var q4:seq<seq<bv1>>;
  var amp4:seq<real>;
  amp4, q4 := hadEn(q3);
  var q6:seq<seq<bv1>>;
  var amp6:seq<real>;
  q6, amp6 := newFun5(q4, amp4, n);
  omega0();
  q7, amp7 := q6, amp6;
}