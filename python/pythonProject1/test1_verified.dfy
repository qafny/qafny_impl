function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

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

method hadtest(n: nat, q: seq<bv1>) returns (q5: seq<real>)
  requires n > 0
  requires |q| == n
  requires castBVInt(q) == 0
  requires forall tmp2 :: 0 <= tmp2 < n ==> q[tmp2] == 0
  ensures |q5| == n
  ensures forall i :: 0 <= i < n ==> q5[i] == omega(0, 2)
{
  assert castBVInt(q) == 0;
  assert forall tmp3 :: 0 <= tmp3 < n ==> q[tmp3] == 0;
  var q4:seq<real>;
  q4 := hadNorHad(q);
  assert forall i :: 0 <= i < n ==> q4[i] == omega(0, 2);
  assert forall i :: 0 <= i < n ==> q4[i] == omega(0, 2);
  q5 := q4;
}