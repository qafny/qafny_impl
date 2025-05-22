function {:axiom} pow2(N:nat): int
                  ensures pow2(N) > 0

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0

function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0

function {:axiom} omega(n:nat, a:nat): real

method {:axiom} newFun3(q: seq<bv1>, n: nat) returns (q4: seq<bv1>)
  ensures |q4| == |q|
  ensures castBVInt(q4) == ((castBVInt(q) + 1) % pow2(n))

method lambdatest(n: nat, k: nat, q: seq<bv1>) returns (q5: seq<bv1>)
  requires n > 0
  requires |q| == n
  requires castBVInt(q) == k
  ensures |q5| == n
  ensures castBVInt(q5) == ((k + 1) % pow2(n))
{
  var q4:seq<bv1>;
  q4 := newFun3(q, n);
  omega0();
  q5 := q4;
}