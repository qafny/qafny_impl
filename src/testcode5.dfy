function pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}

function {:axiom} oracleFun (x:nat, n:nat) : nat
    requires n > 0
    ensures oracleFun(x, n) == (x + 1) % n

method {:axiom} lambdaBaseNor(x: seq<bv1>, n:nat) returns (y : seq<bv1>) 
  requires n > 0
  ensures |x| == |y|
  ensures castBVInt(y) == oracleFun(castBVInt(x), n)

method lambdatest(n:nat, q:seq<bv1>, k:nat) returns (p:seq<bv1>)
  requires n > 0
  requires n == |q|
  requires castBVInt(q) == k
  ensures castBVInt(p) == (k+1) % n
  {
    p := lambdaBaseNor(q, n);
  }