function method {:axiom} omega(n:nat, a:nat): real

function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}

function method pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function {:axiom} sqrt(a:real) :real 
    
    

lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0

function {:axiom} oracleFun (x:nat, a:nat, n:nat) : nat
    requires n > 0
    ensures oracleFun(x, a, n) == (a + x) % n

method hadNorHad(x:seq<bv1>) returns (y : seq<real>) 
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |x| ==> y[k] == omega(x[k] as int,2)
{//for validating them in Dafny
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

method {:axiom} hadEn(x: seq<real>)
            returns (y : seq<seq<bv1>>, ampy: seq<real>, py: seq<real>) 
  requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)
					
  ensures |y| == |ampy| == |py|
						 
  ensures forall k :: 0 <= k < |y| ==> |y[k]| == |x|
  ensures |y| == pow2(|x|)
														 
  ensures forall k :: 0 <= k < |y| ==> castBVInt(y[k]) == k 
  ensures forall k :: 0 <= k < |ampy| ==> 
                            assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
                            ampy[k] == 1.0 / (sqrt(pow2(|x|) as real))
  ensures forall k :: 0 <= k < |py| ==> py[k] == omega(0,2)



method {:axiom} lambdaBaseNor(x: seq<bv1>, a:nat, n:nat) returns (y : seq<bv1>) 
  requires n > 0
  ensures |x| == |y|
  ensures castBVInt(y) == oracleFun(castBVInt(x), a, n)


method lambdaBaseEn(x: seq<seq<bv1>>, a:nat, n:nat) returns (y1 : seq<seq<bv1>>) 
  requires n > 0
  ensures |x| == |y1|
  ensures forall k :: 0 <= k < |y1| ==> castBVInt(y1[k]) == oracleFun(castBVInt(x[k]), a, n)
{
  y1 := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant i == |y1|
    invariant forall j :: 0 <= j < i ==> castBVInt(y1[j]) == oracleFun(castBVInt(x[j]), a, n)
  {
    var t := lambdaBaseNor(x[i], a, n);
    y1 := y1 + [t];
    i := i + 1;
  }

}

method lambdatest(n: nat, q: seq<bv1>, k:nat )
    returns(y: seq<seq<bv1>>, ampy: seq<real>, phasey: seq<real>)
requires |q| == n
requires forall k :: 0<=k<n ==> q[k] == 0
ensures |y| == pow2(n)
ensures |ampy| == |y|
ensures |phasey| == |y|
ensures forall i :: 0 <= i < |y| ==> castBVInt(y[i]) == (i+1)%pow2(n)
ensures forall i :: 0 <= i < |y| ==> 
                            assert sqrt(pow2(n) as real) > 0.0 by {SqrtGt(pow2(n) as real);}
                            ampy[i] == 1.0 / (sqrt(pow2(n) as real))
ensures forall i :: 0 <= i < |phasey| ==> phasey[i] == omega(0, 2)
{
    var t := hadNorHad(q);
    y, ampy, phasey := hadEn(t);
    y := lambdaBaseEn(y, 1, pow2(n));
}