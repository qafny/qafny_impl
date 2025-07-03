function {:axiom} omega(a:nat, n:nat): real


function {:axiom} cos(a:real): real

function {:axiom} sin(a:real): real

//WLOG,we assume theta is in [0,pi/2] 
function {:axiom} arcsin(x: real) : real
  ensures arcsin(x) >= 0.0
  

function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}

function pow2(N:nat):nat 
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function sumFun(f: nat -> bool, n: nat): int
    decreases n
    ensures sumFun(f, n) == if n == 0 then 0 
                          else sumFun(f, n - 1) + sumFun((k: nat)=> f(k + pow2(n - 1)), n - 1)
{
    if n == 0 then 0
    else sumFun(f, n - 1) + sumFun((k: nat) => f(k + pow2(n - 1)), n - 1)
}


function {:axiom} sqrt(a:real) :real 

lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0

lemma {:axiom} trigo(theta: real)
  requires theta >= 0.0
  ensures sin(theta)*sin(theta) + cos(theta)*cos(theta) == 1.0

//|0> or |1> to |+> or |->
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

method {:axiom} hadAA(x: seq<seq<bv1>>, f: nat -> bool) returns (y: seq<real>)
    requires forall k :: 0 <= k < |x| ==> castBVInt(x[k]) == k
    ensures |y| == 2
    ensures y[0]*y[0] + y[1]*y[1] == 1.0
    ensures y[0] == sin(arcsin(sqrt(sumFun(f, |x|) as real)))
    ensures y[1] == cos(arcsin(sqrt(sumFun(f, |x|) as real)))


method singleGrovers(x: seq<real>, r: real) returns (y: seq<real>)
    requires |x| == 2
    requires x[0] * x[0] + x[1] * x[1] == 1.0
    requires x[0] <= 1.0/2.0 
    ensures |y| == 2
    ensures y[0] == sin(3.0 * arcsin(x[0]))
    ensures y[1] == cos(3.0 * arcsin(x[0]))
    {
        y := [sin(3.0 * arcsin(x[0])), cos(3.0 * arcsin(x[0]))];
        assert y[0] == sin(3.0*arcsin(x[0]));
        assert y[1] == cos(3.0*arcsin(x[0]));
    }

