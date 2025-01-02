function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}


function {:axiom} oracleFun (x:nat, a:nat, n:nat) : nat
    requires n > 0
    ensures oracleFun(x, a, n) == a * x % n

method {:axiom} lambdaBaseNor(x: seq<bv1>, a:nat, n:nat) returns (y : seq<bv1>) 
  requires n > 0
  ensures |x| == |y|
  ensures castBVInt(y) == oracleFun(castBVInt(x), a, n)


method lambdaBaseEn(x: seq<seq<bv1>>, z: seq<seq<bv1>>, a:nat, n:nat) returns (y1 : seq<seq<bv1>>, y2 : seq<seq<bv1>>) 
  requires n > 0
  ensures |x| == |y1|
  ensures forall k :: 0 <= k < |y1| ==> castBVInt(y1[k]) == oracleFun(castBVInt(x[k]), a, n)
  ensures |z| == |y2|
  ensures forall k :: 0 <= k < |y2| ==> castBVInt(y2[k]) == oracleFun(castBVInt(z[k]), a, n)
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

  y2 := [];
  i := 0;
  while (i < |z|)
    invariant 0 <= i <= |z|
    invariant i == |y2|
    invariant forall j :: 0 <= j < i ==> castBVInt(y2[j]) == oracleFun(castBVInt(z[j]), a, n)
  {
    var t := lambdaBaseNor(z[i], a, n);
    y2 := y2 + [t];
    i := i + 1;
  }
}


method lambdatest(n:nat, a:nat, baseLq: seq<seq<bv1>>, baseLp: seq<seq<bv1>>, ampL: seq<real>, phaseL: seq<real>)
    requires n > 0
{
    var r1, r2:= lambdaBaseEn(baseLq, baseLp, a, n);
}