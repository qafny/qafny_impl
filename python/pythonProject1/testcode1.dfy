function {:axiom} omega(n:nat, a:nat): real

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

method hadtest (n:nat, q:seq<bv1>)
requires |q| == n
requires forall k :: 0<=k<n ==> q[k] == 0
{
var y:= hadNorHad(q);var i:=0;
while(i<n){
assert (y[i] == omega(0,2));
i:=i+1;
}
}