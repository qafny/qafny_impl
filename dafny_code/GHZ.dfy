//The generated Dafny proof for GHZ and controlled GHZ, the Qafny code is given exactly as in the Qafny paper
predicate boundedSame (x : seq<bv1>, y : seq<bv1> , n:nat) 
  requires n < |x|
  requires n < |y|
{
  forall k :: 0 <= k <= n ==> x[k] == y[k]
}

// this method seems to expand each bv1 seq in an array by one zero
// n is the length of the seq<bv1>, but couldn't you just do: |x[0]|?
method help(x: array<seq<bv1>>, n : nat) 
   modifies x
   requires 1 <= n
   requires forall k :: 0 <= k < x.Length ==> |x[k]| == n
   ensures forall k :: 0 <= k < x.Length ==> |x[k]| == n + 1
   ensures forall k :: 0 <= k < x.Length ==> |x[k]| == |old(x[k])| + 1
   ensures forall k :: 0 <= k < x.Length ==> x[k][n] == 0
   ensures forall k :: 0 <= k < x.Length ==> boundedSame(old(x[k]), x[k],n-1)
{
  var k : nat := 0;
  while k < x.Length
    invariant 0 <= k <= x.Length
    invariant forall j :: 0 <= j < k ==> |x[j]| == n + 1
    invariant forall j :: k <= j < x.Length ==> |x[j]| == n 
    invariant forall j :: 0 <= j < k ==> |x[j]| == |old(x[j])| + 1
    invariant forall j :: 0 <= j < k ==> x[j][|x[j]|-1] == 0
    invariant forall j :: 0 <= j < x.Length ==> boundedSame(old(x[j]), x[j],n-1)
  {
    x[k] := x[k] + [0];
    k := k + 1;
  }
}

method enH(x:bv1) returns (y : array<seq<bv1>>) 
  ensures fresh(y)
  ensures y.Length == 2
  ensures y[0] == [0]
  ensures y[1] == [1]
{
  y := new seq<bv1>[2][[0],[1]];
  return y;
}

predicate boundedSamelt (x : seq<bv1>, y : seq<bv1> , l:nat, h:nat) 
  requires |x| == |y|
  requires h <= |x|
{
  forall k :: l < k < h ==> x[k] == y[k]
}

method cnotNor(x:seq<bv1>, i:nat) returns (y:seq<bv1>)
  requires i + 1 < |x|
  ensures |x| == |y|
  ensures boundedSame(x,y,i)
  ensures x[i] ^ x[i+1] == y[i+1]
  ensures boundedSamelt(x,y,i+1,|x|)
{
  if (x[i] == 1) 
  { 
    y := x[..i+1]+([! x[i+1]])+x[i+2..|x|];

    assert y[i+1] == x[i] ^ x[i+1];
   }
  else {return x;}
}

method cnotEN(x:array<seq<bv1>>, i:nat)
  modifies x
  requires forall k :: 0 <= k < x.Length ==> i + 1 < |x[k]|
  ensures forall k :: 0 <= k < x.Length ==> |old(x[k])| == |x[k]|
  ensures forall k :: 0 <= k < x.Length ==> boundedSame(old(x[k]),x[k],i)
  ensures forall k :: 0 <= k < x.Length ==> (old(x[k])[i]) ^ (old(x[k])[i+1]) == x[k][i+1]
  ensures forall k :: 0 <= k < x.Length ==> boundedSamelt(old(x[k]),x[k],i+1,|x[k]|)
{
   var k := 0;
   while k < x.Length 
     invariant 0 <= k <= x.Length
     invariant forall j :: 0 <= j < x.Length ==> |old(x[j])| == |x[j]|
     invariant forall j :: k <= j < x.Length ==> old(x[j]) == x[j]
     invariant forall j :: 0 <= j < k ==> boundedSame(old(x[j]),x[j],i)
     invariant forall j :: 0 <= j < k ==> (old(x[j])[i]) ^ (old(x[j])[i+1]) == x[j][i+1]
     invariant forall j :: 0 <= j < k ==> boundedSamelt(old(x[j]),x[j],i+1,|x[j]|)
   {
      if (x[k][i] == 1) 
        { 
            x[k] := x[k][..i+1]+([! x[k][i+1]])+x[k][i+2..|x[k]|];

            assert x[k][i+1] == old(x[k][i]) ^ old(x[k][i+1]);
        }
      k := k + 1;
   }
}

predicate allsame (x : seq<bv1>, i: bv1) 
{
  forall k :: 0 <= k < |x| ==> x[k] == i
}

method GHZ(x : array<bv1>, n : nat) returns (z: array<seq<bv1>>)
  modifies x
  requires x.Length == n
  requires 1 <= n
  requires forall k :: 0 <= k < x.Length ==> x[k] == 0
  ensures z.Length == 2
  ensures forall k :: 0 <= k < z.Length ==> |z[k]| == n 
  ensures forall k :: 0 <= k < z.Length ==> allsame(z[k],k as bv1)
{
  var i : int := 1;
  var y := [x[0]];
  z := enH(y[0]);
  while i < n 
    invariant 1 <= i <= n
    invariant forall k :: 0 <= k < z.Length ==> |z[k]| == i
    invariant forall k :: 0 <= k < z.Length ==> allsame(z[k],k as bv1)
  {
    help(z, i);
    cnotEN(z,i-1);
    i := i + 1;
  }
}

method GHZShell(x : seq<bv1>, n : nat) returns (z: seq<seq<bv1>>)
  requires |x| == n
  requires 1 <= n
  requires forall k :: 0 <= k < |x| ==> x[k] == 0
  ensures |z| == 2
  ensures forall k :: 0 <= k < |z| ==> |z[k]| == n 
  ensures forall k :: 0 <= k < |z| ==> allsame(z[k],k as bv1)
{
  var tmp1 := new (bv1)[|x|](i => 0);
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant forall k :: 0 <= k < i ==> tmp1[k] == x[k]
  {
    tmp1[i] := x[i];
    i := i + 1;
  }

  var tmp := GHZ(tmp1,n);
  z := tmp[..];
}

method singleHad(a:bv1) returns (y : seq<bv1>)
  requires a == 0
  ensures y == [0,1]
{
  y := [0,1];
}

method nzero(n:nat) returns (y : seq<bv1>)
  ensures |y| == n
  ensures forall k :: 0 <= k < |y| ==> y[k] == 0
{
  var i := 0;
  y := [];
  while (i < n)
    invariant 0 <= i <= n
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> y[k] == 0
  {
    y := y + [0];
    i := i + 1;
  }
}

method CGHZ(a:bv1, x : seq<bv1>, n : nat) returns (z: seq<seq<bv1>>)
  requires |x| == n
  requires 1 <= n
  requires a == 0
  requires forall k :: 0 <= k < |x| ==> x[k] == 0
  ensures |z| == 3
  ensures forall k :: 0 <= k < |z| ==> |z[k]| == n + 1
  ensures forall k :: 0 <= k < n + 1 ==> z[0][k] == 0
  ensures forall k :: 0 <= k < n ==> z[1][k+1] == 0
  ensures forall k :: 0 <= k < n ==> z[2][k+1] == 1
  ensures z[1][0] == 1
  ensures z[2][0] == 1
{
  var tmp := singleHad(a);
  var tmpa := GHZShell(x,n);
  var tmpq := nzero(n);
  z := [[tmp[0]]+tmpq];
  z := z + [[tmp[1]] + tmpa[0]] + [[tmp[1]] + tmpa[1]];
}
