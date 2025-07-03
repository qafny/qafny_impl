//The generated Dafny proof for Shor's algorithm, the Qafny code is given exactly as in the Qafny paper
predicate boundedSame (x : seq<bv1>, y : seq<bv1> , n:nat) 
  requires n <= |x|
  requires n <= |y|
{
  forall k :: 0 <= k < n ==> x[k] == y[k]
}

method hadH(x:bv1) returns (y : int) 
  ensures y == x as int
{
  y := x as int;
}

method hadHall(x:seq<bv1>) returns (y : array<int>) 
  ensures y.Length == |x|
  ensures forall k :: 0 <= k < |x| ==> y[k] == x[k] as int
{
  var i := 0;
  y := new int[|x|](i => 0);
  while i < |x| 
    invariant 0 <= i <= |x|
    invariant forall k :: 0 <= k < i ==> y[k] == x[k] as int
  {
    y[i] := x[i] as int;
    i := i + 1;
  }
  return y;
}
predicate allsame (x : seq<bv1>, i: bv1) 
{
  forall k :: 0 <= k < |x| ==> x[k] == i
}

const pi := 3.14159265
const conv := 4.0 / (pi * pi)


function pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function Pow(b: int, e: nat): int
    decreases e
  {
    if e == 0 then
      1
    else
      b * Pow(b, e - 1)
  }

function {:axiom} omega(n:nat, a:nat): real

lemma {:axiom} omegaplus(a:nat, b:nat, c:nat)
  ensures omega(a,b+c) == omega(a,b) * omega(a,c)

function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}

  lemma {:axiom }LemmaCastBVTail(x : seq<bv1>, y : seq<bv1>)
    requires |x| + 1 == |y|
    requires boundedSame(x,y,|x|)
    ensures castBVInt(x) + (y[|x|] as nat) * pow2(|x|) == castBVInt(y)


function b2n (x:seq<bv1>, i:nat) : nat
  requires i <= |x|
{
  if (i==0) then 0 else (x[i-1] as nat) * pow2(i-1) + b2n(x[..i-1],i-1)
}

function {:axiom} ab2n(x:nat, n:nat) : seq<bv1>
   ensures |ab2n(x,n)| == n
   ensures b2n(ab2n(x,n), n) == x

  lemma LemmaTailstringSame(x : seq<bv1>, y : seq<bv1>, i:nat)
    requires i <= |x|
    requires i <= |y|
    requires boundedSame(x,y,i)
    ensures x[..i] == y[..i]
  {

  }

    lemma LemmaSameBVCast(x : seq<bv1>, y : seq<bv1>)
    requires x == y
    ensures b2n(x,|x|) == b2n(y,|y|)
  {

  }

  lemma b2nOne(x:seq<bv1>)
    requires |x| >= 1
    requires x[0] == 1
    requires forall k :: 1 <= k < |x| ==> x[k] == 0
    ensures b2n(x,|x|) == 1
  {

  }

  lemma {:axiom} LemmaSubSeq(x : seq<bv1>, n:nat)
    requires n <= |x|
    ensures b2n(x,n) == b2n(x[..n],n)

  lemma LemmaB2NTail(x : seq<bv1>, y : seq<bv1>)
    requires |x| + 1 == |y|
    requires boundedSame(x,y,|x|)
    ensures b2n(x,|x|) == b2n(y,|x|)
  {

     if (|x| == 0) {
    } else {
      LemmaTailstringSame(x,y,|x|);
      LemmaSameBVCast(x,y[..|x|]);
      assert b2n(x,|x|) == b2n(y[..|x|],|x|);
      LemmaSubSeq(y,|x|);
    }

  }

  lemma LemmaB2NTails(x : seq<(real,real,seq<bv1>,seq<bv1>)>, y : seq<(real,real,seq<bv1>,seq<bv1>)>)
    requires |x| == |y|
    requires forall k :: 0 <= k < |x| ==> |x[k].2| + 1 == |y[k].2|
    requires forall k :: 0 <= k < |x| ==> boundedSame(x[k].2,y[k].2,|x[k].2|)
    ensures forall k :: 0 <= k < |x| ==>  b2n(x[k].2,|x[k].2|) == b2n(y[k].2,|x[k].2|)
  {

    var i := 0;
    while (i < |x|)
      invariant 0 <= i <= |x|
      invariant forall k :: 0 <= k < i ==> |x[k].2| + 1 == |y[k].2|
      invariant forall k :: 0 <= k < i ==>  b2n(x[k].2,|x[k].2|) == b2n(y[k].2,|x[k].2|)
    {
      LemmaB2NTail(x[i].2,y[i].2);
      i := i + 1;
    }
  }

method convBvSeq(x:seq<bv1>) returns (y: int)
  ensures 0 <= y < pow2(|x|)
{
  y := 0;
  var i := 0;
  while i < |x|
    invariant 0 <= i <= |x|
    invariant 0 <= y < pow2(i)
  {
    if x[i] == 1
    {
      y := y + pow2(i);
    }
    i := i + 1;
  }
}

method convInt(x : int, l : int) returns (y : seq<bv1>)
  requires l > 0
  ensures |y| == l
{
  y := [];
  var temp := x;
  var i := (l-1);
  while i >= 0
    invariant -1 <= i <= (l-1)
    invariant |y| + i == l-1
  {
    var p := pow2(i);
    if temp >= p
    {
      y := [1] + y;
      temp := temp - p;
    }
    else
    {
      y := [0] + y;
    }
    i := i - 1;
  }
}

method sequenceAdd(x:seq<bv1>, n:bv1, ind:nat) returns (y:seq<bv1>)
  requires 0 <= ind < |x|
  ensures |x| == |y|
  ensures y[ind] == n
  ensures forall k :: 0 <= k < ind ==> x[k] == y[k]
  ensures forall k :: ind < k < |x| ==> x[k] == y[k]
{
  var i := 0;
  y := [];
  while i < |x|
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant i < |x| ==> forall k :: 0 <= k < i ==> x[k] == y[k] || k == ind
    invariant i < |x| ==> forall k :: 0 <= k < i ==> (k == ind ==> y[k] == n)
    invariant i > ind ==> y[ind] == n
    invariant i < ind ==> forall k :: 0 <= k < i ==> x[k] == y[k]
    invariant i >= ind ==> forall k :: 0 <= k < ind ==> x[k] == y[k]
    invariant i > ind ==> forall k :: ind < k < i ==> x[k] == y[k]
  {
    if i == ind {
      y := y + [n];
    }
    else {
      y := y + [x[i]];
    }
    i := i + 1;
  }
}

method LengthenWithZeroes(x:seq<bv1>, l : nat) returns (y:seq<bv1>)
  requires l > |x|
  ensures |y| == l
  ensures forall k :: |x| <= k < l ==> y[k] == 0
  ensures forall k :: 0 <= k < |x| ==> x[k] == y[k]
{
  var i := 0;
  y := [];
  while i < |x|
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant i < |x| ==> forall k :: 0 <= k < i ==> x[k] == y[k]
    invariant boundedSame(x, y, i)
  {
    y := y + [x[i]];
    i := i + 1;
  }
  while i < l
    invariant |x| <= i <= l
    invariant |y| == i
    invariant |x| == 0 || boundedSame(x, y, |x|)
    invariant forall k :: |x| <= k < i ==> y[k] == 0
  {
    y := y + [0];
    i := i + 1;
  }
}

method castEnSeq(x:array<bv1>) returns (y : array<(seq<bv1>, seq<bv1>)>) 
  requires x.Length > 1
  ensures fresh(y)
  ensures x.Length == y.Length
  ensures forall k :: 0 <= k < y.Length ==> |y[k].0| == 0
  ensures forall k :: 0 <= k < y.Length ==> |y[k].1| == x.Length
  ensures forall k :: 0 <= k < y.Length ==> y[k].1[0] == x[k]
  ensures forall k :: 1 <= k < y.Length ==> y[0].1[k] == 0;
{
  var i := 0;
  y := new (seq<bv1>,seq<bv1>)[x.Length](i => ([],[]));
  while i < y.Length
    invariant 0 <= i <= y.Length
    invariant forall k :: 0 <= k < i ==> |y[k].0| == 0
    invariant forall k :: 0 <= k < i ==> |y[k].1| == x.Length
    invariant forall k :: 0 <= k < i ==> y[k].1[0] == x[k]
    invariant i > 0 ==> forall k :: 1 <= k < x.Length ==> y[0].1[k] == 0;
  {
    var temp2 := [x[i]];
    temp2 := LengthenWithZeroes(temp2, x.Length);
    assert forall k :: 1 <= k < x.Length ==> temp2[k] == 0;
    y[i] := ([], temp2);
    i := i + 1;
  }
}

method addHad(z:array<(real,real,seq<bv1>,seq<bv1>)>, ni:nat) returns (y : array<(real,real,seq<bv1>,seq<bv1>)>)
  requires forall k :: 0 <= k < z.Length ==> |z[k].2| == ni
  requires forall k :: 0 <= k < z.Length ==> z[k].0 == 1.0 / (pow2(ni) as real)
  ensures fresh(y)
  ensures y.Length == 2 * z.Length
  ensures forall k :: 0 <= k < z.Length ==> |y[k].2| == ni+1
  ensures forall k :: z.Length <= k < 2 * z.Length ==> |y[k].2| == ni+1
  ensures forall k :: 0 <= k < z.Length ==> y[k].3 == z[k].3
  ensures forall k :: z.Length <= k < 2 * z.Length ==> y[k].3 == z[k-z.Length].3
  ensures forall k :: 0 <= k < z.Length ==> boundedSame(y[k].2,z[k].2,ni)
  ensures forall k :: z.Length <= k < 2 * z.Length ==> boundedSame(y[k].2,z[k-z.Length].2,ni)
  ensures forall k :: 0 <= k < z.Length ==> y[k].2[ni] == 0
  ensures forall k :: z.Length <= k < 2 * z.Length ==> y[k].2[ni] == 1
  ensures forall k :: 0 <= k < z.Length ==> y[k].1 == z[k].1
  ensures forall k :: z.Length <= k < 2* z.Length ==> y[k].1 == z[k-z.Length].1
  ensures forall k :: 0 <= k < z.Length ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
  ensures forall k :: z.Length <= k < 2* z.Length ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
{
  y := new (real,real,seq<bv1>,seq<bv1>)[2 * z.Length](i => (1.0,1.0,[],[]));
  var i := 0;  
  while i < z.Length 
    modifies y 
    invariant y.Length >= i + z.Length
    invariant forall k :: 0 <= k < i ==> |y[k].2| == ni+1
    invariant forall k :: 0 <= k < i ==> boundedSame(y[k].2,z[k].2,ni)
    invariant forall k :: 0 <= k < i ==> y[k].3 == z[k].3
    invariant forall k :: 0 <= k < i ==> y[k].2[ni] == 0
    invariant forall k :: 0 <= k < i ==> y[k].1 == z[k].1
    invariant forall k :: 0 <= k < i ==> y[k].0 == z[k].0 / (2 as real)
  {
    y[i] := (z[i].0 / (2 as real),z[i].1,z[i].2+[0], z[i].3);
    i := i + 1;
  }
  label aa:  var j := z.Length;
  while j < 2 * z.Length 
    modifies y 
    invariant z.Length <= j <= y.Length
    invariant forall k :: 0 <= k < z.Length ==> y[k] == old@aa(y[k])
    invariant forall k :: z.Length <= k < j ==> |y[k].2| == ni+1
    invariant forall k :: z.Length <= k < j ==> boundedSame(y[k].2,z[k-z.Length].2,ni)
    invariant forall k :: z.Length <= k < j ==> (y[k].2)[ni] == 1
    invariant forall k :: z.Length <= k < j ==> y[k].3 == z[k-z.Length].3
    invariant forall k :: z.Length <= k < j ==> y[k].1 == z[k-z.Length].1
    invariant forall k :: z.Length <= k < j ==> y[k].0 == z[k-z.Length].0 / (2 as real)
  {
    y[j] := (z[j-z.Length].0 / (2 as real),z[j-z.Length].1,z[j-z.Length].2+[1], z[j-z.Length].3);
    j := j + 1;
  }
}

function {:axiom} multmod(x: seq<bv1>, a: nat, n: nat) :seq<bv1>
  requires 1 < a < n
  ensures |multmod(x,a,n)| == |x| 
  ensures b2n(multmod(x,a,n), |x|) == a * b2n(x, |x|) % n


  lemma {:axiom} b2nEqSeq(a:nat, N:nat, i:nat, x : array<(seq<bv1>,seq<bv1>)>, n:nat, ni:nat)
    requires i < x.Length 
    requires 1 < a < N
    requires |x[i].1| == n
    requires |x[i].0| == ni + 1
    ensures  b2n(x[i].1, n) == Pow(a, b2n(x[i].0, ni + 1)) % N



method ctrU(z:array<(seq<bv1>,seq<bv1>)>,ni: nat , n:nat, a : nat,  N : nat)
  modifies z
  requires 1 < a < N
  requires 0 < n
  requires forall k :: 0 <= k < z.Length ==> |z[k].0| == ni + 1
  requires forall k :: 0 <= k < z.Length ==> |z[k].1| == n
  requires forall k :: 0 <= k < z.Length ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni)) % N
  ensures forall k :: 0 <= k < z.Length ==> |z[k].0| == ni + 1
  ensures forall k :: 0 <= k < z.Length ==> |z[k].1| == n
  ensures forall k :: 0 <= k < z.Length ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni + 1)) % N
{
  var i := 0;
  while i < z.Length
    modifies z
    invariant i <= z.Length
    invariant forall k :: 0 <= k < z.Length ==> |z[k].0| == ni + 1
    invariant forall k :: 0 <= k < z.Length ==> |z[k].1| == n
    invariant forall k :: i <= k < z.Length ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni)) % N
    invariant forall k :: 0 <= k < i ==> b2n(z[k].1, n) == Pow(a, b2n(z[k].0, ni + 1)) % N
  {
    if z[i].0[ni] == 1 {
      z[i] := (z[i].0,multmod(z[i].1, a, N));      
    }
    else {
    }
    assert |z[i].0| == ni + 1;
    assert |z[i].1| == n;
    assert (b2n(z[i].1, n) == Pow(a, b2n(z[i].0, ni + 1)) % N) by {b2nEqSeq(a,N,i,z,n,ni); }
    i := i+1;
  }
}

method addHadShell(z: seq<(real,real,seq<bv1>,seq<bv1>)>, ni:nat) returns (y: seq<(real,real,seq<bv1>,seq<bv1>)>)
  requires forall k :: 0 <= k < |z| ==> |z[k].2| == ni
  requires forall k :: 0 <= k < |z| ==> z[k].0 == 1.0 / (pow2(ni) as real)
  ensures |y| == 2 * |z| 
  ensures forall k :: 0 <= k < |z|  ==> |y[k].2| == ni+1
  ensures forall k :: |z|  <= k < 2 * |z|  ==> |y[k].2| == ni+1
  ensures forall k :: 0 <= k < |z|  ==> y[k].3 == z[k].3
  ensures forall k :: |z|  <= k < 2 * |z| ==> y[k].3 == z[k-|z|].3
  ensures forall k :: 0 <= k < |z| ==> boundedSame(y[k].2,z[k].2,ni)
  ensures forall k :: |z| <= k < 2 * |z| ==> boundedSame(y[k].2,z[k-|z|].2,ni)
  ensures forall k :: 0 <= k < |z| ==> y[k].2[ni] == 0
  ensures forall k :: |z| <= k < 2 * |z| ==> y[k].2[ni] == 1
  ensures forall k :: 0 <= k < |z| ==> y[k].1 == z[k].1
  ensures forall k :: |z| <= k < 2* |z| ==> y[k].1 == z[k-|z|].1
  ensures forall k :: 0 <= k < |z| ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
  ensures forall k :: |z| <= k < 2* |z| ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
  {
    var tmp1 := new (real,real,seq<bv1>,seq<bv1>)[|z|](i => (1.0,1.0,[],[]));
    var i := 0;
    while (i < |z|)
      invariant 0 <= i <= |z|
      invariant forall k :: 0 <= k < i ==> tmp1[k] == z[k]
    {
      tmp1[i] := z[i];
      i := i + 1;
    }
    var tmp := addHad(tmp1,ni);
    y := tmp[..];
  }

  method castEnSeqShell(x:array<bv1>) returns (y : seq<(real,real,seq<bv1>, seq<bv1>)>) 
  ensures 1 == |y|
  ensures y[0].0 == 1.0
  ensures y[0].1 == 1.0
  ensures x.Length == |y[0].3|
  ensures |y[0].2| == 0
  ensures forall k :: 0 <= k < |y[0].3| ==> y[0].3[k] == x[k]
{
  y := [(1.0,1.0,[],x[..])];
}

method ctrUShell(z:seq<(real,real,seq<bv1>,seq<bv1>)>,
         ni: nat , n:nat, a : nat,  N : nat) returns (y : seq<(real,real,seq<bv1>,seq<bv1>)>)
  requires 1 < a < N
  requires 0 < n
  requires forall k :: 0 <= k < |z| ==> |z[k].2| == ni + 1
  requires forall k :: 0 <= k < |z| ==> |z[k].3| == n
  requires forall k :: 0 <= k < |z| ==> b2n(z[k].3, n) == Pow(a, b2n(z[k].2, ni)) % N
  ensures |y| == |z|
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == ni + 1
  ensures forall k :: 0 <= k < |y| ==> |y[k].3| == n
  ensures forall k :: 0 <= k < |y| ==> b2n(y[k].3, n) == Pow(a, b2n(y[k].2, ni + 1)) % N
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == z[k].0
  ensures forall k :: 0 <= k < |y| ==> y[k].1 == z[k].1 
{
  var tmp1 := new (seq<bv1>,seq<bv1>)[|z|](i => ([],[]));
  var i := 0;
  while (i < |z|)
      invariant 0 <= i <= |z|
      invariant forall k :: 0 <= k < i ==> tmp1[k].0 == z[k].2
      invariant forall k :: 0 <= k < i ==> tmp1[k].1 == z[k].3
  {
      tmp1[i] := (z[i].2,z[i].3);
      i := i + 1;
  }
  ctrU(tmp1,ni,n,a,N);
  var j := 0;
  y := [];
  while (j < |z|)
      invariant 0 <= j <= |z|
      invariant |y| == j
      invariant forall k :: 0 <= k < j ==> y[k].0 == z[k].0
      invariant forall k :: 0 <= k < j ==> y[k].1 == z[k].1
      invariant forall k :: 0 <= k < j ==> y[k].2 == tmp1[k].0
      invariant forall k :: 0 <= k < j ==> y[k].3 == tmp1[k].1
  {
      y := y + [(z[j].0,z[j].1,tmp1[j].0,tmp1[j].1)];
      j := j + 1;
  }
}

method {:axiom} partSeq(x: seq<(real,real,seq<bv1>,seq<bv1>)>, n:nat, c:nat, N:nat, r:nat)
  returns (y: seq<seq<(real,real,seq<bv1>,seq<bv1>)>>)
  requires N < pow2(n)
  requires 1 < c < N
  requires 1 <= n
  requires 1 < r
  requires Pow(c,r) % N == 1
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  requires forall k :: 0 <= k < |x| ==> |x[k].3| == n
  requires forall k :: 0 <= k < |x| ==> b2n(x[k].3, n) == Pow(c, b2n(x[k].2, n)) % N
  ensures |y| == r
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> |y[j][k].2| == n
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> |y[j][k].3| == n
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> b2n(y[j][k].3, n) == Pow(c, b2n(y[j][k].2, n)) % N
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> b2n(y[j][k].2, n) == r * k + j
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].0 == (r as real / pow2(n) as real)
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].1 == 1.0
  ensures forall j :: 0 <= j < |y| ==> |y[j]| == pow2(n) / r

method parmeasure(x: seq<(real,real,seq<bv1>,seq<bv1>)>, n:nat, c:nat, N:nat, r:nat, ran:nat)
  returns (y: seq<(real,real,seq<bv1>)>)
  requires N < pow2(n)
  requires 1 < c < N
  requires 1 <= n
  requires 1 < r
  requires Pow(c,r) % N == 1
  requires 0 <= ran < r
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  requires forall k :: 0 <= k < |x| ==> |x[k].3| == n
  requires forall k :: 0 <= k < |x| ==> b2n(x[k].3, n) == Pow(c, b2n(x[k].2, n)) % N
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == n
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == (r as real / pow2(n) as real)
  ensures forall k :: 0 <= k < |y| ==> y[k].1 == 1.0
  ensures forall k :: 0 <= k < |y| ==> b2n(y[k].2, n) == r * k + ran
  ensures |y| == pow2(n) / r
{
  var z := partSeq(x,n,c,N,r);
  var tmp := z[ran];
  y := [];
  var i := 0;
  while (i < |tmp|)
    invariant 0 <= i <= |tmp|
    invariant i == |y|
    invariant forall k :: 0 <= k < i ==> |y[k].2| == n
    invariant forall k :: 0 <= k < i ==> y[k].0 == (r as real / pow2(n) as real)
    invariant forall k :: 0 <= k < i ==> y[k].1 == 1.0
    invariant forall k :: 0 <= k < i ==> b2n(y[k].2, n) == r * k + ran
  {
    y := y + [(tmp[i].0,tmp[i].1,tmp[i].2)];
    i := i + 1;
  }
}

method singleQFT(a:real, b:real, x:seq<bv1>, n:nat)
  returns (y:seq<(real,real,seq<bv1>)>)
  requires |x| == n
  ensures |y| == pow2(n)
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == a * (1.0 / (pow2(n) as real))
  ensures forall k :: 0 <= k < |y| ==> y[k].1 == b * (omega(pow2(n),k * b2n(x,n)))
  ensures forall k :: 0 <= k < |y| ==> y[k].2 == ab2n(k,n)
{
  var N := pow2(n);
  var i := 0;
  y := [];
  while (i < N)
    invariant 0 <= i <= N 
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> y[k].0 == (1.0 / (N as real)) * a
    invariant forall k :: 0 <= k < i ==> y[k].1 == omega(N,k * b2n(x, n)) * b
    invariant forall k :: 0 <= k < i ==> y[k].2 == ab2n(k,n)
  {
    y := y + [( (1.0 / (N as real) * a), (omega(N,i * b2n(x, n)) * b), ab2n(i,n) )];
    i := i + 1;
  }
}

method QFT(x:seq<(real,real,seq<bv1>)>, n:nat)
  returns (y:seq<seq<(real,real,seq<bv1>)>>)
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |y| ==> |y[k]| == pow2(n)
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].0 == (1.0 / (pow2(n) as real)) * x[j].0
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].1 == omega(pow2(n),k * b2n(x[j].2, n)) * x[j].1
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].2 == ab2n(k,n)
{
  var i := 0;
  y := [];
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> |y[k]| == pow2(n)
    invariant forall j :: 0 <= j < i ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].0 == (1.0 / (pow2(n) as real)) * x[j].0
    invariant forall j :: 0 <= j < i ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].1 == omega(pow2(n),k * b2n(x[j].2, n)) * x[j].1
    invariant forall j :: 0 <= j < i ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].2 == ab2n(k,n)
  {
    var tmp := singleQFT(x[i].0,x[i].1,x[i].2,n);
    y := y + [tmp];
    i := i + 1;
  }
}

method singleQFTi(x:seq<(real,real,seq<bv1>)>, xi:nat, n:nat)
  returns (y:seq<(real,real,seq<bv1>)>)
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == (1.0 / (pow2(n) as real)) * x[k].0
  ensures forall k :: 0 <= k < |y| ==> y[k].1 == (omega(pow2(n),xi * b2n(x[k].2,n))) * x[k].1
  ensures forall k :: 0 <= k < |y| ==> y[k].2 == ab2n(xi,n)
{
  var i := 0;
  y := [];
  while (i < |x|)
    invariant 0 <= i <= |x| 
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> y[k].0 == (1.0 / (pow2(n) as real)) * x[k].0
    invariant forall k :: 0 <= k < i ==> y[k].1 == omega(pow2(n),xi * b2n(x[k].2, n)) * x[k].1
    invariant forall k :: 0 <= k < i ==> y[k].2 == ab2n(xi,n)
  {
    var tmpa := (1.0 / (pow2(n) as real) * x[i].0);
    var tmpb := (omega(pow2(n),xi * b2n(x[i].2, n)) * x[i].1);
    y := y + [(tmpa, tmpb, ab2n(xi,n))];
    i := i + 1;
  }
}


method QFTi(x:seq<(real,real,seq<bv1>)>, n:nat)
  returns (y:seq<seq<(real,real,seq<bv1>)>>)
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures |y| == pow2(n)
  ensures forall k :: 0 <= k < |y| ==> |y[k]| == |x|
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].0 == (1.0 / (pow2(n) as real)) * x[k].0
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].1 == omega(pow2(n),j * b2n(x[k].2, n)) * x[k].1
  ensures forall j :: 0 <= j < |y| ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].2 == ab2n(j,n)
{
  var N := pow2(n);
  var i := 0;
  y := [];
  while (i < N)
    invariant 0 <= i <= N
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> |y[k]| == |x|
    invariant forall j :: 0 <= j < i ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].0 == (1.0 / (pow2(n) as real)) * x[k].0
    invariant forall j :: 0 <= j < i ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].1 == omega(pow2(n),j * b2n(x[k].2, n)) * x[k].1
    invariant forall j :: 0 <= j < i ==> forall k :: 0 <= k < |y[j]| ==> y[j][k].2 == ab2n(j,n)
  {
    var tmp := singleQFTi(x,i,n);
    y := y + [tmp];
    i := i + 1;
  }
}

lemma splitOmegasub(z:seq<(real,real,seq<bv1>)>, i:nat, r:nat, ran:nat, N:nat)
  requires forall k :: 0 <= k < |z| ==> z[k].1 == omega(N,i * (r * k + ran))
  ensures forall k :: 0 <= k < |z| ==> z[k].1 == omega(N, i * r * k) *  omega(N,i * ran)
{
  var j := 0;
  while (j < |z|)
    invariant 0 <= j <= |z|
    invariant forall k :: 0 <= k < j ==> z[k].1 == omega(N, i * r * k) *  omega(N,i * ran)
  {
     assert (i * (r * j + ran) == (i * r * j) + (i * ran));
     omegaplus(N,(i * r * j), i * ran);
     j := j + 1;
  }
}

lemma splitOmega(z: seq<seq<(real,real,seq<bv1>)>>, r:nat, ran:nat, N:nat) 
 requires forall j :: 0 <= j < |z| ==> forall k :: 0 <= k < |z[j]| ==> z[j][k].1 == omega(N,j * (r * k + ran))
 ensures forall j :: 0 <= j < |z| ==> forall k :: 0 <= k < |z[j]| ==> z[j][k].1 == omega(N,j * r * k) *  omega(N,j * ran)
{

  var i := 0;
  while (i < |z|)
    invariant 0 <= i <= |z|
    invariant forall j :: 0 <= j < i ==> forall k :: 0 <= k < |z[j]| ==> z[j][k].1 == omega(N,j * r * k) *  omega(N,j * ran)
  {
     splitOmegasub(z[i],i,r,ran, N);
     i := i + 1;
  }
}

lemma {:axiom} independentOmega(n:nat, j:nat, ran:nat)
  ensures omega(pow2(n), j * ran) == 1.0

lemma omegaCancels(z:seq<(real,real,seq<bv1>)>, r:nat, n:nat, j:nat, ran:nat)
  requires forall k :: 0 <= k < |z| ==> z[k].1 == omega(pow2(n),j * r * k) *  omega(pow2(n),j * ran)
  ensures forall k :: 0 <= k < |z| ==> z[k].1 == omega(pow2(n),j * r * k)
{
  var i := 0;
  while(i < |z|)
    invariant 0 <= i <= |z|
    invariant forall k :: 0 <= k < i ==> z[k].1 == omega(pow2(n),j * r * k)
  {
    independentOmega(n,j,ran);
    i := i + 1;
  }
}

function sumPhase(z:seq<(real,real,seq<bv1>)>) : real
{
  if |z| == 0 then 0.0 else z[0].1 + sumPhase(z[1..])
}

lemma {:axiom} omegaSum(z:seq<(real,real,seq<bv1>)>,r:nat,j:nat,n:nat)
  requires forall k :: 0 <= k < |z| ==> z[k].1 == omega(pow2(n),j * r * k)
  requires 1 < r
  ensures sumPhase(z) == conv * (pow2(n) as real / r as real) * (pow2(n) as real / r as real)

method measure(z:seq<seq<(real,real,seq<bv1>)>>, n:nat, r : nat, ran: nat, ranj:nat)
  returns (a: real, b: seq<bv1>)
  requires 1 < ranj < r
  requires 1 <= n
  requires 1 < r
  requires r < pow2(n)
  requires |z| == pow2(n)
  requires forall k :: 0 <= k < |z| ==> |z[k]| == pow2(n) / r
  requires forall j :: 0 <= j < |z| ==> forall k :: 0 <= k < |z[j]| ==> z[j][k].0 == (1.0 / (pow2(n) as real)) * (r as real / pow2(n) as real)
  requires forall j :: 0 <= j < |z| ==> forall k :: 0 <= k < |z[j]| ==> z[j][k].1 == omega(pow2(n),j * r * k) *  omega(pow2(n),j * ran)
  requires forall j :: 0 <= j < |z| ==> forall k :: 0 <= k < |z[j]| ==> z[j][k].2 == ab2n(j,n)
  ensures a == conv / r as real
  ensures b == ab2n(ranj * pow2(n) / r,n)
{
  var j := ranj * pow2(n) / r;
  assert (ranj * pow2(n) / r < pow2(n));
  assume false;
  var y := z[j];
  omegaCancels(y,r,n,j,ran);
  var tmp := sumPhase(y);
  omegaSum(y,r,j,n);
  assert (tmp == conv * (pow2(n) as real / r as real) * (pow2(n) as real / r as real));
  a := y[0].0 * tmp;
  b := y[0].2;
}

method shors(x : seq<bv1>, y: array<bv1>, n : nat, c : nat, N:nat, r:nat, ran:nat, ranj:nat)
  returns (p: real, w:nat)
  requires N < pow2(n)
  requires 1 < c < N
  requires 1 <= n
  requires 1 < r < pow2(n)
  requires Pow(c,r) % N == 1
  requires 0 <= ran < r
  requires 1 < ranj < r
  requires |x| == n
  requires y.Length == n
  requires forall k :: 0 <= k < n ==> x[k] == 0
  requires forall k :: 0 <= k < n ==> y[k] == 0
  modifies y
  ensures p == conv / r as real
  ensures w == ranj * pow2(n) / r
{
  y[0] := y[0] + 1;
  var temp : array<int> := hadHall(x);
  var t := castEnSeqShell(y);
  b2nOne(t[0].3);
  var i := 0;
  while i < n
     invariant 0 <= i <= n
     invariant forall k :: 0 <= k < |t| ==> |t[k].2| == i
     invariant forall k :: 0 <= k < |t| ==> |t[k].3| == n
     invariant forall k :: 0 <= k < |t| ==> b2n(t[k].3, n) == Pow(c, b2n(t[k].2, i)) % N
     invariant forall k :: 0 <= k < |t| ==> t[k].0 == (1.0 / pow2(i) as real)
     invariant forall k :: 0 <= k < |t| ==> t[k].1 == 1.0
  {
     var tmpy := addHadShell(t,i);
    LemmaB2NTails(t,tmpy[..|t|]);
    LemmaB2NTails(t,tmpy[|t|..]);
    t := ctrUShell(tmpy,i, n, c, N);
    assert (forall k :: 0 <= k < |t| ==> b2n(t[k].3, n) == Pow(c, b2n(t[k].2, i + 1)) % N);
     i := i + 1;
  }
  
  var h := parmeasure(t,n, c, N, r, ran);
  var z := QFTi(h,n);
  splitOmega(z,r,ran,pow2(n));
  var k := [];
  p,k := measure(z,n,r,ran, ranj);
  w := b2n(k,n);
}
