//The generated Dafny proof for simple quantum walk, the Qafny code is given exactly as in the Qafny paper
predicate boundedSame (x : seq<bv1>, y : seq<bv1> , n:nat) 
  requires n <= |x|
  requires n <= |y|
{
  forall k :: 0 <= k < n ==> x[k] == y[k]
}

predicate allzero (x : seq<bv1>) 
{
  forall k :: 0 <= k < |x| ==> x[k]==0
}

function method pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function b2n (x:seq<bv1>, i:nat) : nat
  requires i <= |x|
{
  if (i==0) then 0 else (x[i-1] as nat) * pow2(i-1) + b2n(x,i-1)
}

function {:axiom} ab2n(x:nat, n:nat) : seq<bv1>
   ensures |ab2n(x,n)| == n
   ensures b2n(ab2n(x,n), n) == x

  lemma LemmaPow2LT (n: nat)
    ensures n <= pow2(n)
  {

  }

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

method sequenceAdd(x:seq<bv1>, n:bv1, ind:nat) returns (y:seq<bv1>)
  requires 0 <= ind < |x|
  ensures |y| == |x|
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

method addHad(z:array<(real,seq<bv1>)>, ni:nat) returns (y : array<(real,seq<bv1>)>)
  requires forall k :: 0 <= k < z.Length ==> |z[k].1| == ni
  requires forall k :: 0 <= k < z.Length ==> z[k].0 == 1.0 / (pow2(ni) as real)
  //requires 0 <= z.Length
  ensures fresh(y)
  ensures y.Length == 2 * z.Length
  ensures forall k :: 0 <= k < z.Length ==> |y[k].1| == ni+1
  ensures forall k :: z.Length <= k < 2 * z.Length ==> |y[k].1| == ni+1
  ensures forall k :: 0 <= k < z.Length ==> boundedSame(y[k].1,z[k].1,ni)
  ensures forall k :: z.Length <= k < 2 * z.Length ==> boundedSame(y[k].1,z[k-z.Length].1,ni)
  ensures forall k :: 0 <= k < z.Length ==> y[k].1[ni] == 0
  ensures forall k :: z.Length <= k < 2 * z.Length ==> y[k].1[ni] == 1
  ensures forall k :: 0 <= k < z.Length ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
  ensures forall k :: z.Length <= k < 2* z.Length ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
{
  y := new (real,seq<bv1>)[2 * z.Length](i => (1.0,[]));
  var i := 0;  
  while i < z.Length 
    modifies y 
    invariant y.Length >= i + z.Length
    invariant forall k :: 0 <= k < i ==> |y[k].1| == ni+1
    invariant forall k :: 0 <= k < i ==> boundedSame(y[k].1,z[k].1,ni)
    invariant forall k :: 0 <= k < i ==> y[k].1[ni] == 0
    invariant forall k :: 0 <= k < i ==> y[k].0 == z[k].0 / (2 as real)
  {
    y[i] := (z[i].0 / (2 as real),z[i].1+[0]);
    i := i + 1;
  }
  label aa:  var j := z.Length;
  while j < 2 * z.Length 
    modifies y 
    invariant z.Length <= j <= y.Length
    invariant forall k :: 0 <= k < z.Length ==> y[k] == old@aa(y[k])
    invariant forall k :: z.Length <= k < j ==> |y[k].1| == ni+1
    invariant forall k :: z.Length <= k < j ==> boundedSame(y[k].1,z[k-z.Length].1,ni)
    invariant forall k :: z.Length <= k < j ==> (y[k].1)[ni] == 1
    invariant forall k :: z.Length <= k < j ==> y[k].0 == z[k-z.Length].0 / (2 as real)
  {
    y[j] := (z[j-z.Length].0 / (2 as real),z[j-z.Length].1+[1]);
    j := j + 1;
  }
}

method addHadShell(z: seq<(real,seq<bv1>)>, ni:nat) returns (y: seq<(real,seq<bv1>)>)
  requires forall k :: 0 <= k < |z| ==> |z[k].1| == ni
  requires forall k :: 0 <= k < |z| ==> z[k].0 == 1.0 / (pow2(ni) as real)
  ensures |y| == 2 * |z| 
  ensures forall k :: 0 <= k < |z|  ==> |y[k].1| == ni+1
  ensures forall k :: |z|  <= k < 2 * |z|  ==> |y[k].1| == ni+1
  ensures forall k :: 0 <= k < |z| ==> boundedSame(y[k].1,z[k].1,ni)
  ensures forall k :: |z| <= k < 2 * |z| ==> boundedSame(y[k].1,z[k-|z|].1,ni)
  ensures forall k :: 0 <= k < |z| ==> y[k].1[ni] == 0
  ensures forall k :: |z| <= k < 2 * |z| ==> y[k].1[ni] == 1
  ensures forall k :: 0 <= k < |z| ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
  ensures forall k :: |z| <= k < 2* |z| ==> y[k].0 == 1.0 / (pow2(ni + 1) as real)
  {
    var tmp1 := new (real,seq<bv1>)[|z|](i => (1.0,[]));
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

  lemma LemmaB2NTails(x : seq<(real,seq<bv1>)>, y : seq<(real,seq<bv1>)>)
    requires |x| <= |y|
    requires forall k :: 0 <= k < |x| ==> |x[k].1| + 1 == |y[k].1|
    requires forall k :: 0 <= k < |x| ==> boundedSame(x[k].1,y[k].1,|x[k].1|)
    ensures forall k :: 0 <= k < |x| ==>  b2n(x[k].1,|x[k].1|) == b2n(y[k].1,|x[k].1|)
  {

    var i := 0;
    while (i < |x|)
      invariant 0 <= i <= |x|
      invariant forall k :: 0 <= k < i ==> |x[k].1| + 1 == |y[k].1|
      invariant forall k :: 0 <= k < i ==>  b2n(x[k].1,|x[k].1|) == b2n(y[k].1,|x[k].1|)
    {
      LemmaB2NTail(x[i].1,y[i].1);
      i := i + 1;
    }
  }

  lemma LemmaB2NPow2(x : seq<bv1>, i : nat)
    requires |x| == i + 1
    ensures b2n(x,i+1) == b2n(x,i) + (x[i] as nat) * pow2(i)
  {

  }

  lemma LemmaSeqEQ(x : seq<bv1>, i : nat)
    requires |x| == i
    ensures x[..i] == x
  {

  }

  lemma LemmaB2NPow2x(x : seq<bv1>, y : seq<bv1>, i : nat)
    requires |x| == i
    requires |y| == i + 1
    requires boundedSame(x,y,i)
    ensures b2n(y,i+1) == b2n(x,i) + (y[i] as nat) * pow2(i)
  {
    LemmaB2NPow2(y,i);
    LemmaB2NTail(x,y);
  }

  lemma LemmaB2NPow2s(x : seq<(real,seq<bv1>)>, y : seq<(real,seq<bv1>)>, i : nat)
    requires |x| == |y|
    requires forall k :: 0 <= k < |x| ==> |y[k].1| == i + 1
    requires forall k :: 0 <= k < |x| ==> |x[k].1| == i
    requires forall k :: 0 <= k < |x| ==> boundedSame(x[k].1,y[k].1,i)
    ensures forall k :: 0 <= k < |x| ==> b2n(x[k].1,i) + (y[k].1[i] as nat) * pow2(i) == b2n(y[k].1,i+1)
  {
    var j := 0;
    while(j < |x|)
      invariant 0 <= j <= |x|
      invariant forall k :: 0 <= k < j ==> b2n(x[k].1,i) + (y[k].1[i] as nat) * pow2(i) == b2n(y[k].1,i+1)
    {
      LemmaB2NPow2x(x[j].1,y[j].1,i);
      j := j + 1;
    }
  }

method hadHallEN(n:nat) returns (y : seq<(real,seq<bv1>)>)
  requires 0 < n
  ensures |y| == pow2(n)
  ensures 0 < |y|;
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == (1.0 / pow2(n) as real)
  ensures forall k :: 0 <= k < |y| ==> |y[k].1| == n
  ensures forall k :: 0 <= k < |y| ==> b2n(y[k].1,n) == k
{
   var i := 0;
   y := [(1 as real, [])];
   while (i < n)
     invariant |y| == pow2(i)
     invariant 0 <= i <= n
     invariant forall k :: 0 <= k < |y| ==> y[k].0 == (1.0 / pow2(i) as real)
     invariant forall k :: 0 <= k < |y| ==> |y[k].1| == i
     invariant forall k :: 0 <= k < |y| ==> b2n(y[k].1,i) == k
   {
    var tmpy := addHadShell(y,i);
    LemmaB2NPow2s(y,tmpy[..|y|],i);
    LemmaB2NPow2s(y,tmpy[|y|..],i);
    y := tmpy;
    i := i + 1;
   }
}

method castNorEn(x:seq<(real,seq<bv1>)>, y : seq<bv1>) returns (z: seq<seq<bv1>>)
  ensures |x| == |z|
  ensures forall k :: 0 <= k < |z| ==> z[k] == y
{
  var i := 0;
  z := [];
  while (i < |x|)
    invariant i == |z|
    invariant 0 <= i <= |x|
    invariant forall k :: 0 <= k < |z| ==> z[k] == y
  {
    z := z + [y];
    i := i + 1;
  }
}

method castNorEnSingle(x:seq<(real,seq<bv1>)>, y : bv1) returns (z: seq<bv1>)
  ensures |x| == |z|
  ensures forall k :: 0 <= k < |z| ==> z[k] == y
{
  var i := 0;
  z := [];
  while (i < |x|)
    invariant i == |z|
    invariant 0 <= i <= |x|
    invariant forall k :: 0 <= k < |z| ==> z[k] == y
  {
    z := z + [y];
    i := i + 1;
  }
}

method bitflip(x:array<bv1>, j:nat)
  modifies x
  requires j < x.Length
  ensures j < x.Length
  ensures forall t :: j < t < x.Length ==> x[t] == old(x[t])
  ensures forall t :: 0 <= t < j ==> x[t] == old(x[t])
  ensures x[j] == 1
{
  x[j] := 1;
}

method bitflipShell(x:seq<bv1>, j:nat) returns (y:seq<bv1>)
  requires j < |x|
  ensures j < |y|
  ensures |y| == |x|
  ensures forall t :: j < t < |y| ==> y[t] == x[t]
  ensures forall t :: 0 <= t < j ==> y[t] == x[t]
  ensures y[j] == 1

{
  var tmp1 := new bv1[|x|](i => 0);
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant forall k :: 0 <= k < i ==> tmp1[k] == x[k]
    {
      tmp1[i] := x[i];
      i := i + 1;
    }
  bitflip(tmp1,j); 
  y := tmp1[..];
}

predicate sameLengths(x:seq<seq<bv1>>, n:nat)
{
  forall k :: 0 <= k < |x| ==> |x[k]| == n
}

predicate lengthPat<T>(x:seq<seq<T>>, i:nat)
{
  forall k :: 0 <= k < |x| ==> |x[k]| == pow2(k + i)
}

predicate ampPat(x:seq<real>, n:nat, i:nat)
{
  forall k :: 0 <= k < |x| ==> x[k] == (1.0 / pow2(k + n + i) as real)
}


method bitflipShells(x:seq<seq<bv1>>, ind:nat, j:nat, n:nat) returns (y:seq<seq<bv1>>)
  requires j < n
  requires ind < j
  requires sameLengths(x, n)
  requires forall q :: 0 <= q < |x| ==> forall t :: 0 <= t < j - ind - 1 ==> x[q][t] == 0
  requires forall q :: 0 <= q < |x| ==> forall t :: j - ind - 1 <= t < j ==> x[q][t] == 1
  requires forall q :: 0 <= q < |x| ==> forall t :: j <= t < |x[q]| ==> x[q][t] == 0 
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |x| ==> |y[k]| == |x[k]|
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < j - ind - 1 ==> y[k][t] == 0
  ensures forall k :: 0 <= k < |y| ==> forall t :: j - ind -1 <= t < j + 1 ==> y[k][t] == 1
  ensures forall k :: 0 <= k < |y| ==> forall t :: j + 1 <= t < |x[k]| ==> y[k][t] == 0 
{
  y := [];
  var i := 0;
  assert j - ind - 1 < j;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> |y[k]| == |x[k]|
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < j - ind - 1 ==> y[k][t] == 0
    invariant forall k :: 0 <= k < i ==> forall t :: j - ind -1 <= t < j+1 ==> y[k][t] == 1
    invariant forall k :: 0 <= k < i ==> forall t :: j + 1 <= t < |y[k]| ==> y[k][t] == 0 
  {
    var tmpv := bitflipShell(x[i],j);
    y := y + [tmpv];
    i := i + 1;
  }
}

method fbs(x:seq<bv1>) returns (y:seq<bv1>)
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> y[k] == ! x[k]
{
  y := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> y[k] == ! x[k]
  {
    y := y + [!x[i]];
    i := i + 1;
  }
}

method doubleflip(x:seq<seq<bv1>>) returns (y : seq<seq<bv1>>)
  requires lengthPat(x,0)
  ensures lengthPat(y,1)
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |y| ==> |y[k]| == 2 * |x[k]|
  ensures forall k :: 0 <= k < |y| ==> boundedSame(x[k],y[k],|x[k]|)
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == ! x[k][t]
{
  y := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall k :: 0 <= k < i ==> |y[k]| == 2 * |x[k]|
    invariant forall k :: 0 <= k < i ==> boundedSame(x[k],y[k],|x[k]|)
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == ! x[k][t]
  {
    var tmp := fbs(x[i]);
    y := y + [x[i]+tmp];
    i := i + 1;
  }
}

method ctrLT(axr: seq<real>, axb: seq<seq<seq<bv1>>>, ay: seq<seq<seq<bv1>>>, ah : seq<seq<seq<bv1>>>, au : seq<seq<bv1>>,
        tx: seq<(real,seq<bv1>)>, ty: seq<seq<bv1>>, th : seq<seq<bv1>>, tu : seq<bv1>, ln:nat, txn: nat, hn : nat, j : nat)
  returns (zxr: seq<real>, zxb: seq<seq<seq<bv1>>>, zy: seq<seq<seq<bv1>>>, zh : seq<seq<seq<bv1>>>, zu : seq<seq<bv1>>,
         txa: seq<(real,seq<bv1>)>, tya: seq<seq<bv1>>, tha : seq<seq<bv1>>, tua : seq<bv1>)
  requires j < pow2(txn)
  requires 0 < ln 
  requires |tx| == ln
  requires |ty| == ln
  requires |th| == ln
  requires |tu| == ln
  requires ampPat(axr, txn, 1)
  requires |axr| == |axb| == |ay| == |ah| == |au| == j
  requires forall k :: 0 <= k < |th| ==> |th[k]| == hn
  requires forall k :: 0 <= k < |ah| ==> sameLengths(ah[k],hn)
  requires forall k :: 0 <= k < |ay| ==> sameLengths(ay[k], pow2(txn))
  requires forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: 0 <= t < j - k - 1 ==> ay[k][q][t] == 0
  requires forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: j - k - 1 <= t < j ==> ay[k][q][t] == 1
  requires forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: j <= t < |ay[k][q]| ==> ay[k][q][t] == 0 
  requires forall k :: 0 <= k < |tx| ==> tx[k].0 == (1.0 / pow2(txn) as real)
  requires forall k :: 0 <= k < |tx| ==> |tx[k].1| == txn
  requires forall k :: 0 <= k < |tx| ==> b2n(tx[k].1,txn) == j + k
  requires forall k :: 0 <= k < |ty| ==> allzero(ty[k])
  requires forall k :: 0 <= k < |ty| ==> |ty[k]| == pow2(txn)
  requires forall k :: 0 <= k < |th| ==> allzero(th[k])
  requires allzero(tu)
  requires lengthPat(axb,1)
  requires lengthPat(ay,1)
  requires lengthPat(ah,1)
  requires lengthPat(au,1)
  ensures |zxr| == |axr| + 1
  ensures |zxb| == |axb| + 1
  ensures |zh| == |ah| + 1
  ensures |zu| == |au| + 1
  ensures |zy| == |ay| + 1
  ensures ampPat(zxr, txn, 0)
  ensures forall k :: 0 <= k < |axb| ==> axb[k] == zxb[k+1]
  ensures zxb[0] == [tx[0].1]
  ensures b2n(zxb[0][0],txn) == j
  ensures forall k :: 0 <= k < |ah| ==> ah[k] == zh[k+1]
  ensures forall k :: 0 <= k < |au| ==> au[k] == zu[k+1]
  ensures |zh[0]| == 1
  ensures allzero(zh[0][0])
  ensures forall k :: 0 <= k < |zh| ==> sameLengths(zh[k],hn)
  ensures zu[0] == [0]
  ensures forall k :: 0 <= k < |zy| ==> sameLengths(zy[k], pow2(txn))
  ensures forall k :: 0 <= k < |zy| ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: 0 <= t < j - k ==> zy[k][q][t] == 0
  ensures forall k :: 0 <= k < |zy| ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: j - k <= t < j + 1 ==> zy[k][q][t] == 1
  ensures forall k :: 0 <= k < |zy| ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: j + 1 <= t < |zy[k][q]| ==> zy[k][q][t] == 0
  ensures txa == tx[1..]
  ensures tya == ty[1..]
  ensures tha == th[1..]
  ensures tua == tu[1..]
  ensures lengthPat(zxb,0)
  ensures lengthPat(zy,0)
  ensures lengthPat(zh,0)
  ensures lengthPat(zu,0)
{
  txa := tx[1..];
  tya := ty[1..];
  tha := th[1..];
  tua := tu[1..];
  zxr,zxb :=([tx[0].0]+axr),([[tx[0].1]]+axb);
  zh := [[th[0]]]+ah;
  zu := [[tu[0]]]+au;
  zy := [];
  var i := 0;
  while (i < |ay|)
    invariant 0 <= i <= |ay|
    invariant |zy| == i
    invariant forall k :: 0 <= k < i ==> sameLengths(zy[k], pow2(txn))
    invariant forall k :: 0 <= k < i ==> |ay[k]| == |zy[k]|
    invariant forall k :: 0 <= k < i ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: 0 <= t < j - k - 1 ==> zy[k][q][t] == 0
    invariant forall k :: 0 <= k < i ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: j - k - 1 <= t < j + 1 ==> zy[k][q][t] == 1
    invariant forall k :: 0 <= k < i ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: j + 1 <= t < |zy[k][q]| ==> zy[k][q][t] == 0
  {
    var tmpv := bitflipShells(ay[i],i, j,pow2(txn));
    zy := zy + [tmpv];
    i := i + 1;
  }
  var tamyv := bitflipShell(ty[0],j);
  zy := [[tamyv]] + zy;
}

method doubleBases(x: seq<seq<seq<bv1>>>) returns (y:seq<seq<seq<bv1>>>)
  requires lengthPat(x,0)
  ensures lengthPat(y,1)
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < |x[k]| ==> x[k][t] == y[k][t]
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == x[k][t]

{
  y := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant lengthPat(y,1)
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |x[k]| ==> x[k][t] == y[k][t]
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == x[k][t]
  {
    y := y + [x[i]+x[i]];
    i := i + 1;
  }
}

method doubleBasesA(x: seq<seq<seq<bv1>>>, n:nat) returns (y:seq<seq<seq<bv1>>>)
  requires lengthPat(x,0)
  requires forall k :: 0 <= k < |x| ==> sameLengths(x[k], n)
  ensures lengthPat(y,1)
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < |x[k]| ==> x[k][t] == y[k][t]
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == x[k][t]
  ensures forall k :: 0 <= k < |y| ==> sameLengths(y[k], n)

{
  y := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant lengthPat(y,1)
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |x[k]| ==> x[k][t] == y[k][t]
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == x[k][t]
    invariant forall k :: 0 <= k < i ==> sameLengths(y[k], n)
  {
    y := y + [x[i]+x[i]];
    i := i + 1;
  }
}

method doubleBasesB(x: seq<seq<seq<bv1>>>, j:nat, n:nat) returns (y:seq<seq<seq<bv1>>>)
  requires j < n
  requires |x| == j + 1
  requires lengthPat(x,0)
  requires forall k :: 0 <= k < |x| ==> sameLengths(x[k], n)
  requires forall k :: 0 <= k < |x| ==> forall q :: 0 <= q < |x[k]| ==> forall t :: 0 <= t < j - k ==> x[k][q][t] == 0
  requires forall k :: 0 <= k < |x| ==> forall q :: 0 <= q < |x[k]| ==> forall t :: j - k <= t < j + 1 ==> x[k][q][t] == 1
  requires forall k :: 0 <= k < |x| ==> forall q :: 0 <= q < |x[k]| ==> forall t :: j + 1 <= t < |x[k][q]| ==> x[k][q][t] == 0 
  ensures lengthPat(y,1)
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < |x[k]| ==> x[k][t] == y[k][t]
  ensures forall k :: 0 <= k < |y| ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == x[k][t]
  ensures forall k :: 0 <= k < |y| ==> sameLengths(y[k], n)
  ensures forall k :: 0 <= k < |y| ==> forall q :: 0 <= q < |y[k]| ==> forall t :: 0 <= t < j - k ==> y[k][q][t] == 0
  ensures forall k :: 0 <= k < |y| ==> forall q :: 0 <= q < |y[k]| ==> forall t :: j - k <= t < j + 1 ==> y[k][q][t] == 1
  ensures forall k :: 0 <= k < |y| ==> forall q :: 0 <= q < |y[k]| ==> forall t :: j + 1 <= t < |y[k][q]| ==> y[k][q][t] == 0 
{
  y := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant lengthPat(y,1)
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |x[k]| ==> x[k][t] == y[k][t]
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |x[k]| ==> y[k][t+|x[k]|] == x[k][t]
    invariant forall k :: 0 <= k < i ==> sameLengths(y[k], n)
    invariant forall k :: 0 <= k < i ==> forall q :: 0 <= q < |y[k]| ==> forall t :: 0 <= t < j - k ==> y[k][q][t] == 0
    invariant forall k :: 0 <= k < i ==> forall q :: 0 <= q < |y[k]| ==> forall t :: j - k <= t < j + 1 ==> y[k][q][t] == 1
    invariant forall k :: 0 <= k < i ==> forall q :: 0 <= q < |y[k]| ==> forall t :: j + 1 <= t < |y[k][q]| ==> y[k][q][t] == 0
  {
    y := y + [x[i]+x[i]];
    i := i + 1;
  }
}

method applyHadCtr(axr: seq<real>, axb: seq<seq<seq<bv1>>>, ay: seq<seq<seq<bv1>>>, ah : seq<seq<seq<bv1>>>, au : seq<seq<bv1>>, j: nat, txn: nat, hn : nat)
  returns (zxr: seq<real>, zxb: seq<seq<seq<bv1>>>, zy: seq<seq<seq<bv1>>>, zh : seq<seq<seq<bv1>>>, zu : seq<seq<bv1>>)
  requires j < pow2(txn)
  requires |axr| == |axb| == |ay| == |ah| == |au| == j + 1
  requires forall k :: 0 <= k < |ay| ==> sameLengths(ay[k], pow2(txn))
  requires forall k :: 0 <= k < |ah| ==> sameLengths(ah[k], hn)
  requires forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: 0 <= t < j - k ==> ay[k][q][t] == 0
  requires forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: j - k <= t < j + 1 ==> ay[k][q][t] == 1
  requires forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: j + 1 <= t < |ay[k][q]| ==> ay[k][q][t] == 0 
  requires lengthPat(axb,0)
  requires lengthPat(ay,0)
  requires lengthPat(ah,0)
  requires lengthPat(au,0)
  requires ampPat(axr,txn, 0)
  ensures ampPat(zxr,txn,1)
  ensures |axr| == |zxr|
  ensures lengthPat(zxb,1)
  ensures |axb| == |zxb|
  ensures forall k :: 0 <= k < |zxb| ==> forall t :: 0 <= t < |axb[k]| ==> axb[k][t] == zxb[k][t]
  ensures forall k :: 0 <= k < |zxb| ==> forall t :: 0 <= t < |axb[k]| ==> zxb[k][t+|axb[k]|] == axb[k][t]
  ensures lengthPat(zy,1)
  ensures |ay| == |zy|
  ensures forall k :: 0 <= k < |zy| ==> forall t :: 0 <= t < |ay[k]| ==> ay[k][t] == zy[k][t]
  ensures forall k :: 0 <= k < |zy| ==> forall t :: 0 <= t < |ay[k]| ==> zy[k][t+|ay[k]|] == ay[k][t]
  ensures forall k :: 0 <= k < |zy| ==> sameLengths(zy[k], pow2(txn))
  ensures forall k :: 0 <= k < |zy| ==> |zy[k]| == 2 * |ay[k]|
  ensures forall k :: 0 <= k < |zy| ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: 0 <= t < j - k ==> zy[k][q][t] == 0
  ensures forall k :: 0 <= k < |zy| ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: j - k <= t < j + 1 ==> zy[k][q][t] == 1
  ensures forall k :: 0 <= k < |zy| ==> forall q :: 0 <= q < |zy[k]| ==> forall t :: j + 1 <= t < |zy[k][q]| ==> zy[k][q][t] == 0 
  ensures lengthPat(zh,1)
  ensures |ah| == |zh|
  ensures forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |ah[k]| ==> ah[k][t] == zh[k][t]
  ensures forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |ah[k]| ==> zh[k][t+|ah[k]|] == ah[k][t]
  ensures forall k :: 0 <= k < |zh| ==> sameLengths(zh[k], hn)
  ensures lengthPat(zu,1)
  ensures |zu| == |au|
  ensures forall k :: 0 <= k < |zu| ==> |zu[k]| == 2 * |au[k]|
  ensures forall k :: 0 <= k < |zu| ==> boundedSame(au[k],zu[k],|au[k]|)
  ensures forall k :: 0 <= k < |zu| ==> forall t :: 0 <= t < |au[k]| ==> zu[k][t+|au[k]|] == ! au[k][t]

{
  var i := 0;
  zxr := [];
  zxb := doubleBases(axb);
  zh := doubleBasesA(ah,hn);
  zy := doubleBasesB(ay,j,pow2(txn));
  zu := doubleflip(au);
  while (i < |axr|)
    invariant 0 <= i <= |axr|
    invariant i == |zxr|
    invariant ampPat(zxr,txn, 1)
  {
    zxr := zxr + [axr[i] / 2.0];
    i := i + 1;
  }
}

function method {:axiom} bitArithA(a:seq<bv1>, n:nat) : seq<bv1>
  requires |a| == n
  ensures |a| == |bitArithA(a,n)|
  ensures b2n(bitArithA(a,n),n) == 2 * b2n(a,n) + 1

function method {:axiom} bitArithB(a:seq<bv1>, n:nat) : seq<bv1>
  requires |a| == n
  ensures |a| == |bitArithA(a,n)|
  ensures b2n(bitArithA(a,n),n) == 2 * b2n(a,n) + 2

method goleft(ah : seq<seq<bv1>>, au : seq<bv1>, n:nat) returns (zh : seq<seq<bv1>>)
  requires |ah| == |au|
  requires forall k :: 0 <= k < |ah| ==> |ah[k]| == n
  ensures |ah| == |zh|
  ensures forall k :: 0 <= k < |zh| ==> |zh[k]| == n
  ensures forall k :: 0 <= k < |zh| ==> au[k] == 0 ==> b2n(zh[k],n) == 2 * b2n(ah[k],n) + 1
  ensures forall k :: 0 <= k < |zh| ==> au[k] == 1 ==> zh[k] == ah[k]
{
  zh := [];
  var i := 0;
  while (i < |ah|)
    invariant 0 <= i <= |ah|
    invariant i == |zh|
    invariant forall k :: 0 <= k < i ==> |zh[k]| == n
    invariant forall k :: 0 <= k < i ==> au[k] == 0 ==> b2n(zh[k],n) == 2 * b2n(ah[k],n) + 1
    invariant forall k :: 0 <= k < i ==> au[k] == 1 ==> zh[k] == ah[k]
  {
    if(au[i] == 0) 
    {
      var tmp := bitArithA(ah[i],n);
      zh := zh + [tmp];
    }
    else {
      zh := zh + [ah[i]];
    }
    i := i + 1;
  }
}


method golefts(ah : seq<seq<seq<bv1>>>, au : seq<seq<bv1>>, n:nat) returns (zh : seq<seq<seq<bv1>>>)
  requires |ah| == |au|
  requires forall k :: 0 <= k < |ah| ==> |ah[k]| == |au[k]|
  requires forall k :: 0 <= k < |ah| ==> sameLengths(ah[k],n)
  ensures |ah| == |zh|
  ensures forall k :: 0 <= k < |zh| ==> |zh[k]| == |au[k]|
  ensures forall k :: 0 <= k < |zh| ==> sameLengths(zh[k],n)
  ensures forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 0 ==> b2n(zh[k][t],n) == 2 * b2n(ah[k][t],n) + 1
  ensures forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 1 ==> zh[k][t] == ah[k][t]
{
  zh := [];
  var i := 0;
  while (i < |ah|)
    invariant 0 <= i <= |ah|
    invariant i == |zh|
    invariant forall k :: 0 <= k < i ==> sameLengths(zh[k],n)
    invariant forall k :: 0 <= k < i ==> |zh[k]| == |au[k]|
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 0 ==> b2n(zh[k][t],n) == 2 * b2n(ah[k][t],n) + 1
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 1 ==> zh[k][t] == ah[k][t]
  {
    var tmp := goleft(ah[i],au[i],n);
    zh := zh + [tmp];
    i := i + 1;
  }
}

method goright(ah : seq<seq<bv1>>, au : seq<bv1>, n:nat) returns (zh : seq<seq<bv1>>)
  requires |ah| == |au|
  requires forall k :: 0 <= k < |ah| ==> |ah[k]| == n
  ensures |ah| == |zh|
  ensures forall k :: 0 <= k < |zh| ==> |zh[k]| == n
  ensures forall k :: 0 <= k < |zh| ==> au[k] == 1 ==> b2n(zh[k],n) == 2 * b2n(ah[k],n) + 2
  ensures forall k :: 0 <= k < |zh| ==> au[k] == 0 ==> zh[k] == ah[k]
{
  zh := [];
  var i := 0;
  while (i < |ah|)
    invariant 0 <= i <= |ah|
    invariant i == |zh|
    invariant forall k :: 0 <= k < i ==> |zh[k]| == n
    invariant forall k :: 0 <= k < i ==> au[k] == 1 ==> b2n(zh[k],n) == 2 * b2n(ah[k],n) + 2
    invariant forall k :: 0 <= k < i ==> au[k] == 0 ==> zh[k] == ah[k]
  {
    if(au[i] == 1) 
    {
      var tmp := bitArithB(ah[i],n);
      zh := zh + [tmp];
    }
    else {
      zh := zh + [ah[i]];
    }
    i := i + 1;
  }
}

method gorights(ah : seq<seq<seq<bv1>>>, au : seq<seq<bv1>>, n:nat) returns (zh : seq<seq<seq<bv1>>>)
  requires |ah| == |au|
  requires forall k :: 0 <= k < |ah| ==> |ah[k]| == |au[k]|
  requires forall k :: 0 <= k < |ah| ==> sameLengths(ah[k],n)
  ensures |ah| == |zh|
  ensures forall k :: 0 <= k < |zh| ==> |zh[k]| == |au[k]|
  ensures forall k :: 0 <= k < |zh| ==> sameLengths(zh[k],n)
  ensures forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 1 ==> b2n(zh[k][t],n) == 2 * b2n(ah[k][t],n) + 2
  ensures forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 0 ==> zh[k][t] == ah[k][t]
{
  zh := [];
  var i := 0;
  while (i < |ah|)
    invariant 0 <= i <= |ah|
    invariant i == |zh|
    invariant forall k :: 0 <= k < i ==> sameLengths(zh[k],n)
    invariant forall k :: 0 <= k < i ==> |zh[k]| == |au[k]|
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 1 ==> b2n(zh[k][t],n) == 2 * b2n(ah[k][t],n) + 2
    invariant forall k :: 0 <= k < i ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 0 ==> zh[k][t] == ah[k][t]
  {
    var tmp := goright(ah[i],au[i],n);
    zh := zh + [tmp];
    i := i + 1;
  }
}
method qwalk(x:seq<bv1>, y: seq<bv1>, h : seq<bv1>, u:bv1, m : nat, n:nat) 
   returns (axr : seq<real>, axb: seq<seq<seq<bv1>>>, ay : seq<seq<seq<bv1>>>, ah : seq<seq<seq<bv1>>>, au : seq<seq<bv1>>)
  requires |x| > 0
  requires m == pow2(|x|) //|x| is t in the paper
  requires m < n
  requires |y| == m
  requires |h| == n
  requires forall k :: 0 <= k < |x| ==> x[k] == 0
  requires forall k :: 0 <= k < |y| ==> y[k] == 0
  requires forall k :: 0 <= k < |h| ==> h[k] == 0
  requires u == 0
  ensures |axr| == |axb| == |ay| == |ah| == |au| == m
  ensures forall k :: 0 <= k < |ah| ==> sameLengths(ah[k], n)
  ensures forall k :: 0 <= k < |ay| ==> sameLengths(ay[k], m) 
  ensures forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: 0 <= t < m - k - 1 ==> ay[k][q][t] == 0
  ensures forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: m - k - 1 <= t < m ==> ay[k][q][t] == 1
  ensures forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: m <= t < |ay[k][q]| ==> ay[k][q][t] == 0 
  ensures ampPat(axr, |x|, 1)
  ensures lengthPat(axb,1)
  ensures lengthPat(ay,1)
  ensures lengthPat(ah,1)
  ensures lengthPat(au,1)
{
  var tmpx := hadHallEN(|x|);
  assert |tmpx| == m;

  var tmpy := castNorEn(tmpx,y);
  var tmph := castNorEn(tmpx,h);
  var tmpu := castNorEnSingle(tmpx,u);
  axr := [];
  axb := [];
  ay := [];
  ah := [];
  au := [];
  var j := 0;
  LemmaPow2LT(m);

  while(j < m)
    invariant 0 <= j <= m
    invariant |tmpx| == m - j
    invariant |tmpy| == m - j
    invariant |tmph| == m - j
    invariant |tmpu| == m - j
    invariant |axr| == |axb| == |ay| == |ah| == |au| == j 
    invariant forall k :: 0 <= k < |tmph| ==> |tmph[k]| == n
    invariant forall k :: 0 <= k < |tmpy| ==> |tmpy[k]| == m
    invariant forall k :: 0 <= k < |tmpx| ==> |tmpx[k].1| == |x|
    invariant forall k :: 0 <= k < |tmpx| ==> b2n(tmpx[k].1,|x|) == j + k
    invariant forall k :: 0 <= k < |tmpy| ==> allzero(tmpy[k]);
    invariant forall k :: 0 <= k < |tmph| ==> allzero(tmph[k]);
    invariant allzero(tmpu);
    invariant forall k :: 0 <= k < |ah| ==> sameLengths(ah[k], n)
    invariant forall k :: 0 <= k < |ay| ==> sameLengths(ay[k], m)
    invariant forall k :: 0 <= k < |tmpx| ==> tmpx[k].0 == (1.0 / m as real)    
    invariant forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: 0 <= t < j - k - 1 ==> ay[k][q][t] == 0
    invariant forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: j - k - 1 <= t < j ==> ay[k][q][t] == 1
    invariant forall k :: 0 <= k < |ay| ==> forall q :: 0 <= q < |ay[k]| ==> forall t :: j <= t < |ay[k][q]| ==> ay[k][q][t] == 0 
    invariant ampPat(axr, |x|, 1)
    invariant lengthPat(axb,1)
    invariant lengthPat(ay,1)
    invariant lengthPat(ah,1)
    invariant lengthPat(au,1)
  {
    axr,axb,ay,ah,au,tmpx,tmpy,tmph,tmpu := ctrLT(axr,axb,ay,ah,au,tmpx,tmpy,tmph,tmpu,m-j,|x|,n,j);
    axr,axb,ay,ah,au := applyHadCtr(axr,axb,ay,ah,au,j,|x|, n);
    var zh := golefts(ah,au,n);
    zh := gorights(zh,au,n);
    assert forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 1 ==> b2n(zh[k][t],n) == 2 * b2n(ah[k][t],n) + 2;
    assert forall k :: 0 <= k < |zh| ==> forall t :: 0 <= t < |zh[k]| ==> au[k][t] == 0 ==> b2n(zh[k][t],n) == 2 * b2n(ah[k][t],n) + 1;
    ah := zh;
    j := j + 1;
  }
  assert tmpx == [];
  assert tmpy == [];
  assert tmph == [];
  assert tmpu == [];
}
