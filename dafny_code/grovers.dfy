//The generated Dafny proof for Grover's algorithm, the Qafny code is given exactly as in the Qafny paper's appendix
predicate boundedSame (x : seq<bv1>, y : seq<bv1> , n:nat) 
  requires n <= |x|
  requires n <= |y|
{
  forall k :: 0 <= k < n ==> x[k] == y[k]
}

function method pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function method Pow(b: int, e: nat): int
    decreases e
  {
    if e == 0 then
      1
    else
      b * Pow(b, e - 1)
  }

function method b2n (x:seq<bv1>, i:nat) : nat
  requires i <= |x|
{
  if (i==0) then 0 else (x[i-1] as nat) * pow2(i-1) + b2n(x[..i-1],i-1)
}

function method {:axiom} ab2n(x:nat, n:nat) : seq<bv1>
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


function method dotproduct(x:seq<bv1>,y:seq<bv1>,i:nat) : nat
  requires i <= |x|
  requires i <= |y|
{
   if i == 0 then 0 else (x[i-1] as nat * y[i-1] as nat + dotproduct(x,y,i-1))
}

predicate allzero (x : seq<bv1>) 
{
  forall k :: 0 <= k < |x| ==> x[k]==0
}

 lemma dotproductzero(x:seq<bv1>,y:seq<bv1>,i:nat)
  requires i <= |x|
  requires i <= |y|
  requires forall k :: 0 <= k < i ==> y[k] == 0
  ensures dotproduct(x,y,i) == 0
{

}

 lemma {:axiom} dotproductzeroA(x:seq<(real,real,seq<bv1>)>,y:seq<bv1>,n:nat)
  requires |y| == n
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures forall k :: 0 <= k < |x| ==> dotproduct(x[k].2,y,n) == 0

function method ncat(x:seq<(real,real,seq<bv1>)>,y:bv1,n:nat) : seq<(real,real,seq<bv1>)>
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures |x| == |ncat(x,y,n)|
  ensures forall k :: 0 <= k < |x| ==> x[k].0 == ncat(x,y,n)[k].0
  ensures forall k :: 0 <= k < |x| ==> x[k].1 == ncat(x,y,n)[k].1
  ensures forall k :: 0 <= k < |x| ==> |ncat(x,y,n)[k].2| == n + 1
  ensures forall k :: 0 <= k < |x| ==> boundedSame(x[k].2,ncat(x,y,n)[k].2,n)
  ensures forall k :: 0 <= k < |x| ==> ncat(x,y,n)[k].2[|x[k].2|] == y
{
  if (|x| == 0) then [] else [(x[0].0,x[0].1,x[0].2+[y])] + ncat(x[1..],y,n)
}


method nHEnPre(n:nat) returns (y : seq<(real,real,seq<bv1>)>)
  ensures |y| == pow2(n)
  ensures 0 < |y|;
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == (1.0 / pow2(n) as real)
  ensures forall k :: 0 <= k < |y| ==> y[k].1 == 1.0
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == n
  ensures allzero(y[0].2)
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == n
{
   y := [((1.0 / pow2(n) as real),1.0,[])];
   var i := 0;
   while (i < n)
     invariant i <= n
     invariant |y| == pow2(i)
     invariant 0 < |y|;
     invariant forall k :: 0 <= k < |y| ==> y[k].0 == (1.0 / pow2(n) as real)
     invariant forall k :: 0 <= k < |y| ==> y[k].1 == 1.0
     invariant forall k :: 0 <= k < |y| ==> |y[k].2| == i
     invariant allzero(y[0].2)
   {
    y := ncat(y,0,i) + ncat(y,1,i);
    i := i + 1;
   }

}

function method {:axiom} f(x:seq<bv1>) : bv1

function method {:axiom} cos(a:real): real

function method {:axiom} sin(a:real): real

function method cosa(n:nat, a:real) : real
{
  cos(n as real * a)
}

function method sina(n:nat, a:real) : real
{
  sin(n as real * a)
}

lemma {:axiom} sinplus(za:real, a:real, n:nat, m:nat)
  requires za == cosa(m,a) * sina(n,a) + sina(m,a) * cosa(n,a)
  ensures za == sina(m+n,a)

lemma {:axiom} cosplus(za:real, a:real, n:nat, m:nat)
  requires za == cosa(m,a) * cosa(n,a) - sina(m,a) * sina(n,a)
  ensures za == cosa(m+n,a)

predicate equalBv (x : seq<seq<bv1>>, target:bv1) 
{
  forall k :: 0 <= k < |x| ==> f(x[k])==target
}

method splitBasesAux(x:seq<(real,real,seq<bv1>)>, m:nat, n:nat) returns (y:seq<seq<bv1>>, z:seq<seq<bv1>>)
  requires |x| == m
  requires forall k :: 0 <= k < |x| ==> x[k].0 == (1.0 / pow2(n) as real)
  requires forall k :: 0 <= k < |x| ==> x[k].1 == 1.0
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures |y| + |z| == m
  ensures equalBv(y, 0)
  ensures equalBv(z, 1)
{
    y := [];
    z := [];
    var i := 0;

    while(i < m)
      invariant 0 <= i <= m
      invariant |y| + |z| == i
      invariant equalBv(y,0)
      invariant equalBv(z,1)
    {
        if(f(x[i].2)==0)
        {
            y := y + [x[i].2];
        }
        else
        {
            z := z + [x[i].2]; 
        }
        i := i + 1;
    }

}

method splitBases(x:seq<(real,real,seq<bv1>)>, m:nat, n:nat) returns (ya:real, y:seq<seq<bv1>>, za:real, z:seq<seq<bv1>>)
  requires 0 < m
  requires |x| == m
  requires forall k :: 0 <= k < |x| ==> x[k].0 == (1.0 / pow2(n) as real)
  requires forall k :: 0 <= k < |x| ==> x[k].1 == 1.0
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures |y| + |z| == m
  ensures equalBv(y, 0)
  ensures equalBv(z, 1)
  ensures ya == cos(|z| as real / m as real)
  ensures za == sin(|z| as real / m as real)
{
    y,z := splitBasesAux(x,m,n);
    ya := (cos(|z| as real / m as real));
    za := (sin(|z|as real / m as real));
}

method oracleuc (ya:real, y:(seq<seq<bv1>>)) returns (za:real)
  requires equalBv(y, 1)
  ensures za == - ya
{
  za := - ya;
}

method dis(ya: real, y:seq<seq<bv1>>, za:real, z:seq<seq<bv1>>, theta:real) returns (yaa:real, zaa:real)
  requires equalBv(y, 0)
  requires equalBv(z, 1)
  ensures zaa == ya * sin(2.0 * theta) - za * cos(2.0 * theta)
  ensures yaa == ya * cos(2.0 * theta) + za * sin(2.0 * theta)
{
  zaa := ya * sina(2, theta) - za * cosa(2, theta);
  yaa := ya * cosa(2, theta) + za * sina(2, theta);
}

method grovers(x:seq<bv1>, n:nat, t : nat) returns (theta:real, ya: real, y:seq<seq<bv1>>,za:real, z:seq<seq<bv1>>)
  requires forall k :: 0 <= k < |x| ==> x[k] == 0
  requires |x| == n
  ensures ya == cosa(2*t+1, theta)
  ensures za == sina(2*t+1, theta)
  ensures equalBv(y, 0)
  ensures equalBv(z, 1) 
{
  var s := nHEnPre(|x|);
  ya,y,za,z := splitBases(s, pow2(n),n);
  theta := 0.0;
  theta := (|z| as real / pow2(n) as real);
  assert (ya == cosa(1,theta));
  assert (za == sina(1,theta));
  var i := 0;
  while (i < t)
    invariant 0 <= i <= t
    invariant ya == cosa(2 * i + 1, theta)
    invariant za == sina(2 * i + 1, theta)
  {
    var tmpza := oracleuc(za,z);
    ya, za := dis(ya,y,tmpza,z, theta);
    sinplus(za, theta, 2*i+1, 2);
    cosplus(ya, theta, 2* i + 1, 2);
    i := i + 1;
  }
}
