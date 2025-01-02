//Dafny Proof for DeutschJozsa algorithm, the Qafny version of the code is in the ~Qafnycode/test/Resource/Algorithms
function method {:axiom} sqrt(a:real) : real

lemma {:axiom} sqrtgreater(a:real)
   requires a > 0.0
   ensures sqrt(a) > 0.0

lemma {:axiom} sqrtdouble(a:real)
  ensures sqrt(a) * sqrt(a) == a

function method {:axiom} Pow(b: real, e: nat): real

lemma {:axiom} PowGreater(a:real,e:nat)
   ensures Pow(a,e) > 0.0

lemma {:axiom} PowZero(a:real, e:nat)
   requires e == 0
   ensures Pow(a,0) == 1.0

predicate boundedSame (x : seq<bv1>, y : seq<bv1> , n:nat) 
  requires n <= |x|
  requires n <= |y|
{
  forall k :: 0 <= k < n ==> x[k] == y[k]
}

function method pow2(N:nat):nat
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

  lemma {:axiom} pow2gt()
    ensures forall e: nat {:trigger pow2(e)} :: 0 < pow2(e)

function method bitToNat(a:bv1, i:nat) : nat
{
  if a == 0 then 0 else pow2(i)
}

function method castbvtonat (x:seq<bv1>, n:nat) : nat
  requires n <= |x|
  decreases n
{
  if (n == 0) then 0 else castbvtonat(x, n-1) + bitToNat(x[n-1],n-1)
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
  ensures forall k :: 0 <= k < |y| ==> y[k].0 == sqrt(pow2(n) as real)
  ensures forall k :: 0 <= k < |y| ==> y[k].1 == 1.0
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == n
  ensures allzero(y[0].2)
  ensures forall k :: 0 <= k < |y| ==> |y[k].2| == n
{
   y := [(sqrt(pow2(n) as real),1.0,[])];
   var i := 0;
   while (i < n)
     invariant i <= n
     invariant |y| == pow2(i)
     invariant 0 < |y|;
     invariant forall k :: 0 <= k < |y| ==> y[k].0 == sqrt(pow2(n) as real)
     invariant forall k :: 0 <= k < |y| ==> y[k].1 == 1.0
     invariant forall k :: 0 <= k < |y| ==> |y[k].2| == i
     invariant allzero(y[0].2)
   {
    y := ncat(y,0,i) + ncat(y,1,i);
    i := i + 1;
   }

}

function method sumphase(x : seq<(real, real, seq<bv1>)>, y:seq<bv1>, i:nat) : real
  requires i <= |x|
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == |y|
{
  if i == 0 then 0.0 else x[i-1].1 * Pow(-1.0, dotproduct(x[i-1].2,y,|y|)) + sumphase(x,y, i - 1)
}


lemma sumphasezero(x : seq<(real, real, seq<bv1>)>, y:seq<bv1>, i:nat)
  requires i <= |x|
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == |y|
  requires forall k :: 0 <= k < i ==> x[k].1 == 1.0
  requires forall k :: 0 <= k < i ==> Pow(-1.0, dotproduct(x[k].2,y,|y|)) == 1.0
  ensures sumphase(x,y,i) == (i as real)
{

}

method addphase(x : seq<(real, real, seq<bv1>)>, y:seq<bv1>) returns (z : real)
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == |y|
  ensures z == sumphase(x,y,|x|)
{
  z := 0.0;
  var i := 0;
  while (i < |x|)
    invariant i <= |x|
    invariant z == sumphase(x,y,i)
  {
    z := z + (x[i].1 * Pow(-1.0, dotproduct(x[i].2,y,|y|)));
    i := i + 1;
  }
}

method nHEnPost(x: seq<(real,real,seq<bv1>)>, n:nat,y: seq<(real,real,seq<bv1>)>)
       returns (z : seq<(real,real,seq<bv1>)>)
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  requires forall k :: 0 <= k < |y| ==> |y[k].2| == n
  requires forall k :: 0 <= k < |y| ==> y[k].0 == sqrt(pow2(n) as real)
  requires |y| == |x|
  ensures |z| == |y|
  ensures forall k :: 0 <= k < |z| ==> z[k].0 == (pow2(n) as real)
  ensures forall k :: 0 <= k < |z| ==> |z[k].2| == n
  ensures forall k :: 0 <= k < |z| ==> z[k].2 == y[k].2
  ensures forall k :: 0 <= k < |z| ==> z[k].1 == sumphase(x,y[k].2,|x|)
{
   z := [];
   var i := 0;
   while (i < |y|)
     invariant i <= |y|
     invariant i == |z|
     invariant forall k :: 0 <= k < i ==> z[k].2 == y[k].2
     invariant forall k :: 0 <= k < i ==> z[k].0 == pow2(n) as real
     invariant forall k :: 0 <= k < i ==> z[k].1 == sumphase(x,y[k].2,|x|)
   {
    var tmp := addphase(x,y[i].2);
    z := z + [(y[i].0 * sqrt(pow2(n) as real),tmp, y[i].2)];
    sqrtdouble(pow2(n) as real);
    i := i + 1;
   }

}

function method {:axiom} f(x:nat) : bv1

method {:axiom} oracleuc (x:seq<(real,real,seq<bv1>)>, n:nat, y:int) returns (z:seq<(real,real,seq<bv1>)>)
  requires y == -1
  requires forall k :: 0 <= k < |x| ==> |x[k].2| == n
  ensures |x| == |z|
  ensures forall k :: 0 <= k < |z| ==> |z[k].2| == n
  ensures forall k :: 0 <= k < |z| ==> z[k].0 == x[k].0
  ensures forall k :: 0 <= k < |z| ==> z[k].1 == x[k].1 * 1.0 
  ensures forall k :: 0 <= k < |z| ==> z[k].2 == x[k].2


method dj(x:seq<bv1>, y: bv1) returns (z : seq<(real,real,seq<bv1>)>)
  requires forall k :: 0 <= k < |x| ==> x[k] == 0
  requires y == 0
{
  var s := nHEnPre(|x|);
  var a := -1;
  var t := oracleuc(s,|x|, a);
  assert (forall k :: 0 <= k < |t| ==> t[k].1 == 1.0);
  z := nHEnPost(t,|x|,t);
  assert allzero(z[0].2);
  assert (forall k :: 0 <= k < |z| ==> z[k].1 == sumphase(t,z[k].2,|z|));
  dotproductzeroA(t,z[0].2,|z[0].2|);
  var i := 0;
  while (i < |t|)
    invariant 0 <= i <= |t|
    invariant forall k :: 0 <= k < i ==> Pow(-1.0, dotproduct(t[k].2,z[0].2,|z[0].2|)) == 1.0
  {
    PowZero(-1.0, dotproduct(t[i].2,z[0].2,|z[0].2|));
    i := i + 1;
  }
  sumphasezero(t,z[0].2,pow2(|x|));
  assert z[0].1 == pow2(|x|) as real;
}
