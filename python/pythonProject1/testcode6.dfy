function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}

function {:axiom} omega(n:nat, a:nat): real

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

method lambdaEn2(baseLq: seq<seq<seq<bv1>>>, baseLp: seq<seq<seq<bv1>>>, ampL: seq<seq<real>>, phaseL: seq<seq<real>>, n:nat, omega1:nat, omega2:nat) 
        returns (y1: seq<seq<seq<bv1>>>, y2: seq<seq<seq<bv1>>>, ampy: seq<seq<real>>, phasey: seq<seq<real>>)
requires n > 0
requires |ampL| == |phaseL| == |baseLq| == |baseLp| == pow2(n)
requires forall i :: 0 <= i < |baseLq| ==> forall j :: 0 <= j < |baseLq[i]| ==> castBVInt(baseLq[i][j]) == i
requires forall i :: 0 <= i < |baseLp| ==> forall j :: 0 <= j < |baseLp[i]| ==> castBVInt(baseLp[i][j]) == j
requires forall i :: 0 <= i < |ampL| ==> forall j :: 0 <= j < |ampL[i]| ==> ampL[i][j] == 1.0 / pow2(n) as real
requires forall i :: 0 <= i < |phaseL| ==> forall j :: 0 <= j < |phaseL[i]| ==> phaseL[i][j] == 1 as real
ensures |baseLq| == |y1|
ensures |baseLp| == |y2|
ensures forall k :: 0 <= k < |y1| ==> |y1[k]| == |baseLq[k]|
ensures forall k :: 0 <= k < |y2| ==> |y2[k]| == |baseLp[k]|
ensures forall i :: 0 <= i < |y1| ==> forall j :: 0 <= j < |y1[i]| ==> castBVInt(y1[i][j]) == i
ensures forall i :: 0 <= i < |y2| ==> forall j :: 0 <= j < |y2[i]| ==> castBVInt(y2[i][j]) == j
ensures forall i :: 0 <= i < |ampy| ==> forall j :: 0 <= j < |ampy[i]| ==> ampy[i][j] == 1.0 / pow2(n) as real
ensures forall i :: 0 <= i < |phasey| ==> forall j :: 0 <= j < |phasey[i]| ==> phasey[i][j] == omega(omega1, omega2)


method lambdatest(n:nat, baseLq: seq<seq<seq<bv1>>>, baseLp: seq<seq<seq<bv1>>>, ampL: seq<seq<real>>, phaseL: seq<seq<real>>, k:nat)
    returns (y1: seq<seq<seq<bv1>>>, y2: seq<seq<seq<bv1>>>, ampy: seq<seq<real>>, phasey: seq<seq<real>>)
requires n > 0
requires |ampL| == |phaseL| == |baseLq| == |baseLp| == pow2(n)
requires forall i :: 0 <= i < |baseLq| ==> forall j :: 0 <= j < |baseLq[i]| ==> castBVInt(baseLq[i][j]) == i
requires forall i :: 0 <= i < |baseLp| ==> forall j :: 0 <= j < |baseLp[i]| ==> castBVInt(baseLp[i][j]) == j
requires forall i :: 0 <= i < |ampL| ==> forall j :: 0 <= j < |ampL[i]| ==> ampL[i][j] == 1.0 / pow2(n) as real
requires forall i :: 0 <= i < |phaseL| ==> forall j :: 0 <= j < |phaseL[i]| ==> phaseL[i][j] == 1 as real
ensures |baseLq| == |y1|
ensures |baseLp| == |y2|
ensures forall k :: 0 <= k < |y1| ==> |y1[k]| == |baseLq[k]|
ensures forall k :: 0 <= k < |y2| ==> |y2[k]| == |baseLp[k]|
ensures forall i :: 0 <= i < |y1| ==> forall j :: 0 <= j < |y1[i]| ==> castBVInt(y1[i][j]) == i
ensures forall i :: 0 <= i < |y2| ==> forall j :: 0 <= j < |y2[i]| ==> castBVInt(y2[i][j]) == j
ensures forall i :: 0 <= i < |ampy| ==> forall j :: 0 <= j < |ampy[i]| ==> ampy[i][j] == 1.0 / pow2(n) as real
ensures forall i :: 0 <= i < |phasey| ==> forall j :: 0 <= j < |phasey[i]| ==> phasey[i][j] == omega(k, pow2(n))
{
    y1, y2, ampy, phasey := lambdaEn2(baseLq, baseLp, ampL, phaseL,n, k, pow2(n));
}