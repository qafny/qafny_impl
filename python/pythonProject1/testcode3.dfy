function method {:axiom} omega(n:nat, a:nat): real

function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}
function pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function {:axiom} sqrt(a:real) :real 
    
    

lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0

method {:axiom} hadEn(x1:seq<seq<bv1>>, x2:seq<seq<bv1>>,  amp: seq<real>, phase: seq<real>)
            returns (y1 : seq<seq<seq<bv1>>>, y2:seq<seq<seq<bv1>>>, ampy: seq<seq<real>>, py: seq<seq<real>>) 
  requires |x1| == |amp| == |phase|
  requires |x2| == |amp| == |phase|
  ensures |y1| == |x1|
  ensures |y2| == |x2|
  ensures |y1| == |y2|
  ensures |amp| == |ampy|
  ensures |phase| == |py|
  ensures forall k :: 0 <= k < |y1| ==> |y1[k]| == |x1|
  ensures forall k :: 0 <= k < |y2| ==> |y2[k]| == |x2|
  ensures forall k :: 0 <= k < |y1| ==> |ampy[k]| == |amp|
  ensures forall k :: 0 <= k < |y1| ==> |py[k]| == |phase|
  ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |y1[k]| ==> castBVInt(y1[k][j]) == k
  ensures forall k :: 0 <= k < |y2| ==> forall j :: 0 <= j < |y2[k]| ==> castBVInt(y2[k][j]) == j
  ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |ampy[k]| ==> 
                            assert sqrt(pow2(|y1|) as real) > 0.0 by {SqrtGt(pow2(|y1|) as real);}
                            ampy[k][j] == 1.0 / (pow2(|y1|) as real)
  ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |py[k]| ==> py[k][j] == omega(castBVInt(x1[j]) * k,2) * phase[j]


method hadtest(n:nat, baseLq: seq<seq<bv1>>, baseLp: seq<seq<bv1>>, ampL: seq<real>, phaseL: seq<real>)   
        returns (y1:seq<seq<seq<bv1>>>, y2:seq<seq<seq<bv1>>>, ampy: seq<seq<real>>, py: seq<seq<real>>)
requires |ampL| == |phaseL| == |baseLq| == |baseLp| == pow2(n)
requires forall new_k :: 0 <= new_k < pow2(n) ==> castBVInt(baseLq[new_k]) == new_k
requires forall new_k :: 0 <= new_k < pow2(n) ==> castBVInt(baseLp[new_k]) == new_k 
ensures |ampL| == |ampy|
ensures |phaseL| == |py|
ensures |y1| == |y2| == |baseLq| == |baseLp|
ensures forall k :: 0 <= k < |y1| ==> |y1[k]| == |baseLq|
ensures forall k :: 0 <= k < |y2| ==> |y2[k]| == |baseLp|
ensures forall k :: 0 <= k < |y1| ==> |ampy[k]| == |ampL|
ensures forall k :: 0 <= k < |y1| ==> |py[k]| == |phaseL|
ensures forall i :: 0 <= i < |y1| ==> forall j :: 0 <= j < |y1[i]| ==> castBVInt(y1[i][j]) == i
ensures forall i :: 0 <= i < |y2| ==> forall j :: 0 <= j < |y2[i]| ==> castBVInt(y2[i][j]) == j
ensures forall i :: 0 <= i < |ampy| ==> forall j :: 0 <= j < |ampy[i]| ==> 
                                                                        assert sqrt(pow2(|y1|) as real) > 0.0 by {SqrtGt(pow2(|y1|) as real);}
                                                                        ampy[i][j] == 1.0 / (pow2(|y1|) as real) 
ensures forall i :: 0 <= i < |py| ==> forall j :: 0 <= j < |py[i]| ==> py[i][j] == omega(castBVInt(baseLq[j]) * i,2) * phaseL[j]
{
    y1, y2, ampy, py := hadEn(baseLq, baseLp, ampL, phaseL);
}

