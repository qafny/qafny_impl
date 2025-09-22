function {:axiom} omega(n:nat, a:nat): real

function {:axiom} sqrt(a:real): real
  requires a > 0.0
  ensures sqrt(a) > 0.0


lemma {:axiom} omega0()
  ensures forall k : nat :: omega(0, k) == 1.0

/*function castBVInt(x : seq<bv1>) : nat
ensures castBVInt(x) == if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
{
   if |x| == 0 then 0 else ((x[0] as nat) * pow2(|x|-1)) + castBVInt(x[1..])
}*/

function {:axiom} castBVInt(x : seq<bv1>) : nat
  ensures castBVInt(x) >= 0

function b2n (x:seq<bv1>, i:nat) : nat
  requires i <= |x|
{
  if (i==0) then 0 else (x[i-1] as nat) * pow2(i-1) + b2n(x[..i-1],i-1)
}

/*function {:axiom} b2n(x : seq<bv1>, n:int) : nat
  requires n <= |x|
  ensures b2n(x, n) == if |x| == 0 then 0 else (x[0] as nat) + 2 * b2n(x[1..], n-1)*/

/*function pow2(N:nat):nat
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}*/


function {:axiom} pow2(N:nat): int
  ensures pow2(N) > 0


function powN(N:nat, k: nat) : int
  decreases k
{
    if (k == 0) then 1 else N * powN(N, k-1)
}

function boolToBV1(b : bool) : seq<bv1>
{
    if b then [1] else [0]
}

lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0




method hadNorHad(x:seq<bv1>) returns (y : seq<real>)
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |x| ==> y[k] == omega(x[k] as int,2)
{
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


method {:axiom} hadEn(x: seq<real>, y: seq<real>)
            returns (amp: seq<real>, x1: seq<seq<bv1>>, y1 : seq<seq<bv1>>) 
  requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)
  ensures |x1| == |y1| == |amp| == |x|
  ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == pow2(|y|)
  ensures forall k :: 0 <= k < |y1| ==> |y1[k]| == pow2(|y|) 
  ensures forall k :: 0 <= k < |y1| ==> castBVInt(y1[k]) == k
  ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
  ensures forall k :: 0 <= k < |amp| ==> amp[k] == (1.0 / (sqrt(pow2(|x|) as real))) * omega(0,2)


method {:axiom} norEn2(x: seq<bv1>, y: seq<seq<seq<bv1>>>)
        returns (x1: seq<seq<seq<bv1>>>)

    ensures |x1| == |y|
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |y[k]|
    ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| == |x1[k][j]| == |x|
    ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> castBVInt(x1[k][j]) == castBVInt(x)
    ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> samebit(x1[k][j], x, |x|)


method {:axiom} hadNorEn(x: seq<real>, y: seq<bv1>) returns (amp1: seq<real>, x1: seq<seq<bv1>>, y1: seq<seq<bv1>>)
    ensures |x1| == |y1| == |amp1| == pow2(|x|)
    ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |x|
    ensures forall k :: 0 <= k < |y1| ==> y1[k] == y
    ensures forall k :: 0 <= k < |amp1| ==> amp1[k] == (1.0 / sqrt(pow2(|x|) as real))


method {:axiom} partialcastEn1toEn2(x: seq<bv1>) returns (x1 : seq<seq<bv1>>)
  ensures |x1| == pow2(|x|)
  ensures forall k :: 0 <= k < pow2(|x|) ==> |x1[k]| == |x|
  ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k

method {:axiom} ampMul(amp : seq<real>, x: nat, y : seq<bv1>) returns (amp1: seq<real>)
    requires x > 0
    ensures |amp| == |amp1|
    ensures forall k :: 0 <= k < |amp| ==> amp1[k] == (amp[k] * (1.0/sqrt(x as real))) * omega(castBVInt(y), 2)

method {:axiom} duplicateSeq(x: seq<bv1>, n:nat) returns (x1:seq<seq<bv1>>)
    ensures |x1| == n
    ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
    ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x)
    ensures forall k :: 0 <= k < |x1| ==> samebit(x1[k], x, |x|)
    ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |x|

method {:axiom} createAmp(n:nat) returns(amp:seq<real>)
  ensures |amp| == n
  ensures forall k :: 0 <= k < |amp| ==> assert sqrt(n as real) > 0.0 by {SqrtGt(n as real);}
                                          amp[k] == 1.0/sqrt(n as real)

function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
    requires |x| >= n
    requires |y| >= n
    ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]

function {:axiom} bool2BV1(b : bool) : seq<bv1>
    ensures castBVInt(bool2BV1(b)) == if b then 1 else 0
    ensures |bool2BV1(b)| == 1

method {:axiom} duplicateAmp(x: real, n:nat) returns (x1:seq<real>)
    ensures |x1| == n
    ensures forall k :: 0 <= k < |x1| ==> x1[k] == x




method conditionaltest4 (n : nat, N: nat, r: seq<bv1>, u: seq<bv1>, q: seq<seq<seq<bv1>>>, p: seq<seq<seq<bv1>>>, amp: seq<seq<real>> )
    returns (q2: seq<seq<seq<seq<bv1>>>>, p2: seq<seq<seq<seq<bv1>>>>, r2: seq<seq<seq<seq<bv1>>>>, u2: seq<seq<seq<seq<bv1>>>>, amp2: seq<seq<seq<real>>>)
    requires |r| == |u| == 1
    requires castBVInt(r) == 0
    requires |p| == |q| == |amp| == pow2(n)
    requires forall k :: 0 <= k < pow2(n) ==> |p[k]| == pow2(n)
    requires forall k :: 0 <= k < pow2(n) ==> |q[k]| == pow2(n)
    requires forall k :: 0 <= k < pow2(n) ==> |amp[k]| == pow2(n)
    requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> |p[k][j]| == n
    requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> |q[k][j]| == n
    requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(q[k][j]) == k 
    requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> castBVInt(p[k][j]) == j
    requires forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> amp[k][j] == 1.0 / (pow2(n) as real)

    ensures |q2| == |p2| == |u2| == |r2| == |amp2| == pow2(n)
    ensures forall k :: 0 <= k < pow2(n) ==> |p2[k]| == pow2(n)
    ensures forall k :: 0 <= k < pow2(n) ==> |q2[k]| == pow2(n)
    ensures forall k :: 0 <= k < pow2(n) ==> |r2[k]| == pow2(n)
    ensures forall k :: 0 <= k < pow2(n) ==> |u2[k]| == pow2(n)
    ensures forall k :: 0 <= k < pow2(n) ==> |amp2[k]| == pow2(n)
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> |p2[k][j]| == pow2(1)
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> |q2[k][j]| == pow2(1)
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> |r2[k][j]| == pow2(1)
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> |u2[k][j]| == pow2(1)
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> |amp2[k][j]| == pow2(1)
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> |p2[k][j][l]| == n
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> |q2[k][j][l]| == n
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> |r2[k][j][l]| == 1
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> |u2[k][j][l]| == 1
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> castBVInt(q2[k][j][l]) == k
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> castBVInt(p2[k][j][l]) == j
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> castBVInt(u2[k][j][l]) == if j < N then 1 else 0
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> castBVInt(r2[k][j][l]) == if j < N then l else 0
    ensures forall k :: 0 <= k < pow2(n) ==> forall j :: 0 <= j < pow2(n) ==> forall l :: 0 <= l < pow2(1) ==> 
                  amp2[k][j][l] == if castBVInt(p2[k][j][l]) < N then 1.0/(pow2(n) as real) * 1.0/(sqrt(pow2(|r2[k][j][l]|) as real)) else 1.0/(pow2(n) as real)
{
    var u3 := norEn2(u, p);
    var r3 := norEn2(r, p);

    var u4 := [];
    var r4 := [];
    var p4 := [];
    var q4 := [];
    var amp4 := [];

    var tmp := 0;
    while (tmp < |p|)
        invariant 0 <= tmp <= |p|
        invariant |u4| == |r4| == |p4| == |q4| == |amp4| == tmp
        invariant forall k :: 0 <= k < |p4| ==> |p4[k]| == |q4[k]| == |r4[k]| == |u4[k]| == |amp4[k]| == pow2(n)
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> |p4[k][j]| == |q4[k][j]| == |r4[k][j]| == |u4[k][j]| == |amp4[k][j]| == pow2(1)
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> forall l :: 0 <= l < |p4[k][j]| ==> |p4[k][j][l]| == |q4[k][j][l]| == n
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> forall l :: 0 <= l < |p4[k][j]| ==> |u4[k][j][l]| == |r4[k][j][l]| == 1
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> forall l :: 0 <= l < |p4[k][j]| ==> castBVInt(p4[k][j][l]) == j
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> forall l :: 0 <= l < |p4[k][j]| ==> castBVInt(q4[k][j][l]) == k
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> forall l :: 0 <= l < |p4[k][j]| ==> castBVInt(u4[k][j][l]) == if castBVInt(p[k][j]) < N then 1 else 0
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> forall l :: 0 <= l < |p4[k][j]| ==> castBVInt(r4[k][j][l]) == if j < N then l else 0
        invariant forall k :: 0 <= k < |p4| ==> forall j :: 0 <= j < |p4[k]| ==> forall l :: 0 <= l < |p4[k][j]| ==> amp4[k][j][l] == if castBVInt(p[k][j]) < N then 1.0/(pow2(n) as real) * 1.0/(sqrt(pow2(|r4[k][j][l]|) as real)) else 1.0/(pow2(n) as real)
    {
        var tmpu := [];
        var tmpr := [];
        var tmpp := [];
        var tmpq := [];
        var tmpamp := [];

        var tmp1 := 0;
        while (tmp1 < |p[tmp]|)
            invariant 0 <= tmp1 <= |p[tmp]|
            invariant |tmpu| == |tmpr| == |tmpp| == |tmpq| == |tmpamp| == tmp1
            invariant forall k :: 0 <= k < tmp1 ==> |tmpu[k]| == |tmpr[k]| == |tmpp[k]| == |tmpq[k]| == |tmpamp[k]| == pow2(1)
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> |tmpp[k][j]| == n
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> |tmpq[k][j]| == n
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> |tmpr[k][j]| == 1
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> |tmpu[k][j]| == 1
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(tmpq[k][j]) == tmp
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(tmpp[k][j]) == k 
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(tmpr[k][j]) == if castBVInt(p[tmp][k]) < N then j else 0
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> castBVInt(tmpu[k][j]) == if castBVInt(p[tmp][k]) < N then 1 else 0
            invariant forall k :: 0 <= k < tmp1 ==> forall j :: 0 <= j < pow2(1) ==> tmpamp[k][j] == if castBVInt(p[tmp][k]) < N then  1.0/(pow2(n) as real) * 1.0/(sqrt(pow2(|tmpr[k][j]|) as real)) else 1.0/(pow2(n) as real)
        {
            var res := bool2BV1(castBVInt(p[tmp][tmp1]) < N);
            var tmmmu := duplicateSeq(res, pow2(|r3[tmp][tmp1]|));
            var tmmmp := duplicateSeq(p[tmp][tmp1], pow2(|r3[tmp][tmp1]|));
            var tmmmq := duplicateSeq(q[tmp][tmp1], pow2(|r3[tmp][tmp1]|));
            var tmmmr := duplicateSeq(r3[tmp][tmp1], pow2(|r3[tmp][tmp1]|));
            var tmmpamp := duplicateAmp(amp[tmp][tmp1], pow2(|r3[tmp][tmp1]|));
            if (castBVInt(p[tmp][tmp1]) < N){
                tmmmr := partialcastEn1toEn2(r3[tmp][tmp1]);
                tmmpamp := ampMul(tmmpamp, pow2(|r3[tmp][tmp1]|), r3[tmp][tmp1]);
            }
            omega0();
            tmpu := tmpu + [tmmmu];
            tmpp := tmpp + [tmmmp];
            tmpq := tmpq + [tmmmq];
            tmpr := tmpr + [tmmmr];
            tmpamp := tmpamp + [tmmpamp];
            tmp1 := tmp1 + 1;
        }

        u4 := u4 + [tmpu];
        r4 := r4 + [tmpr];
        p4 := p4 + [tmpp];
        q4 := q4 + [tmpq];
        amp4 := amp4 + [tmpamp];

        tmp := tmp + 1;
    }
    p2, q2, r2, u2, amp2 := p4, q4, r4, u4, amp4;

}