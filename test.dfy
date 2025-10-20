lemma {:axiom} triggerSqrtMul()
                      ensures forall k, j :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)


function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
    requires |x| >= n
    requires |y| >= n
    ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]
      

function {:axiom} powN(N:nat, k: nat) : int
                    ensures powN(N, k) > 0

function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n

method {:axiom} cutHad(x: seq<real>) returns (x1: seq<real>)
          requires 0 < |x|
          ensures |x1| == |x| - 1
          ensures forall k :: 0 <= k < |x1| ==> x1[k] == x[k+1]

method  mergeBitEn(x: seq<seq<bv1>>, i : nat) returns (x1: seq<seq<bv1>>)
          requires |x| == pow2(i)
          requires forall k :: 0 <= k < |x| ==> |x[k]| == i
    //      requires |x| == pow2(n)
          ensures |x1| == |x| * 2 
          ensures |x1| == pow2(i + 1)
          ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == i + 1
          ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k], i)
          ensures forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k], i) 
          ensures forall k :: 0 <= k < |x| ==> x1[k][i] == 0
          ensures forall k :: |x| <= k < |x1| ==> x1[k][i] == 1
          {
            var left  := seq(|x|,  k requires 0 <= k < |x|=> x[k] + [0]);
            var right := seq(|x|,  k requires 0 <= k < |x|=> x[k] + [1]);
            x1 := left + right;
     //       pow2add();
          }

function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0
                ensures sqrt(a) * sqrt(a) == a


method mergeAmpEn(amp: seq<real>, q : real) returns (amp1: seq<real>)
          ensures |amp1| == |amp| * 2
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == 1.0 / sqrt(pow2(1) as real) * amp[k]
          ensures forall k :: |amp| <= k < |amp1| ==> amp1[k] == 1.0 / sqrt(pow2(1) as real) * amp[k-|amp|] * q
          {
            var left  := seq(|amp|, k requires 0 <= k < |amp|=> (1.0 / sqrt(pow2(1) as real)) * amp[k]);
            var right := seq(|amp|, k requires 0 <= k < |amp|=> (1.0 / sqrt(pow2(1) as real)) * amp[k] * q);
            amp1 := left + right;
          }

function {:axiom} omega(a:nat, n:nat): real

lemma {:axiom} mergeBitTrigger(x: seq<seq<bv1>>, x1: seq<seq<bv1>>, i:nat)
          requires forall k :: 0 <= k < |x| ==> |x[k]| == i
          //requires forall k :: 0 <= k < |x| ==> castBVInt(x[k]) == k
          requires |x1| == |x| * 2 
          requires forall k :: 0 <= k < |x1| ==> |x1[k]| == i + 1
  //        requires forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k], i)
  //        requires forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k], i) 
          requires forall k :: 0 <= k < |x| ==> x1[k][i] == 0
          requires forall k :: |x| <= k < |x1| ==> x1[k][i] == 1
          ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k

// function {:axiom} castBVInt(x : seq<bv1>) : nat
//                 ensures castBVInt(x) >= 0
//                 ensures castBVInt(x) < pow2(|x|) 

function bv1ToNat(b: bv1): nat { if b == 1 then 1 else 0 }

function castBVInt(x: seq<bv1>): nat
      ensures castBVInt(x) >= 0
      ensures castBVInt(x) < pow2(|x|)
      decreases x
    {
      if |x| == 0 then 0
      else bv1ToNat(x[0]) + 2 * castBVInt(x[1..])
    }

function {:axiom} castIntBV(x: nat, n: nat) : seq<bv1>
 //               requires x < pow2(n)
                ensures castBVInt(castIntBV(x, n)) == x
                ensures |castIntBV(x, n)| == n


method duplicateMergeBitEn(x: seq<seq<bv1>>) returns (x1: seq<seq<bv1>>)
          ensures |x1| == |x| * 2
          ensures forall k :: 0 <= k < |x| ==> |x1[k]| == |x[k]|
          ensures forall k :: |x| <= k < |x1| ==> |x1[k]| == |x[k - |x|]|
          ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k], |x[k]|)
          ensures forall k :: 0 <= k < |x| ==> castBVInt(x1[k]) == castBVInt(x[k])
          ensures forall k :: |x| <= k < |x1| ==> samebit(x[k - |x|], x1[k], |x[k - |x|]|)
          ensures forall k :: |x| <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x[k - |x|])
          {
            x1 := x + x;
          }

function pow2(n:nat): nat
            ensures pow2(n) > 0
            {
              if n == 0 then 1
              else 2 * pow2(n-1)
            }

lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0

lemma {:axiom} pow2add()
                      ensures forall k : nat :: pow2(k) * 2 == pow2(k + 1)

lemma {:axiom} powNTimesMod()
          ensures forall k: nat, j: nat, l : nat, N:nat {:trigger powN(k, j) * (powN(k, l) % N)}:: N > 0 ==> powN(k, j) * (powN(k, l) % N) % N == powN(k, j + l) % N

lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)

lemma {:axiom} sqrt1() 
    ensures sqrt(1.0) == 1.0

method hadNorHad(x:seq<bv1>) returns (y : seq<real>) 
              ensures |y| == |x|
              ensures forall k :: 0 <= k < |x| ==> y[k] == omega(if x[k] == 1 then 1 else 0, 2)
            {
              var i := 0;
              y := [];
              while i < |x| 
                invariant 0 <= i <= |x|
                invariant |y| == i
                invariant forall k :: 0 <= k < i ==> y[k] == omega(if x[k] == 1 then 1 else 0, 2)
              {
                y := y + [omega(if x[i] == 1 then 1 else 0, 2)];
                i := i + 1;
              }
            }

 method Shors(n: nat, p: seq<bv1>, q: seq<bv1>, base: nat, N: nat) returns (p1: seq<seq<bv1>>, q1: seq<seq<bv1>>, amp1: seq<real>)
  requires |q| == n
  requires n > 0
  requires N > 0
  requires forall i :: 0 <= i < |q| ==> q[i] == 0
  requires castBVInt(q) == 0
  requires |p| == n
  requires forall i :: 0 <= i < |p| ==> p[i] == 0
  requires castBVInt(p) == 0
//  ensures |p1| == pow2(n)
//  ensures |q1| == pow2(n)
//  ensures |amp1| == pow2(n)
//  ensures forall k :: 0 <= k < |q1| ==> |q1[k]| == n
 // ensures forall k :: 0 <= k < |p1| ==> |p1[k]| == n 
//  ensures forall k :: 0 <= k < |amp1| ==> amp1[k] == 1.0 / sqrt(pow2(n) as real)
//  ensures forall k :: 0 <= k < |q1| ==> castBVInt(q1[k]) == k
//  ensures forall k :: 0 <= k < |p1| ==> castBVInt(p1[k]) == (powN(base, k) % N)
{
  var i := 0;
  var q2 := hadNorHad(q);  
  //base case with i == 0
  var q3 := seq(pow2(i), _ => castIntBV(i, i));
  var p3 := seq(pow2(i), _ => castIntBV(powN(base, i) % N, n));
  var amp3 := seq(pow2(i), _ => 1.0/ sqrt(pow2(i) as real));
  //sqrt1(); 
  //while i < n
   // invariant |q3| == pow2(i)
  //  invariant |p3| == pow2(i)
   // invariant |amp3| == pow2(i)
   // invariant |q2| == n - i
   // invariant forall k :: 0 <= k < |q2| ==> q2[k] == omega(0, 2)
   // invariant forall k :: 0 <= k < |q3| ==> |q3[k]| == i
   // invariant forall k :: 0 <= k < |p3| ==> |p3[k]| == n
   // invariant forall k :: 0 <= k < |amp3| ==>  amp3[k] == (1.0 / sqrt(pow2(i) as real))
   // invariant forall k :: 0 <= k < |q3| ==> castBVInt(q3[k]) == k
  //  invariant forall k :: 0 <= k < |p3| ==> castBVInt(p3[k]) == (powN(base, k) % N)
  {
   
    var q5 := q2[0];
    var q4 := cutHad(q2);
    var q6 := mergeBitEn(q3, i);
    var p6 := duplicateMergeBitEn(p3);
    var amp6 := mergeAmpEn(amp3, q5);
//    assert forall k :: 0 <= k < |p3| ==> castBVInt(p3[k]) == (powN(base, k) % N);
    
   // omega0();
   // mergeBitTrigger(q3, q6, |q3[0]|);
   // triggerSqrtMul();
    //pow2mul();

   // var p7 := seq(|p6|,
    //  k0 requires 0 <= k0 < |p6| =>
    //    if castBVInt(q6[k0]) >= pow2(i) then
    //     castIntBV(((powN(base, pow2(i)) * castBVInt(p6[k0])) % N), |p6[k0]|) 
    //     else p6[k0]
    //);

    //q3 := q6;
    //p3 := p7;
   // amp3 := amp6;
   // q2 := q4;
    i := i + 1;
  }
  
  q1 := q3;
  p1 := p3;
  amp1 := amp3;
}