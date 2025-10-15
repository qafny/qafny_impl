class DafnyLibrary:

  class Method:
    def __init__(self, code: str, dependencies: [str] = []):
      """Constructor for a template method. code is the actual Dafny code that makes up the method. dependencies is a list of method names that this method depends on."""
      self._code = code
      self._dependencies = dependencies

    def code(self) -> str:
      """The code as a string that defines and implements this method."""
      return self._code

    def dependencies(self) -> [str]:
      """The other method names used by this method as an array of strings."""
      return self._dependencies

  # internal static list of library methods
  _methods = {
    'omega': 'function {:axiom} omega(n:nat, a:nat): real',
    'omega0' : '''lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0''',
    'omega1' : '''lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == -1.0''',
    'dotProd': '''function dotProd(x: seq<bv1>, y: seq<bv1>): nat
                requires |x| == |y|
                decreases |x|
              {
                if |x| == 0 then 0
                else ((bit(x[0]) * bit(y[0])) + dotProd(x[1..], y[1..]))
              }''',
    'sqrt': '''function {:axiom} sqrt(a:real): real
                ensures a > 0.0 ==> sqrt(a) > 0.0
                ensures sqrt(a) * sqrt(a) == a''',
    'sqrt1': '''lemma {:axiom} sqrt1() 
                ensures sqrt(1.0) == 1.0''',
    'castBVInt':'''function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0
                ensures castBVInt(x) < pow2(|x|) ''', 
    # 'castBVInt': Method('''function castBVInt(x : seq<bv1>) : nat
    #             ensures castBVInt(x) >= 0
    #             ensures castBVInt(x) < pow2(|x|) 
    #             {
    #               if |x| == 0 then 0
    #               else bv1ToNat(x[0]) + 2 * castBVInt(x[1..])
    #             }''', ['pow2', 'bv1ToNat']),
    'bv1ToNat': '''function bv1ToNat(b: bv1): nat { if b == 1 then 1 else 0 }''',
    'castIntBV': Method('''function {:axiom} castIntBV(x: nat, n: nat) : seq<bv1>
                ensures castBVInt(castIntBV(x, n)) == x
                ensures |castIntBV(x, n)| == n ''', ['castBVInt']),
    'pow2': '''function pow2(n:nat): nat
            ensures pow2(n) > 0
            {
              if n == 0 then 1
              else 2 * pow2(n-1)
            }''',
    'abs' : '''function abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n
                {
                  if n >= 0 then n else -n
                }''',
    'pow': '''function {:axiom} pow(base: nat, k: nat): nat
              ensures k == 0 ==> pow(base, k) == 1
              ensures base == 0 && k > 0 ==> pow(base, k) == 0
              ensures base > 0 ==> pow(base, k) > 0''',
    'powN' : '''function {:axiom} powN(N:nat, k: nat) : int
                    ensures powN(N, k) > 0''',
    'powTimesMod' : Method('''lemma {:axiom} powTimesMod()
          ensures forall k: nat, j: nat, l : nat, N:nat {:trigger pow(k, j) * (pow(k, l) % N)}:: N > 0 ==> pow(k, j) * (pow(k, l) % N) % N == pow(k, j + l) % N''', ['pow']),

    'pow2mul' : Method('''lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)''', ['pow2']),

    'sqrtMul' : Method('''lemma {:axiom} sqrtmul()
                ensures forall k : real, j : real :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)''', ['sqrt']),
                
    'SqrtGt': Method('''lemma {:axiom} SqrtGt(a:real)
                requires a > 0.0
                ensures sqrt(a) > 0.0''', ['sqrt']),
   
    'countN': Method('''function countN(x: nat): nat
              ensures countN(x) <= x
            {
              if x == 0 then 0
              else (if x % 2 == 1 then 1 else 0) + countN(x / 2)
            }''', ['countNLEn']),
   
    'countStep': Method('''lemma {:axiom} countStep(i:nat)
              ensures forall k: nat :: k < pow2(i) ==> countN(k + pow2(i)) == countN(k) + 1''', ['pow2', 'countN']),
   
    'hadNorHad': Method('''method hadNorHad(x:seq<bv1>) returns (y : seq<real>) 
              ensures |y| == |x|
              ensures forall k :: 0 <= k < |x| ==> y[k] == omega(bv1ToNat(x[k]),2)
            {
              var i := 0;
              y := [];
              while i < |x| 
                invariant 0 <= i <= |x|
                invariant |y| == i
                invariant forall k :: 0 <= k < i ==> y[k] == omega(bv1ToNat(x[k]),2)
              {
                y := y + [omega(bv1ToNat(x[i]), 2)];
                i := i + 1;
              }
            }''', ['omega', 'bv1ToNat']),
    'hadEn': Method('''method {:axiom} hadEn(x: seq<real>)
            returns (amp: seq<real>, y : seq<seq<bv1>>) 
            requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)
                    
            ensures |y| == |amp|
                      
            ensures forall k :: 0 <= k < |y| ==> |y[k]| == |x|
            ensures |y| == pow2(|x|)
                                      
            ensures forall k :: 0 <= k < |y| ==> castBVInt(y[k]) == k 
            ensures forall k :: 0 <= k < |amp| ==> 
                                      assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
                                      amp[k] == 1.0 / (sqrt(pow2(|x|) as real))''', ['omega', 'pow2', 'castBVInt', 'SqrtGt', 'sqrt']),

    'partialcastEn1toEn2' : Method('''method {:axiom} partialcastEn1toEn2(x: seq<bv1>) returns (x1 : seq<seq<bv1>>)
          ensures |x1| == pow2(|x|)
          ensures forall k :: 0 <= k < pow2(|x|) ==> |x1[k]| == |x|
          ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
    ''', ['pow2', 'castBVInt', 'sqrt', 'omega']),

    'ampMul' : Method('''method {:axiom} ampMul(amp : seq<real>, x: nat, y : seq<bv1>) returns (amp1: seq<real>)
          requires x > 0
          ensures |amp| == |amp1|
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == (amp[k] * (1.0/sqrt(x as real))) * omega(castBVInt(y), 2)
                          ''', ['omega', 'castBVInt', 'sqrt']),
    'duplicateSeq' : Method('''
      method {:axiom} duplicateSeq(x: seq<bv1>, n:nat) returns (x1:seq<seq<bv1>>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
        ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x)
        ensures forall k :: 0 <= k < |x1| ==> samebit(x1[k], x, |x|)
    ''', ['castBVInt', 'samebit']),

    'duplicateAmp' : Method('''
    method {:axiom} duplicateAmp(x: real, n:nat) returns (x1:seq<real>)
        ensures |x1| == n
        ensures forall k :: 0 <= k < |x1| ==> x1[k] == x
    ''', []),

    'samebit' : '''
      function {:axiom} samebit(x: seq<bv1>, y: seq<bv1>, n :nat) : bool
          requires |x| >= n
          requires |y| >= n
          ensures samebit(x, y, n) == forall k :: 0 <= k < n ==> x[k] == y[k]
      ''',

    'hadNorEn' : Method('''
      method {:axiom} hadNorEn(x: seq<real>, y: seq<bv1>) returns (amp1: seq<real>, x1: seq<seq<bv1>>, y1: seq<seq<bv1>>)
        ensures |x1| == |y1| == |amp1| == pow2(|x|)
        ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k
        ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |x|
        ensures forall k :: 0 <= k < |y1| ==> y1[k] == y
        ensures forall k :: 0 <= k < |amp1| ==> amp1[k] == (1.0 / sqrt(pow2(|x|) as real))
        ''', ['pow2', 'castBVInt', 'sqrt']),

    'norEn2' : Method('''
        method {:axiom} norEn2(x: seq<bv1>, y: seq<seq<seq<bv1>>>)
        returns (x1: seq<seq<seq<bv1>>>)
          ensures |x1| == |y|
          ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |y[k]|
          ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| == |x1[k][j]| == |x|
          ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> castBVInt(x1[k][j]) == castBVInt(x)
          ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> samebit(x1[k][j], x, |x|)
      ''', ['castBVInt', 'samebit']),

    'mergeBitEn' : Method('''method  mergeBitEn(x: seq<seq<bv1>>, i : nat) returns (x1: seq<seq<bv1>>)
          requires |x| == pow2(i)
          requires forall k :: 0 <= k < |x| ==> |x[k]| == i
          requires forall k :: 0 <= k < |x| ==> castBVInt(x[k]) == k
          ensures |x1| == |x| * 2 
          ensures |x1| == pow2(i + 1)
          ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == i + 1
          ensures forall k :: 0 <= k < |x| ==> x1[k] == x[k] + [0]
          ensures forall k :: |x| <= k < |x1| ==> x1[k] == x[k - |x|] + [1]
          ensures forall k :: 0 <= k < |x| ==> castBVInt(x1[k]) == k
          ensures forall k :: |x| <= k < |x1| ==> castBVInt(x1[k]) == k 
          {
            var left  := seq(|x|,  k requires 0 <= k < |x|=> x[k] + [0]);
            var right := seq(|x|,  k requires 0 <= k < |x|=> x[k] + [1]);
            x1 := left + right;
            var k1 := 0;
            while k1 < |x|
                invariant 0 <= k1 <= |x|
                invariant forall k :: 0 <= k < k1 ==> castBVInt(x1[k]) == castBVInt(x[k])
            {
                castAppendBitValue(x[k1], 0); 
                k1 := k1 + 1;
            }

            var k2 := 0;
            while k2 < |x|
                invariant 0 <= k2 <= |x|
                invariant forall k::0 <= k < k2 ==> castBVInt(x1[|x| + k]) == castBVInt(x[k]) + pow2(i)
            {
                castAppendBitValue(x[k2], 1); 
                k2 := k2 + 1;
            }
          }''', ['castAppendBitValue']),

    'mergeBitEn2': Method('''method  mergeBitEn2(x: seq<seq<seq<bv1>>>, i: nat) returns (x1: seq<seq<seq<bv1>>>)
          requires |x| == pow2(i)
          requires forall r :: 0 <= r < |x| ==> forall k :: 0 <= k < |x[r]| ==> |x[r][k]| == i
          requires forall r :: 0 <= r < |x| ==> forall k :: 0 <= k < |x[r]| ==> castBVInt(x[r][k]) == r
          ensures |x1| == |x| * 2
          ensures |x1| == pow2(i + 1)
          ensures forall r :: 0 <= r < |x| ==> |x1[r]| == |x[r]|
          ensures forall r :: |x| <= r < |x1| ==> |x1[r]| == |x[r - |x|]|
          ensures forall r :: 0 <= r < |x1| ==> forall k :: 0 <= k < |x1[r]| ==> |x1[r][k]| == i + 1
          ensures forall r :: 0 <= r < |x| ==> forall k :: 0 <= k < |x[r]| ==> x1[r][k] == x[r][k] + [0]
          ensures forall r :: |x| <= r < |x1| ==> forall k :: 0 <= k < |x1[r]| ==> x1[r][k] == x[r-|x|][k] + [1]
          ensures forall r :: 0 <= r < |x| ==> forall k :: 0 <= k < |x[r]| ==> castBVInt(x1[r][k]) == r
          ensures forall r :: |x| <= r < |x1| ==> forall k :: 0 <= k < |x1[r]| ==> castBVInt(x1[r][k]) == r
          {
          var left  := seq(|x|, r requires 0 <= r < |x| =>
                          seq(|x[r]|, k requires 0 <= k < |x[r]| => x[r][k] + [0]));
          var right := seq(|x|, r requires 0 <= r < |x| =>
                          seq(|x[r]|, k requires 0 <= k < |x[r]| => x[r][k] + [1]));
          x1 := left + right;

          // Prove integer-value guarantees for left half
          var r := 0;
          while r < |x|
              invariant 0 <= r
              invariant r <= |x|
              invariant forall rr, k ::
              0 <= rr < r ==> 0 <= k < |x[rr]| ==> castBVInt(x1[rr][k]) == castBVInt(x[rr][k])
          {
              var k1 := 0;
              while k1 < |x[r]|
              invariant 0 <= k1
              invariant k1 <= |x[r]|
              invariant forall k :: 0 <= k < k1 ==> castBVInt(x1[r][k]) == castBVInt(x[r][k])
              {
              castAppendBitValue(x[r][k1], 0);
              k1 := k1 + 1;
              }
              r := r + 1;
          }

          // Prove integer-value guarantees for right half
          var r2 := 0;
          while r2 < |x|
              invariant 0 <= r2
              invariant r2 <= |x|
              invariant forall rr, k ::
              0 <= rr < r2 ==> 0 <= k < |x[rr]| ==>
                  castBVInt(x1[|x| + rr][k]) == castBVInt(x[rr][k]) + pow2(i)
          {
              var k2 := 0;
              while k2 < |x[r2]|
              invariant 0 <= k2
              invariant k2 <= |x[r2]|
              invariant forall k :: 0 <= k < k2 ==>
                  castBVInt(x1[|x| + r2][k]) == castBVInt(x[r2][k]) + pow2(i)
              {
              castAppendBitValue(x[r2][k2], 1);
              k2 := k2 + 1;
              }
              r2 := r2 + 1;
          }
          }''', ['castAppendBitValue']), 
    
    'castAppendBitValue': '''lemma {:axiom} castAppendBitValue(s: seq<bv1>, b: bv1)
            ensures castBVInt(s + [b]) == castBVInt(s) + if b == 1 then pow2(|s| as int) else 0''',
    
    'mergeAmpEn' : Method('''method mergeAmpEn(amp: seq<real>, q : real) returns (amp1: seq<real>)
          ensures |amp1| == |amp| * 2
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == invPow2(1) * amp[k]
          ensures forall k :: |amp| <= k < |amp1| ==> amp1[k] == invPow2(1) * amp[k-|amp|] * q
          {
            var left  := seq(|amp|, k requires 0 <= k < |amp|=> (invPow2(1)) * amp[k]);
            var right := seq(|amp|, k requires 0 <= k < |amp|=> (invPow2(1)) * amp[k] * q);
            amp1 := left + right;
          }''', ['sqrt', 'pow2']),

    'mergeAmpEn2':'''method mergeAmpEn2(amp: seq<seq<real>>, q: real) returns (amp1: seq<seq<real>>)
                ensures |amp1| == |amp| * 2
                ensures forall r :: 0 <= r < |amp| ==> |amp1[r]| == |amp[r]|
                ensures forall r :: |amp| <= r < |amp1| ==> |amp1[r]| == |amp[r - |amp|]|

                ensures forall r :: 0 <= r < |amp| ==> forall k :: 0 <= k < |amp[r]| ==> amp1[r][k] == invPow2(1) * amp[r][k]
                ensures forall r :: |amp| <= r < |amp1| ==> forall k ::0 <= k < |amp1[r]| ==> amp1[r][k] == invPow2(1) * amp[r-|amp|][k] * q
              {
                var s := invPow2(1);

                var left  := seq(|amp|, r requires 0 <= r < |amp| =>
                                seq(|amp[r]|, k requires 0 <= k < |amp[r]| => s * amp[r][k]));
                var right := seq(|amp|, r requires 0 <= r < |amp| =>
                                seq(|amp[r]|, k requires 0 <= k < |amp[r]| => s * amp[r][k] * q));
                amp1 := left + right;
              }''', 

    'triggerSqrtMul' : Method('''lemma {:axiom} triggerSqrtMul()
                      ensures forall k, j :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)''', ['sqrt']),

    'pow2add' : Method('''lemma {:axiom} pow2add()
                      ensures forall k : nat :: pow2(k) * 2 == pow2(k + 1)''', ['pow2']),

    'cutHad' : Method('''method {:axiom} cutHad(x: seq<real>) returns (x1: seq<real>)
          requires 0 < |x|
          ensures |x1| == |x| - 1
          ensures forall k :: 0 <= k < |x1| ==> x1[k] == x[k+1]''', []),

    'mergeBitTrigger' : Method('''lemma {:axiom} mergeBitTrigger(x: seq<seq<bv1>>, x1: seq<seq<bv1>>, n:nat)
          requires forall k :: 0 <= k < |x| ==> |x[k]| == n
          //requires forall k :: 0 <= k < |x| ==> castBVInt(x[k]) == k
          requires |x1| == |x| * 2
          requires forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
          requires forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k][0..n], n)
          requires forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k][0..n], n) 
          requires forall k :: 0 <= k < |x| ==> x1[k][n] == 0
          requires forall k :: |x| <= k < |x1| ==> x1[k][n] == 1
          ensures forall k :: 0 <= k < |x1| ==> castBVInt(x1[k]) == k''', ['castBVInt', 'samebit']),

    'duplicateMergeBitEn' : Method('''method {:axiom} duplicateMergeBitEn(x: seq<seq<bv1>>) returns (x1: seq<seq<bv1>>)
          ensures |x1| == |x| * 2
          ensures forall k :: 0 <= k < |x| ==> |x1[k]| == |x[k]|
          ensures forall k :: |x| <= k < |x1| ==> |x1[k]| == |x[k - |x|]|
          ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k], |x[k]|)
          ensures forall k :: 0 <= k < |x| ==> castBVInt(x1[k]) == castBVInt(x[k])
          ensures forall k :: |x| <= k < |x1| ==> samebit(x[k - |x|], x1[k], |x[k - |x|]|)
          ensures forall k :: |x| <= k < |x1| ==> castBVInt(x1[k]) == castBVInt(x[k - |x|])''', ['castBVInt', 'samebit']),
    'bool2BV1' : Method('''function {:axiom} bool2BV1(b : bool) : seq<bv1>
            ensures castBVInt(bool2BV1(b)) == if b then 1 else 0
            ensures |bool2BV1(b)| == 1''', ['castBVInt']),
    'ampeqtrigger' : Method('''lemma {:axiom} ampeqtrigger()
          ensures forall k : nat :: (sqrt(pow2(k) as real)) * (sqrt(pow2(k) as real)) == pow2(k) as real''', ['sqrt', 'pow2']),
    'En1toEn2_2' : Method('''method {:axiom} En1toEn2_2(x: seq<seq<bv1>>, y: seq<seq<bv1>>, amp: seq<real>)
        returns (x1: seq<seq<seq<bv1>>>, y1: seq<seq<seq<bv1>>>, amp1: seq<seq<real>>)
        requires |x| == |y| == |amp|
        ensures |x1| == |y1| == |amp1| == |x|
        ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |y1[k]| == |amp1[k]| == pow2(|y[k]|)
        ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==>|x1[k][j]| == |x[k]|
        ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |y1[k]| ==>|y1[k][j]| == |y[k]|
        ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> castBVInt(x1[k][j]) == castBVInt(x[k])
        ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |y1[k]| ==> castBVInt(y1[k][j]) == j
        ensures forall k :: 0 <= k < |amp1| ==> forall j :: 0 <= j < |amp1[k]| ==> amp1[k][j] == amp[k] * (1.0/sqrt(pow2(|y[k]|) as real)) * omega(j * castBVInt(y[k]), 2)
                ''', ['pow2', 'sqrt', 'omega', 'castBVInt']),

    'QFT_En1toEn2_2' : Method('''method {:axiom} QFT_En1toEn2_2(x: seq<seq<bv1>>, y: seq<seq<bv1>>, amp: seq<real>)
        returns (x1: seq<seq<seq<bv1>>>, y1: seq<seq<seq<bv1>>>, amp1: seq<seq<real>>)
        requires |x| == |y| == |amp|
        ensures |x1| == |y1| == |amp1| == |x|
        ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == |y1[k]| == |amp1[k]| == pow2(|y[k]|)
        ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==>|x1[k][j]| == |x[k]|
        ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |y1[k]| ==>|y1[k][j]| == |y[k]|
        ensures forall k :: 0 <= k < |x1| ==> forall j :: 0 <= j < |x1[k]| ==> castBVInt(x1[k][j]) == castBVInt(x[k])
        ensures forall k :: 0 <= k < |y1| ==> forall j :: 0 <= j < |y1[k]| ==> castBVInt(y1[k][j]) == j
        ensures forall k :: 0 <= k < |amp1| ==> forall j :: 0 <= j < |amp1[k]| ==> amp1[k][j] == amp[k] * (1.0/sqrt(pow2(|y[k]|) as real)) * omega(j * castBVInt(y[k]), pow2(|y[k]|))

    ''', ['pow2', 'castBVInt', 'omega', 'sqrt']),

    'pow2sqrt' : Method('''lemma {:axiom} pow2sqrt()
        ensures forall k :nat  :: sqrt(pow2(2 * k) as real) == pow2(k) as real''', ['sqrt', 'pow2']),
    
    'invPow2Step': Method('''lemma {:axiom}invPow2Step(i: nat)
        ensures invPow2(i) * invPow2(1) == invPow2(i + 1)''', ['invPow2']), 
    
    'invPow2': Method('''function invPow2(i: nat): real { 1.0 / sqrt(pow2(i) as real) }''', ['sqrt', 'pow2']),
    
    'powModLeft': '''lemma {:axiom} powModLeft()
  ensures forall k: nat, add: nat, j: nat, Y: nat, N: nat {:trigger pow(k, add) * ((pow(k, j) * Y) % N)} :: N > 0 ==> (pow(k, add) * ((pow(k, j) * Y) % N)) % N == ((pow(k, j + add) * Y) % N)''',
   
   'choose': '''function choose(n: nat, v: nat): nat
            {
              if v > n then 0
              else if v == 0 || v == n then 1
              else choose(n - 1, v - 1) + choose(n - 1, v)
            }''', 
   
   'measureEn1': '''
    method measureEn1(p: seq<seq<bv1>>) returns (v: nat)
        requires |p| > 0
        ensures exists k :: 0 <= k < |p| && v == castBVInt(p[k])
      {
        var k :| 0 <= k < |p|;
        v := castBVInt(p[k]);
      }''',
    
    'countNLEn':'''lemma {:axiom} countNLEn(n: nat, k: nat)
    ensures k < pow2(n) ==> countN(k) <= n''', 
  
  'hammingProjMass':'''lemma {:axiom} hammingProjMass(n: nat, v: nat, S: real)
  requires 0 <= v <= n
  ensures  S == (choose(n, v) as real) * (invPow2(n) * invPow2(n))
  ensures  sqrt(S) == sqrt(choose(n, v) as real) * invPow2(n)''',
  
  'projEn1': Method('''function projEn1(p: seq<seq<bv1>>, amp: seq<real>, v: nat): real
  requires |p| == |amp|
{
  projEn1From(p, amp, v, 0)
}''', ['projEn1From']), 

'projEn1From': '''function projEn1From(p: seq<seq<bv1>>, amp: seq<real>, v: nat, i: nat): real
  requires |p| == |amp|
  requires i <= |p|
  decreases |p| - i
{
  if i == |p| then 0.0
  else (if castBVInt(p[i]) == v then amp[i] * amp[i] else 0.0)
       + projEn1From(p, amp, v, i + 1)
}''', 

'projEn1LowerBound': '''lemma {:axiom} projEn1LowerBound(p: seq<seq<bv1>>, amp: seq<real>, s1: nat, w: nat)
  requires |p| == |amp|
  requires 0 <= w < |p|
  ensures  castBVInt(p[w]) == s1 ==> projEn1(p, amp, s1) >= amp[w]*amp[w]'''

  }
  
  @staticmethod
  def getMethod(name: str) -> str:
    """Returns the text associated with a method name. If a method with that name doesn't exist, it throws a KeyError"""
    method = DafnyLibrary._methods[name] 
    return method if isinstance(method, str) else method.code()

  @staticmethod
  def buildLibrary(methods: set[str]) -> str:
    """Returns a string representing the text associated with each method name in the methods set. If the method depends on other methods, this function will insert those other methods as well."""
    # pull in all dependent functions
    all_methods = set()
    while len(methods) > 0:
      next_methods = set()
      for method in methods:
        if method not in all_methods:
          # check for dependency methods
          method_info = DafnyLibrary._methods[method]

          if isinstance(method_info, DafnyLibrary.Method):
            next_methods.update(method_info.dependencies())

          # add the method to all_methods
          all_methods.add(method)
      methods = next_methods

    # create the library as a string
    library = ''
    for method in all_methods:
      library += DafnyLibrary.getMethod(method) + '\n\n'

    return library