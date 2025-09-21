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
    'dotProd': '''function dotProd(x: seq<bv1>, y: seq<bv1>): nat
                requires |x| == |y|
                decreases |x|
              {
                if |x| == 0 then 0
                else ((bit(x[0]) * bit(y[0])) + dotProd(x[1..], y[1..]))
              }''',
    'sqrt': '''function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0''',
    'castBVInt': Method('''function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0
                ensures castBVInt(x) < pow2(|x|) ''', ['pow2']),
    'castIntBV': Method('''function {:axiom} castIntBV(x: nat, n: nat) : seq<bv1>
                ensures castBVInt(castIntBV(x, n)) == x
                ensures |castIntBV(x, n)| == n ''', ['castBVInt']),
    'pow2': '''function pow2(n:nat): nat
            ensures pow2(n) > 0
            {
              if n == 0 then 1
              else 2 * pow2(n-1)
            }''',
    'abs' : '''function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n''',

    'powN' : '''function {:axiom} powN(N:nat, k: nat) : int
                    ensures powN(N, k) > 0''',
    'powNTimesMod' : '''lemma {:axiom} powNTimesMod()
          ensures forall k: nat, j: nat, l : nat, N:nat {:trigger powN(k, j) * (powN(k, l) % N)}:: N > 0 ==> powN(k, j) * (powN(k, l) % N) % N == powN(k, j + l) % N''',

    'pow2mul' : Method('''lemma {:axiom} pow2mul()
                ensures forall k : nat, j : nat :: pow2(k) * pow2(j) == pow2(k + j)''', ['pow2']),

    'sqrtMul' : Method('''lemma {:axiom} sqrtmul()
                ensures forall k : real, j : real :: k > 0.0 && j > 0.0 ==> sqrt(k) * sqrt(j) == sqrt(k * j)''', ['sqrt']),
                
    'SqrtGt': Method('''lemma {:axiom} SqrtGt(a:real)
                requires a > 0.0
                ensures sqrt(a) > 0.0''', ['sqrt']),
    'hadNorHad': Method('''method hadNorHad(x:seq<bv1>) returns (y : seq<real>) 
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
            }''', ['omega']),
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

    'mergeBitEn' : Method('''method {:axiom} mergeBitEn(x: seq<seq<bv1>>, n : nat) returns (x1: seq<seq<bv1>>)
          requires forall k :: 0 <= k < |x| ==> |x[k]| == n
          ensures |x1| == |x| * 2
          ensures forall k :: 0 <= k < |x1| ==> |x1[k]| == n + 1
          ensures forall k :: 0 <= k < |x| ==> samebit(x[k], x1[k][0..n], n)
          ensures forall k :: |x| <= k < |x1| ==> samebit(x[k-|x|], x1[k][0..n], n) 
          ensures forall k :: 0 <= k < |x| ==> x1[k][n] == 0
          ensures forall k :: |x| <= k < |x1| ==> x1[k][n] == 1''', ['samebit']),

    'mergeAmpEn' : Method('''method {:axiom} mergeAmpEn(amp: seq<real>, q : real) returns (amp1: seq<real>)
          ensures |amp1| == |amp| * 2
          ensures forall k :: 0 <= k < |amp| ==> amp1[k] == 1.0 / sqrt(pow2(1) as real) * amp[k]
          ensures forall k :: |amp| <= k < |amp1| ==> amp1[k] == 1.0 / sqrt(pow2(1) as real) * amp[k-|amp|] * q''', ['sqrt']),

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
        ensures forall k :nat  :: sqrt(pow2(2 * k) as real) == pow2(k) as real''', ['sqrt', 'pow2'])
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