class DafnyLibrary:

  class Method:
    def __init__(self, code: str, dependencies: [str] = []):
      self._code = code
      self._dependencies = dependencies

    def code(self):
      return self._code

    def dependencies(self):
      return self._dependencies

  _methods = {
    'omega': 'function {:axiom} omega(n:nat, a:nat): real',
    'omega0' : '''lemma {:axiom} omega0()
                    ensures forall k : nat :: omega(0, k) == 1.0''',
    'sqrt': '''function {:axiom} sqrt(a:real): real
                requires a > 0.0
                ensures sqrt(a) > 0.0''',
    'castBVInt': '''function {:axiom} castBVInt(x : seq<bv1>) : nat
                ensures castBVInt(x) >= 0''',
    'pow2': '''function {:axiom} pow2(N:nat): int
                ensures pow2(N) > 0''',
    'abs' : '''function {:axiom} abs(n : int) : nat
                ensures abs(n) == if n >= 0 then n else -n''',

    'pow2Mul' : Method('''lemma {:axiom} pow2Mul()
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

    'partialcastEn1toEn2' : Method('''method {:axiom} method {:axiom} partialcastEn1toEn2(x: seq<bv1>) returns (x1 : seq<seq<bv1>>)
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
      ''', ['castBVInt', 'samebit'])
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
