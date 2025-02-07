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
    'omega': 'function {:axiom} omega(n:nat, a:nat) : real',
    'sqrt': 'function {:axiom} sqrt(a:real) : real',
    'castBVInt': '''function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}''',
    'pow2': '''function pow2(N:nat) : int
  ensures pow2(N) > 0
{
  if (N==0) then 1 else 2 * pow2(N-1)
}''',
    'SqrtGt': Method('''lemma {:axiom} SqrtGt(a: real)
  requires a > 0.0
  ensures sqrt(a) > 0.0''', ['sqrt']),
      'hadNorHad': Method('''method hadNorHad(x: seq<bv1>) returns (y: seq<real>) 
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
            returns (y : seq<seq<bv1>>, ampy: seq<real>, py: seq<real>) 
  requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)
          
  ensures |y| == |ampy| == |py|
             
  ensures forall k :: 0 <= k < |y| ==> |y[k]| == |x|
  ensures |y| == pow2(|x|)
                             
  ensures forall k :: 0 <= k < |y| ==> castBVInt(y[k]) == k 
  ensures forall k :: 0 <= k < |ampy| ==> 
                            assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
                            ampy[k] == 1.0 / (sqrt(pow2(|x|) as real))
  ensures forall k :: 0 <= k < |py| ==> py[k] == omega(0,2)''', ['omega', 'pow2', 'castBVInt', 'SqrtGt', 'sqrt'])
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
