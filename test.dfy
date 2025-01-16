predicate boundedSame (x : seq<bv1>, y : seq<bv1> , n:nat) 
  requires n <= |x|
  requires n <= |y|
{
  forall k :: 0 <= k < n ==> x[k] == y[k]
}

function {:axiom} omega(n:nat, a:nat): real

function {:axiom} sqrt(a:real): real

lemma {:axiom} SqrtGt(a:real)
  requires a > 0.0
  ensures sqrt(a) > 0.0

lemma {:axiom} omegaplus(a:nat, b:nat, c:nat)
  ensures omega(b+c,a) == omega(b,a) * omega(c,a)
  
method hadH(x:bv1) returns (y : int) 
  ensures y == x as int
{
  y := x as int;
}

function castBVInt (x:seq<bv1>) : nat
{
  if (|x|==0) then 0 else (x[0] as nat) + 2 * castBVInt(x[1..])
}

function b2n (x:seq<bv1>, i:nat) : nat
  requires i <= |x|
{
  if (i==0) then 0 else (x[i-1] as nat) * pow2(i-1) + b2n(x[..i-1],i-1)
}


method {:axiom} n2b(x:nat, n:nat) returns (y:seq<bv1>)
    ensures |y| == n
    ensures castBVInt(y) == x

function pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function Pow(b: int, e: nat): int
    decreases e
  {
    if e == 0 then
      1
    else
      b * Pow(b, e - 1)
  }

//apply H to a Nor typed array x, to a Had type
// x[0,n) <- H, detect the type of x[0,n): nor
//call the function hadNorHad: y
// y' := hadNorHad(x)
//after the above line, we have the following constarints
// 1. |y'| == |x|, 
// 2. forall k :: 0 <= k < |x| ==> y'[k] == omega(x[k] as int,2)
method hadNorHad(x:seq<bv1>) returns (y : seq<real>) 
  ensures |y| == |x|
  ensures forall k :: 0 <= k < |x| ==> y[k] == omega(x[k] as int,2)
{//for validating them in Dafny
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

// fills a sequence of length n with a sequence of bv1's that (when combined) equal k.
// see: copy(k, n)
method b2ns (k:nat, n:nat) returns (x: seq<seq<bv1>>)
   ensures |x| == n
   ensures forall j :: 0 <= j < n ==> castBVInt(x[j]) == k
{
  x := [];
  var i := 0;
  while i < n
    invariant 0 <= i <= n
    invariant |x| == i
    invariant forall j :: 0 <= j < i ==> castBVInt(x[j]) == k
  {
    var v := n2b(k,n);
    x := x + [v];
    i := i + 1;
  }
}

// makes a n-length sequence of real nunmbers that are all equal to k.
// a more general form of zeroes(n) (i.e. copy(0, n))
method copy (k:real, n:nat) returns (x:seq<real>)
   ensures |x| == n
   ensures forall j :: 0 <= j < n ==> x[j] == k
{
  x := [];
  var i := 0;
  while i < n
    invariant 0 <= i <= n
    invariant |x| == i
    invariant forall j :: 0 <= j < i ==> x[j] == k
  {
    x := x + [k];
    i := i + 1;
  }
}

//apply had typed to en(1) typed state
//x[0,n) <- H;
//now the following is a function call for had to en(1)
// x has type had
//in general we are dealing with en(t)
//if x has en(2), we need to add seq as x:seq<seq<seq<bv1>>>
//In Dafny, seq<?> is a sequence of ?
method {:axiom} hadEn(x: seq<real>)
            returns (y : seq<seq<bv1>>, ampy: seq<real>, py: seq<real>) 
  requires forall k :: 0 <= k < |x| ==> x[k] == omega(0,2)
  ensures |y| == |ampy| == |py|
  ensures forall k :: 0 <= k < |y| ==> |y[k]| == |x|
  ensures |y| == pow2(|x|)
  ensures forall k :: 0 <= k < |y| ==> castBVInt(y[k]) == k 
  ensures forall k :: 0 <= k < |ampy| ==> 
                            assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
                            ampy[k] == 1.0 / (sqrt(pow2(|x|) as real))
  ensures forall k :: 0 <= k < |py| ==> py[k] == omega(0,2)
/*
{
  var i := 0;
  y := [];
  ampy := [];
  py := [];
  while i < |x|
    // bound the range of i.
    invariant 0 <= i <= |x|
    
    // the length of the returned sequences should be exactly equal to the current iteration number
    invariant |y| == i
    invariant |ampy| == i
    invariant |py| == i

    // the length of the sub sequences that have been created should be equal to the length of x
    invariant forall k :: 0 <= k < i ==> |y[k]| == |x|
    invariant forall k :: 0 <= k < i ==> |ampy[k]| == |amp|
    invariant forall k :: 0 <= k < i ==> |py[k]| == |phase|

    // constrain the values of the basis kets
    invariant forall k :: 0 <= k < i ==> forall j :: 0 <= j < |y[k]| ==> castBVInt(y[k][j]) == k
    // constrain the values of the amplitudes
    invariant forall k :: 0 <= k < i ==> forall j :: 0 <= j < |ampy[k]| ==> 
                                    assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
                                    ampy[k][j] == 1.0 / (sqrt(pow2(|x|) as real)) * amp[j]
    // constrain the values of the phases
    invariant forall k :: 0 <= k < i ==> forall j :: 0 <= j < |py[k]| ==> py[k][j] == omega(castBVInt(x[j]) * k, 2) * phase[j]
  {
    // add the next basis ket
    var basis_kets := b2ns(i, |x|); // seq<seq<bv1>>
    y := y + [basis_kets];
    
    // add the next amplitude
    var amplitudes := []; // copy(amplitude, |amp|); dont copy
    var j := 0;
    while j < |amp| 
      // bound the range of j
      invariant 0 <= j <= |amp|
      invariant j == |amplitudes|
      invariant forall k :: 0 <= k < j ==> 
        assert sqrt(pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
        amplitudes[k] == (1.0 / (sqrt(pow2(|x|) as real)) * amp[k])
    {
      // prove that computing the amplitude (next statement) won't result in a divide by zero error
      assert sqrt (pow2(|x|) as real) > 0.0 by {SqrtGt(pow2(|x|) as real);}
      amplitudes := amplitudes + [1.0 / (sqrt(pow2(|x|) as real)) * amp[j]];
      j := j + 1;
    }

    ampy := ampy + [amplitudes];

    // add the next phase
    var yphases := [];
    // reset our iterator
    j := 0;
    while j < |phase| 
      // bound the range of j
      invariant 0 <= j <= |phase|
      invariant j == |yphases|
      invariant forall k :: 0 <= k < j ==> yphases[k] == omega(castBVInt(x[k]) * i, 2) * phase[k]
    {
      yphases := yphases + [omega(castBVInt(x[j]) * i, 2) * phase[j]];
      j := j + 1;
    }

    py := py + [yphases];

    i := i + 1;
  }
}
*/

//maybe we should break had compilation into smaller steps
//One operation that is very useful is the en type casting
//we need to cast en(t) to en(t+1) type
//below is an example to cast en(1) to en(2)
//for en(t), one just need to add seq< ... seq< ...  before the type
method castBaseEn(x: seq<seq<bv1>>) returns (y : seq<seq<seq<bv1>>>)
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> y[k] == x 
{
  y := [];
  var i := 0;
  while(i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall j :: 0 <= j < i ==> y[j] == x
  {
    y := y + [x];
    i := i + 1;
  }
}

function sumFun(f: nat -> bv1, n:nat) : nat
{
  if n == 0 then 0 else f(n) as nat + sumFun(f,n-1)
}

lemma {:axioms} constantFun(f: nat -> bv1, n:nat)
  ensures forall k :: 0 <= k <  pow2(n) ==> f(k) == 0

lemma {:axioms} balanceFun(f: nat -> bv1, n:nat)
  requires 0 < n
  ensures forall k :: 0 <= k < pow2(n-1) ==> f(k) == 0
  ensures forall k :: pow2(n-1) <= k < pow2(n) ==> f(k) == 1

//cast en(t) to en(t+1) function for amplitude and phase.
//these two should be the same
method castBaseAmp(x: seq<real>) returns (y : seq<seq<real>>)
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> y[k] == x 
{
  y := [];
  var i := 0;
  while(i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall j :: 0 <= j < i ==> y[j] == x
  {
    y := y + [x];
    i := i + 1;
  }
}

//cast had state to en(1) function for phase
method castHadEnPhase(x: seq<real>) returns (y : seq<seq<real>>)
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> y[k] == x
{
  y := [];
  var i := 0;
  while(i < |x|)
    invariant 0 <= i <= |x|
    invariant |y| == i
    invariant forall j :: 0 <= j < i ==> y[j] == x
  {
    y := y + [x];
    i := i + 1;
  }
}


//oracleFun is a template
// x[0,n) <- lambda(y => |f(y)>), e.g., f could be a * y %N. where a and N are constants
//f can be arbitary
function {:axiom} oracleFun (x:nat) : nat
  //ensures ... fill this out to be the spec indicated in a lambda term
  //for example, if we have lambda(x => |x + n>), the 
  //the ensures can be ensures oracleFun(x) == x + n

//when x is of type Nor
method {:axiom} lambdaBaseNor(x: seq<bv1>) returns (y : seq<bv1>) 
  ensures |x| == |y|
  ensures castBVInt(x) == oracleFun(castBVInt(y))
  //another way to deal with this is to just modify the left hand side above

//the following is an template for en(1)
//for en(t), one can generate the template accordingly
//the function take en(1) and outputs en(1)
/// x[0,n) <- lambda(y => |f(y)>)
//// x[0,n),z[0,m) <- lambda(y,u => |f(y)>|g(u)>)
// have oraclef, and oracleg
// call the below once for x --> after this call, f(y) is stored in some new var 
// x' ==> y' := lambdaBaseEn(x') with oracleFun ==> oraclef
//call it another time for z
// z' ==> u' := lambdaBaseEn(z') with oracleFun ==> oracleg
method lambdaBaseEn(x: seq<seq<bv1>>) returns (y : seq<seq<bv1>>) 
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> castBVInt(x[k]) == oracleFun(castBVInt(y[k]))
{
  y := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant i == |y|
    invariant forall j :: 0 <= j < i ==> castBVInt(x[j]) == oracleFun(castBVInt(y[j]))
  {
    var t := lambdaBaseNor(x[i]);
    y := y + [t];
    i := i + 1;
  }
}

function {:axiom} oraclePhase (x:seq<bv1>) : real
  //ensures ... fill this out to be the spec indicated in a lambda term
  //for example, if we have lambda(x => phase(x) |x>), the 
  //the ensures can be ensures oraclePhase(x) == phase(x)
  //phase is always in the form of omega(a, b)

//x[0,n) <- lambda(y => g(x1) ? |x1>), where g(x1) == omega(g'(x1), 2)
// x[0,n) into amp, phase, basis.
//x': seq<real> is the original phase array for x
//the above expression means to take each element in x',
//and multiply x'[j] with g(x1)[j]
//the following is a template function for adding phase in lambda expr
// this assumes the phase(x1) function only talks about a single range
//which is the most common case, but the phase(x1) function could be something like
//phase(x1,x2,...), in this case, we need to put the variable x1, x2, ... in the arguments
//each of the x1, x2, ... have type seq<seq<bv1>>, this is assumed that
//their types are en(1), if dealing with en(t) type case, we also need to modify the type
method lambdaPhaseEn(x: seq<real>, x1 : seq<seq<bv1>>) returns (y : seq<real>) 
  requires |x| == |x1| 
  ensures |x| == |y|
  ensures forall k :: 0 <= k < |y| ==> y[k] == x[k] * oraclePhase(x1[k])
{//the body is not necessary, but it is for ensuring the requires and ensures are ok.
  y := [];
  var i := 0;
  while (i < |x|)
    invariant 0 <= i <= |x|
    invariant |x| == |x1|
    invariant i == |y|
    invariant forall j :: 0 <= j < i ==> y[j] == x[j] * oraclePhase(x1[j])
  {
    var t := x[i] * oraclePhase(x1[i]);
    y := y + [t];
    i := i + 1;
  }
}

// modular multiplication lambda
function {:axiom} multmod(x: seq<bv1>, a: nat, n: nat) :seq<bv1>
  requires 1 < a < n
  ensures |multmod(x,a,n)| == |x| 
  ensures b2n(multmod(x,a,n), |x|) == a * b2n(x, |x|) % n


  lemma {:axiom} b2nEqSeq(a:nat, N:nat, i:nat, x : seq<seq<bv1>>, y: seq<seq<bv1>>, n:nat, ni:nat)
    requires i < |y|
    requires |y| <= |x| 
    requires 1 < a < N
    requires |y[i]| == n
    requires |x[i]| == ni + 1
    ensures  b2n(y[i], n) == Pow(a, b2n(x[i], ni + 1)) % N

// quantum conditionals require different strategies depending on the body of the conditional 
//quantum conditional for en(1)
//this is a template for mod-mult
// controls on the last bit of z0, apply modular multiplication on z1
method ctrU(z0:seq<seq<bv1>>,z1:seq<seq<bv1>>,ni: nat , n:nat, a : nat,  N : nat)
  returns (u1:seq<seq<bv1>>) //template for modification. clearly, z0 is not modified,
                             //so the compiler needs to determine which seq is modified or not
                             //and then put the modification one as returns
  requires 1 < a < N
  requires 0 < n
  requires |z0| == |z1|
  requires forall k :: 0 <= k < |z0| ==> |z0[k]| == ni + 1
  requires forall k :: 0 <= k < |z1| ==> |z1[k]| == n
  //the above requirements should be universally true for every quantum conditional
  //the below one is specific to a special conditional, here, it is used in mod-mult circuit
  requires forall k :: 0 <= k < |z1| ==> b2n(z1[k], n) == Pow(a, b2n(z0[k], ni)) % N
  ensures |u1| == |z0|
  ensures forall k :: 0 <= k < |u1| ==> |u1[k]| == n
  //the above ensures should be universally true for every quantum conditional
  //the below one is specific to a special conditional, here, it is used in mod-mult circuit
  ensures forall k :: 0 <= k < |u1| ==> b2n(u1[k], n) == Pow(a, b2n(z0[k], ni + 1)) % N
{
  u1 := [];
  var i := 0;
  while i < |z0|
    invariant i <= |z0|
    invariant i == |u1|
    invariant forall k :: 0 <= k < i ==> |u1[k]| == n
    invariant forall k :: 0 <= k < i ==> b2n(u1[k], n) == Pow(a, b2n(z0[k], ni + 1)) % N
  {
    if z0[i][ni] == 1 {
      u1 := u1 + [multmod(z1[i], a, N)];      
    }
    else {
      u1 := u1 + [z1[i]];
    }
    assert |u1[i]| == n;
    //should be part of a template and it requires user input below.
    //if no input, then we should have a general pattern.
    assert (b2n(u1[i], n) == Pow(a, b2n(z0[i], ni + 1)) % N) by {b2nEqSeq(a,N,i,z0,u1,n,ni); } // b2nEqSeq is generated by the compiler per the user's input
    i := i+1;
  }
}
