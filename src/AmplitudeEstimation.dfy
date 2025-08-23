include "grovers.dfy"


// Define a datatype to hold measurement data. m is the number of grover operations,
// k is the number of success(f(x)==1), and N is the total number of measurements.
datatype MeasurementData = MD(m: int, k: int, n: nat)


function method {:axiom} sqrt(x: real): real

function method arcsin(x: real): real

method runGrovers(x: seq<bv1>, M: seq<int>, n: nat) returns (z: seq<seq<bv1>>, results: seq<MeasurementData>)
  // This function simulates the Grover's algorithm and returns the measurement data.
  // m is a sequence of integers representing the number of Grover operations,
  // and n is the total number of measurements
  requires |M| > 0
  requires |x| == n
  requires forall i :: 0 <= i < |x| ==> x[i] == 0
  requires forall i :: 0 <= i < |M| ==> 0 <= M[i] <= pow2(n)
  ensures |results| == |M|
  ensures forall i :: 0 <= i < |M| ==> results[i].m == M[i] && results[i].n == n
  ensures forall i :: 0 <= i < |M| ==> 0 <= results[i].k <= pow2(n)

{
  results := []; 
  z := [];
  var i := 0;
  while i < |M|
    invariant 0 <= i <= |M|
    invariant |results| == i
    invariant forall j :: 0 <= j < i ==> results[j].m == M[j] && results[j].n == n
    invariant forall j :: 0 <= j < i ==> 0 <= results[j].k <= pow2(n) 
    decreases |M| - i
  {   
    // call the Grover's algorithm with the given number of operations m[i] 
    var a: real, b: real, c: seq<seq<bv1>>, d: real, tmp: seq<seq<bv1>>;
    a, b, c, d, tmp := grovers(x, n, M[i] as nat);  //might need to change the uniform operator Hxn to general unitary A
    assert |tmp| <= pow2(n);
    // Append the measurement data with tmp as n
    results := results + [MD(M[i], |tmp|, n)];
    z := z + tmp;
    i := i + 1;
  }
}


// Define the likelihood function.
// For a given amplitude a (with 0 <= a <= 1) and measurement data,
// Likelihood(a, data) = products of [p_k(a)]^(n_k) * [1 - p_k(a)]^(N_k - n_k) for all k,
// where p_k(a) = sin^2((2*m_k + 1)*theta) and theta = arcsin(sqrt(a)).
function method likelihood(a: real, data: seq<MeasurementData>): real
  requires 0.0 <= a <= 1.0
  requires forall i :: 0 <= i < |data| ==> data[i].k >= 0
  requires forall i :: 0 <= i < |data| ==> 0 <= data[i].k <= pow2(data[i].n)
  
{
  if |data| == 0 then 1.0
  else
    var first := data[0];
    var theta := arcsin(sqrt(a));
    var p := sin((2*first.k + 1) as real * theta) * sin((2*first.k + 1) as real * theta);
    var term := Pow(p, first.k)* Pow((1.0 - p), (pow2(first.n) - first.k));
    term * likelihood(a, data[1..])
}


// Grid-search method to find the value of a in [0,1] that maximizes the likelihood.
method estimateamp(x: seq<bv1>, M: seq<int>, n: nat, step: real) returns (a_est: real)
  requires |M| > 0
  requires |x| == n
  requires forall i :: 0 <= i < |x| ==> x[i] == 0
  requires forall i :: 0 <= i < |M| ==> 0 <= M[i] <= pow2(n)
  requires forall i :: 0 <= i < |x| ==> x[i] == 0
  requires step > 0.0 && step <= 1.0
  ensures 0.0 <= a_est <= 1.0
{
  var _, data : seq<MeasurementData> := runGrovers(x, M, n);
 // data := runGrovers(x);
  var best_a := 0.0;
  var best_L := likelihood(0.0, data);
  var a := 0.0;
  // Compute the maximum number of steps as an integer
  var max_steps: int := (1.0 / step).Floor;
  var steps_remaining: nat := max_steps; // Ghost variable for termination

  // We'll use a simple loop to check candidate a values at intervals of 'step'.
  while a <= 1.0 && steps_remaining > 0
    invariant 0.0 <= a <= 1.0 + step
    invariant steps_remaining >= 0
  //  decreases 1.0 - a
    decreases steps_remaining // Use integer counter for termination
  {
    var current := likelihood(a, data);
    if current > best_L {
        best_L := current;
        best_a := a;
    }
    a := a + step;
    steps_remaining := steps_remaining - 1; // Strictly decreases
  }

  a_est := best_a;
}