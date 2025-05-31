datatype Qubit = Qubit(index: nat)
datatype En2 = En2(k: nat, j: nat)
datatype En1 = En1(k: nat)

function method pow2(N:nat):int
  ensures pow2(N) > 0
{
	if (N==0) then 1 else 2 * pow2(N-1)
}

function method {:axiom} sqrt(a:real) :real 
    requires a > 0.0
    ensures sqrt(a) > 0.0

function method {:axiom} a(j: nat): real
function method {:axiom} ph(j: nat): real
function method {:axiom} omega(a:real, n:nat): real
function method {:axiom} cos(a:real): real

//WLOG we assume theta is in [0,pi/2]
function method {:axiom} sin(a:real): real
    ensures sin(a) != 0.0

// function method phi(n: nat): (En1 -> real)
//     requires n >= 0
//     ensures forall state :: phi(n)(state) == 
//              if 0 <= state.k < pow2(n)
//              then (1.0/sqrt(pow2(n) as real)) * a(state.k) 
//              else 0.0
//assuming that a(k) is the amplitude of the k-th state after applying unitary operation A
function method alpha(n: nat, f: nat -> bv1): (En1 -> real)
    requires n >= 0
    ensures forall state :: alpha(n, f)(state) == 
      if state.k < pow2(n) && f(state.k) == 1 then
        (1.0/sqrt(pow2(n) as real)) * a(state.k) 
      else if state.k < pow2(n) && f(state.k) == 0 then
        0.0
      else 
        0.0
        {
        (state: En1) =>  
          if state.k < pow2(n) && f(state.k) == 1 then
            (1.0/sqrt(pow2(n) as real)) * a(state.k)
          else if state.k < pow2(n) && f(state.k) == 0 then
            0.0
          else 
            0.0 
        }

function method beta(n: nat, f: nat -> bv1): (En1 -> real)
    requires n >= 0
    ensures forall state :: beta(n, f)(state) == 
      if state.k < pow2(n) && f(state.k) == 0 then
        (1.0/sqrt(pow2(n) as real)) * a(state.k) 
      else if state.k < pow2(n) && f(state.k) == 0 then
        0.0
      else 
        0.0

function method phi(n: nat, alpha: (En1 -> real), beta: (En1 -> real), theta: real): (En1 -> real)
    requires n >= 0
    ensures forall state :: phi(n, alpha, beta, theta)(state) == 
             if 0 <= state.k < pow2(n)
             then sin(theta)*alpha(state) + cos(theta)*beta(state)
             else 0.0



//properties of t unitary operation
function method VU(alpha: (En1 -> real), beta: (En1 -> real), theta: real, n: nat, t: nat): (En1 -> real)
    ensures forall state :: VU(alpha, beta, theta, n, t)(state) == 
      if state.k < pow2(n) then
        sin((2*t+1) as real *theta)*alpha(state) + cos((2*t+1) as real * theta)*beta(state)
      else
        0.0

//
method Grover(theta: real, n: nat, t: nat, f: nat -> bv1, alpha: En1 -> real, beta: En1 -> real) returns (out: En1 -> real)
    requires n >= 0
    requires theta >= 0.0// or VU_t(0, ...)
    {
        out := phi(n, alpha, beta, theta); // or phi_t(0, ...)
        var i := 1;

        while i < t
        {
            out := VU(alpha, beta, theta, n, i); // or VU_t(0, ...)
            i := i + 1;
        }

    }

    


