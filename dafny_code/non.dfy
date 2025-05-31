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

function method phi(n: nat): (En2 -> real)
  requires n >= 0
  ensures forall state :: phi(n)(state) == 
           if 0 <= state.k < 2 && 0 <= state.j < pow2(n) 
           then (1.0/sqrt(2.0)) * a(state.j) 
           else 0.0
{
  (state: En2) =>
    if 0 <= state.k < 2 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j)
    else
      0.0
}

function method alpha(n: nat): (En2 -> real)
  requires n >= 0
  ensures forall state :: alpha(n)(state) == 
    if state.k == 0 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(ph(state.j), pow2(n))
    else if state.k == 1 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(-ph(state.j), pow2(n))
    else
      0.0
{
  (state: En2) =>
    if state.k == 0 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(ph(state.j), pow2(n))
    else if state.k == 1 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(-ph(state.j), pow2(n))
    else
      0.0
}

function method beta(n: nat): (En2 -> real)
  requires n >= 0
  ensures forall state :: beta(n)(state) == 
    if state.k == 1 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(-ph(state.j), pow2(n))
    else if state.k == 0 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(ph(state.j), pow2(n))
    else
      0.0
{
  (state: En2) =>
    if state.k == 1 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(-ph(state.j), pow2(n))
    else if state.k == 0 && 0 <= state.j < pow2(n) then
      (1.0/sqrt(2.0)) * a(state.j) * omega(ph(state.j), pow2(n))
    else
      0.0
}

//cos(theta) is the expected value of cos(ph(x))
//apply SU on phi -> phi and alpha
function method sfphiOp(theta: real, phi: (En2 -> real), alpha: (En2 -> real)): (En2 -> real) 
  ensures forall state: En2 :: 
      sfphiOp(theta, phi, alpha)(state) == 2.0 * cos(theta) * phi(state) - alpha(state)
  {
  (state: En2) => 2.0 * cos(theta) * phi(state) - alpha(state)
}

function method sfbetaOp(theta: real, phi: (En2 -> real)): (En2 -> real) 
  ensures forall state: En2 :: 
    sfbetaOp(theta, phi)(state) == phi(state){
  (state: En2) => phi(state)
}


function method sf_phiOp(theta: real, phi: (En2 -> real), beta: (En2 -> real)): (En2 -> real) 
  ensures forall state: En2 :: 
    sf_phiOp(theta, phi, beta)(state) == 2.0 * cos(theta) * phi(state) - beta(state){
  (state: En2) => 2.0 * cos(theta) * phi(state) - beta(state)
}


function method sf_alphaOp(phi: (En2 -> real), alpha: (En2 -> real)): (En2 -> real) 
  ensures forall state: En2 ::
    sf_alphaOp(phi, alpha)(state) == phi(state){
    (state: En2) => phi(state)
    }

function method phi_t(t: nat, theta: real, phi: (En2 -> real), alpha: (En2 -> real), beta: (En2 -> real)): (En2 -> real)
  requires theta > 0.0
{
  (state: En2) =>
    if t % 2 == 1 then 
        1.0/sin(theta) * (sin((t+1) as real * theta)*phi(state) - sin(t as real *theta) * alpha(state))
    else
        1.0/sin(theta) * (sin((t+1) as real * theta)*phi(state) - sin(t as real * theta) * beta(state))
}


method NonBoolean(phi: (En2 -> real), theta: real, K: nat, n: nat, alpha: (En2 -> real), beta: (En2 -> real)) returns (out: (En2 -> real)) 
  requires K > 0
  requires theta > 0.0
  ensures K%2 == 0 ==> out == phi_t(K, theta, phi, alpha, beta)
  ensures K%2 == 1 ==> out == phi_t(K, theta, phi, beta, alpha)
{
  out := phi; // or phi_t(0, ...)
  var k := 1;

  while k < K
    // invariant 0 <= k <= K
    // invariant k > 0 ==> (
    //   (k-1)%2 == 0 ==> out == phi_t(k-1, theta, phi, alpha, beta) &&
    //   (k-1)%2 == 1 ==> out == phi_t(k-1, theta, phi, beta, alpha)
    // )
  {
    if (k % 2 == 0) {
      out := phi_t(k, theta, phi, alpha, beta);
    } else {
      out := phi_t(k, theta, phi, beta, alpha);
    }
    k := k + 1;
  }

  // At loop exit, k == K
  // So out == phi_t(K-1, ...)
  // One more update needed:
  if K % 2 == 0 {
    out := phi_t(K, theta, phi, alpha, beta);
  } else {
    out := phi_t(K, theta, phi, beta, alpha);
  }
}