import opened QPrelude
import opened B2N
import opened Power2  
import opened Power
import opened DivMod

// Preparation + Setup
// Fix the constraint of [n >= 2]
method Shor1(a : nat, N : nat, n : nat) returns (x : qreg, y : qreg)
   requires N >= 2 && n >= 2
   ensures typeof(x) is (n) $ had { forall i | 0 <= i < n :: x[i] == 1 }
   ensures typeof(y) is exactly (n) $ ch(1) { y[0] == 1 }
   ensures fresh(x) && fresh(y)
{
   x := nor(n, _ => 0);
   y := nor(n, _ => 0);
   x *= H;
   LemmaB2NTailingZeros(y.m.b, 1);
   y *= NorToCH;
   y *= cl(a => a + 1);
}


method Shor2(a : nat, N : nat, n : nat, x : qreg, y : qreg) 
   requires N >= 2 && n >= 2
   requires typeof(x) is (n) $ had { forall i | 0 <= i < n :: x[i] == 1 }
   requires typeof(y) is exactly (n) $ ch(1) { y[0] == 1 }
   ensures typeof (y, x) is exactly (n, n) $ ch(Pow2(n)) {
     forall k | 0 <= k < Pow2(n) :: y[k] == Pow(a, k) % N
       &&
     forall k | 0 <= k < Pow2(n) :: x[k] == k
   }
   modifies x, y
{
   reveal Pow2(); 
   qfor (i := 0) in x becomes x' with y
     invariant i == x.card
     invariant typeof(x') is (n - i) $ had {
       forall k | 0 <= k < (n - i) :: x'[k] == 1
     }
     invariant typeof (y, x) is exactly (n, i) $ ch(Pow2(i)) {
       forall k | 0 <= k < Pow2(i) :: y[k] == Pow(a, k) % N
         &&
       forall k | 0 <= k < Pow2(i) :: x[k] == k
     }
   {
     y *= cl(v => (Pow(a, Pow2(i)) * v) % N);
   }
}


method ShorPreMeasurement(a : nat, N : nat, n : nat) returns (x : qreg, y : qreg)
   requires N >= 2 && n >= 2
   ensures typeof (y, x) is exactly (n, n) $ ch(Pow2(n)) {
     forall k | 0 <= k < Pow2(n) :: y[k] == Pow(a, k) % N
       &&
     forall k | 0 <= k < Pow2(n) :: x[k] == k
   }
   ensures fresh(x) && fresh(y)
{
   x, y := Shor1(a, N, n);
   Shor2(a, N, n, x, y);
}
 
method ShorPostMeasurement(a : nat, N : nat, n : nat, x : qreg, y : qreg)
  requires N >= 2 && n >= 2
  requires typeof (y, x) is exactly (n, n) $ ch(Pow2(n)) {
    forall k | 0 <= k < Pow2(n) :: y[k] == Pow(a, k) % N
      &&
    forall k | 0 <= k < Pow2(n) :: x[k] == k
  }
  ensures exists r : nat :: exists t : nat :: typeof (x) is exactly (n) $ ch(x.m.dof) {
    forall k | 0 <= k < x.m.dof :: exists m : nat :: x[k] == r * m + t
  }
  modifies x, y;
{
  var base, idxmap, m', k;
  base, idxmap, m', k := y.Measure();
  assert typeof (x) is exactly (n) $ ch(x.m.dof) {
    forall k | 0 <= k < x.m.dof <= Pow2(n) / N :: (Pow(a, x[k]) % N) == base
  };
}
