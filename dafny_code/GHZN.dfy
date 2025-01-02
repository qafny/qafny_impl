import opened QPrelude
import opened Power2

method GHZ(x : qreg, n : nat) returns (qs : set<qreg>, q : qreg)
  requires n >= 3
  requires typeof (x) is (n) $ nor { forall i | 0 <= i < n :: x[i] == 0 }
  ensures fresh(qs) && |qs| == n
  ensures typeof qs with q is 1 $ ch(2) { forall p | p in qs :: q.GetSession()[p] == [0, 1] }
  modifies x
{
  q := x.SplitNor();
  q *= H;
  q *= PlusToCH;
  assert typeof (q) is exactly (1) $ ch(2) { q.GetSession()[q] == [0, 1] }
  by { reveal Pow2(); }         // important to discharge [PlusToCH]
  qs, q := GHZInnerN(x, n - 1, q);
}

method GHZInnerN(x : qreg, n : nat, q : qreg)
  returns (qs : set<qreg>, q' : qreg)
  requires n >= 2
  requires typeof (x) is (n) $ nor { forall i | 0 <= i < n :: x[i] == 0 }
  requires typeof (q) is exactly (1) $ ch(2) { q.GetSession()[q] == [0, 1] }
  ensures  fresh(qs - { q })
  ensures  |qs| == (1 + n)
  ensures  typeof qs with q' is 1 $ ch(2) { forall p | p in qs :: q'.GetSession()[p] == [0, 1] }
  modifies x, q
{
  qs := { q };
  q' := q;
  while (x.card > 0)
    decreases x.card
    invariant  |qs| == 1 + n - x.card
    invariant fresh(qs - { q })
    invariant typeof (x) is (x.card) $ nor { forall i | 0 <= i < x.card :: x[i] == 0 }
    invariant typeof qs with q' is 1 $ ch(2) { forall p | p in qs :: q'.GetSession()[p] == [0, 1] }
  {
    var p := x.SplitNor();
    p.JoinSessionNor1(q');
    // from 1th position, it's the controlled qubit
    var stashed := q'.PartitionLeft(1, 1); 
    {
      p *= cl(a => a + 1);     // X 
    }
    q'.MergeLeft(stashed);
    qs := qs + { p };
    q' := p;
  }
}
