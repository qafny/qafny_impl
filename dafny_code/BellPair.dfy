// include "./QPrelude.dfy"
// import opened QPrelude

/* # Translating Dafny to Qafny
 *  In cases of Q[n] where n is known, we can simply to a bv<n> 
 * 	However, when n is unknown, seq<bv1> must be utilized instead
 */

//---------------------------------------
// Helper library/Boilerplate
//---------------------------------------

// returns true if all of x equals all of y up to (not including) the end index
predicate boundedSame(x: seq<bv1>, y: seq<bv1>, end: nat)
	requires end <= |x|
	requires end <= |y|
{
  forall i :: 0 <= i < end ==> x[i] == y[i]
}

// like boundedSame but you can also specify a low bound
predicate boundedSameRange(x: seq<bv1>, y: seq<bv1>, start: nat, end: nat)
	requires start <= end
	requires start <= |x|
	requires start <= |y|
	requires end <= |x|
	requires end <= |y|
{
	forall i :: start < i < end ==> x[i] == y[i]
}

function{:axiom} sqrt(a: real): real
	ensures (sqrt(a) * sqrt(a)) == a

// extreme oversimplification of the hadamard gate
method hadamard(x: bv1) returns (result: array<(real, seq<bv1>)>) 
	ensures result.Length == 2
	// amplitude post-conditions
	ensures result[0].0 == 1.0/sqrt(2.0)
	ensures x == 0 ==> result[1].0 == 1.0/sqrt(2.0)
	ensures x == 1 ==> result[1].0 == -1.0/sqrt(2.0)
	ensures result[0].1 == [0]
	ensures result[1].1 == [1]
{
	if x == 0 {
		result := new (real, seq<bv1>)[2][(1.0/sqrt(2.0), [0]), ( 1.0/sqrt(2.0), [1])];
	} else {
		result := new (real, seq<bv1>)[2][(1.0/sqrt(2.0), [0]), (-1.0/sqrt(2.0), [1])];
	}
}

// this method converts a bv1 (nor group) into an EN group
method enNor(x: bv1) returns (result: array<(real, seq<bv1>)>)
	ensures result.Length == 1
	ensures result[0].0 == 1.0 // amplitude
	ensures result[0].1 == [x] // value
{
	//                   1  value   
	result := new (real, seq<bv1>)[1] [(1.0, [x])];
}

// this method joins two en groups together
/*method join(x: array<(real, seq<bv1>)>, y: array<(real, seq<bv1>)>) returns (result: array<(real, seq<bv1>)>)
	ensures result.Length == x.Length * y.Length
	ensures forall i :: 0 <= i < result.Length ==> result[i].0 == x[i / y.Length].0 * y[i % y.Length].0
	ensures forall i :: 0 <= i < result.Length ==> result[i].1 == x[i / y.Length].1 + y[i % y.Length].1
{
	result := new (real, seq<bv1>)[x.Length * y.Length];

	var i: nat := 0;
	while i < x.Length 
		decreases x.Length - i
		invariant 0 <= i <= x.Length
		invariant forall k :: 0 <= k <= i ==> forall l :: 0 <= l <= y.Length ==> result[k * y.Length + l].0 == x[k].0 * y[l].0
		invariant forall k :: 0 <= k <= i ==> forall l :: 0 <= l <= y.Length ==> result[k * y.Length + l].1 == x[k].1 + y[l].1
	{
		var j: nat := 0;
		while j < y.Length
			decreases y.Length - j
			invariant 0 <= j <= y.Length
			// the full ranges of all previous iterations should be set
			invariant forall k :: 0 <= k < i ==> forall l :: 0 <= l <= y.Length ==> result[k * y.Length + l].0 == x[k].0 * y[l].0
			invariant forall k :: 0 <= k < i ==> forall l :: 0 <= l <= y.Length ==> result[k * y.Length + l].1 == x[k].1 + y[l].1
			// only a partial range for this iteration should be set
			invariant forall k :: 0 <= k < j ==> result[i * y.Length + k].0 == x[i].0 * y[k].0
			invariant forall k :: 0 <= k < j ==> result[i * y.Length + k].1 == x[i].1 + y[k].1
		{
			result[i*y.Length + j] := (x[i].0 * y[j].0, x[i].1 + y[j].1);

			j := j + 1;
		}

		i := i + 1;
	}
}*/

// cnot gate implemented for the entangled group. ci is the control index, fi is the flip index
/*method cnotEN(x: array<(real, seq<bv1>)>, ci: nat, fi: nat)
	modifies x
	requires x.Length >= 2
	requires forall j :: 0 <= j < x.Length ==> |x[j].1| == |x[0].1| 
	requires ci < |x[0].1|
	requires fi < |x[0].1|
	requires ci != fi
	// unchanged amplitude
	ensures forall j :: 0 <= j < x.Length ==> old(x[j].0) == x[j].0
	// unchanged length
	ensures forall j :: 0 <= j < x.Length ==> |old(x[j].1)| == |x[j].1|
	// everything below is unchanged
	ensures forall j :: 0 <= j < x.Length ==> boundedSame(old(x[j].1), x[j].1, fi)
	// the flip index is flipped if the control index is set (xor)
	ensures forall j :: 0 <= j < x.Length ==> old(x[j].1[ci]) ^ old(x[j].1[fi]) == x[j].1[fi]
	// everything above is unchanged
	ensures forall j :: 0 <= j < x.Length ==> boundedSameRange(old(x[j].1), x[j].1, fi, |x[j].1|)
{
	var i := 0;
	while i < x.Length 
		decreases x.Length - i
		invariant 0 <= i <= x.Length
		invariant ci != fi
		// the amplitudes shouldn't change
		invariant forall j :: 0 <= j < x.Length ==> old(x[j].0) == x[j].0
		// the length shouldn't change
		invariant forall j :: 0 <= j < x.Length ==> |old(x[j].1)| == |x[j].1|
		// everything after our current iteration shouldn't change
		invariant forall j :: i < j < x.Length ==> old(x[j].1) == x[j].1
		// everything before the flip index shouldn't change
		invariant forall j :: 0 <= j < i ==> boundedSame(old(x[j].1), x[j].1, fi) // <-- can't be maintained by the loop
		// the flip index is flipped if the control index is set (xor)
		invariant forall j :: 0 <= j < i ==> old(x[j].1[ci]) ^ old(x[j].1[fi]) == x[j].1[fi]
		// everything after the flip index shouldn't change
		invariant forall j :: 0 <= j < i ==> boundedSameRange(old(x[j].1), x[j].1, fi, |x[j].1|) // <-- can't be maintained by the loop
	{
		if (x[i].1[ci] == 1) {
			x[i] := (x[i].0, x[i].1[0..fi] + ([!x[i].1[fi]]) + x[i].1[(fi+1)..|x[i].1|]); // flip the bit at the bit index
		}

		assert old(x[i].0) == x[i].0;
		assert x[i].1[fi] == old(x[i].1[ci]) ^ old(x[i].1[fi]);

		i := i + 1;
	}
}*/

method cnotEN(x: array<(real, seq<bv1>)>)
	modifies x
	requires x.Length >= 2
	// all basis-kets have the same length
	requires forall j :: 0 <= j < x.Length ==> |x[j].1| == 2
	// unchanged amplitude
	ensures forall j :: 0 <= j < x.Length ==> old(x[j].0) == x[j].0
	// unchanged length
	ensures forall j :: 0 <= j < x.Length ==> |old(x[j].1)| == |x[j].1|
	// everything not at index 1 in the basis ket is unchanged
	ensures forall j :: 0 <= j < x.Length ==> boundedSame(old(x[j].1), x[j].1, 1)
	// the 2nd bit in every basis ket is flipped if the control index is set (xored)
	ensures forall j :: 0 <= j < x.Length ==> old(x[j].1[0]) ^ old(x[j].1[1]) == x[j].1[1]
	// everything after is unchanged
	// ensures forall j :: 0 <= j < x.Length ==> boundedSameRange(old(x[j].1), x[j].1, 1, |x[j].1|)
{
	var i := 0;
	while i < x.Length
		decreases x.Length - i
		// amplitudes shouldn't change
		invariant forall j :: 0 <= j < x.Length ==> old(x[j].0) == x[j].0
		// length shouldn't change
		invariant forall j :: 0 <= j < x.Length ==> |old(x[j].1)| == |x[j].1|
		invariant forall j :: 0 <= j < x.Length ==> |x[j].1| == 2
		invariant forall j :: 0 <= j < x.Length ==> |old(x[j].1)| == 2
		// everything after our current iteration shouldn't change
		invariant forall j :: i < j < x.Length ==> old(x[j].1) == x[j].1
		// everything before the flip index shouldn't change
		invariant forall j :: 0 <= j < i ==> boundedSame(old(x[j].1), x[j].1, 1)
		// the flip index is flipped if the control index is set (xor)
		invariant forall j :: 0 <= j < i ==> x[j].1[0] ^ old(x[j].1[1]) == x[j].1[1]
		// everything after the flip index shouldn't change
		// invariant forall j :: 0 <= j < i ==> boundedSameRange(old(x[j].1), x[j].1, 1, |x[j].1|)
	{
		if (x[i].1[0] == 1) {
			x[i] := (x[i].0, [x[i].1[0], !x[i].1[1]] + x[i].1[2..|x[i].1|]);
		}

		assert x[i].1[1] == old(x[i].1[0]) ^ old(x[i].1[1]);
		i := i + 1;
	}
}

// [0, [(1/sqrt(2), |0>), ((1/sqrt(2), |1>)]] <-- doesn't work with static typing
// [(1/sqrt(2), |00>), ((1/sqrt(2), |11>)]

// [1/sqrt(2), ]

//---------------------------------------
// Bell-Pair Proof
//---------------------------------------

// q : Q[2], q[0, 2) : nor ↦ |0⟩
method BellPair(q: seq<bv1>) 
	// 0. pre-conditions
	requires |q| == 2
	requires q == [0, 0] // requires q[0,2) : nor ↦ |0⟩ 
{
	// phase removed from entanglement example as it is not important for the bell pair example
	// entanglement is a list of (amplitude, basis vector) tuples that corresponds to the variety of basis states for the qubit
	// we have dropped the phase here from the full EN type definition: seq<(real, real, seq<bv1>)> and changed the seq<bv1> to bv2 for brevity
	
	// 1. convert x (q[0]) to en type
	//    induce superposition on the first qubit
	var q0_en := hadamard(q[0]);

	// 2. convert y (q[1]) to en type
	var q1_en := enNor(q[1]);

	// 3. join x and y together
	// var q_en := join(q0_en, q1_en);
	var q_en := new (real, seq<bv1>)[2] [(1.0/sqrt(2.0), [0, 0]), (1.0/sqrt(2.0), [1, 1])];

	/* if (q[0]) {
	 *	  q[1] *= λ (x => |(x + 1) % 2 ⟩);
	 * }  
	 * Ensures:
	 *	a. amplitude is unchanged
	 *	b. the [1] bit of the basis ket will be set if the [0] bit of the basis ket is set.
	 *	c. the [1] bit of the basis ket will be unset if the [0] bit of the basis ket is unset.
	*/

	// |01> --> |11>; |00> --> |00>
	// anything less than i is unchanged from the last loop
	// anything at i+1 is changed if i is positive. 


	// 4. cnot (x) { y }
	cnotEN(q_en); //, 0, 1);

	// 5. post-conditions
	// ensures q[0, 2) : en ↦ ∑ j ∈ [0 , 2) . 1/sqrt(2) |j⟩|j⟩
	assert(q_en[0].0 == 1.0/sqrt(2.0));
	assert(q_en[0].1 == [0, 0]);
	assert(q_en[1].0 == 1.0/sqrt(2.0));
	assert(q_en[1].1 == [1, 1]);

	print(q_en);
}