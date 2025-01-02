method NorHad(x: bv1) returns (result: array<(real, seq<bv1>)>) 
	ensures result.Length == 2
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