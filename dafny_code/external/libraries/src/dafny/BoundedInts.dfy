// RUN: %verify "%s"

module {:options "-functionSyntax:4"} Dafny.BoundedInts {
  const TWO_TO_THE_0:   int := 1

  const TWO_TO_THE_1:   int := 2
  const TWO_TO_THE_2:   int := 4
  const TWO_TO_THE_4:   int := 16
  const TWO_TO_THE_5:   int := 32
  const TWO_TO_THE_7:   int := 0x80
  const TWO_TO_THE_8:   int := 0x100
  const TWO_TO_THE_15:  int := 0x8000
  const TWO_TO_THE_16:  int := 0x1_0000
  const TWO_TO_THE_24:  int := 0x100_0000
  const TWO_TO_THE_31:  int := 0x8000_0000
  const TWO_TO_THE_32:  int := 0x1_00000000
  const TWO_TO_THE_40:  int := 0x100_00000000
  const TWO_TO_THE_48:  int := 0x10000_00000000
  const TWO_TO_THE_56:  int := 0x1000000_00000000
  const TWO_TO_THE_63:  int := 0x80000000_00000000
  const TWO_TO_THE_64:  int := 0x1_00000000_00000000
  const TWO_TO_THE_127: int := 0x80000000_00000000_00000000_00000000
  const TWO_TO_THE_128: int := 0x1_00000000_00000000_00000000_00000000
  const TWO_TO_THE_256: int := 0x1_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
  const TWO_TO_THE_512: int :=
    0x1_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000;

  newtype uint8 = x: int  | 0 <= x < TWO_TO_THE_8
  newtype uint16 = x: int | 0 <= x < TWO_TO_THE_16
  newtype uint32 = x: int | 0 <= x < TWO_TO_THE_32
  newtype uint64 = x: int | 0 <= x < TWO_TO_THE_64
  newtype uint128 = x: int | 0 <= x < TWO_TO_THE_128

  newtype int8 = x: int   | -TWO_TO_THE_7 <= x < TWO_TO_THE_7
  newtype int16 = x: int  | -TWO_TO_THE_15 <= x < TWO_TO_THE_15
  newtype int32 = x: int  | -TWO_TO_THE_31 <= x < TWO_TO_THE_31
  newtype int64 = x: int  | -TWO_TO_THE_63 <= x < TWO_TO_THE_63
  newtype int128 = x: int  | -TWO_TO_THE_127 <= x < TWO_TO_THE_127

  newtype nat8 = x: int   | 0 <= x < TWO_TO_THE_7
  newtype nat16 = x: int  | 0 <= x < TWO_TO_THE_15
  newtype nat32 = x: int  | 0 <= x < TWO_TO_THE_31
  newtype nat64 = x: int  | 0 <= x < TWO_TO_THE_63
  newtype nat128 = x: int  | 0 <= x < TWO_TO_THE_127

}
