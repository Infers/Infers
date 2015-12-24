// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Integral =
  open Infers

  type Integral<'t> =
    {
      IsSigned: bool
      IsBigInt: bool
      Suffices: list<string>
      ToInt64: 't -> int64
      OfInt64: int64 -> 't
      ToUInt64: 't -> uint64
      OfUInt64: uint64 -> 't
      ToBigInt: 't -> bigint
      OfBigInt: bigint -> 't
    }

  let inline mk isSigned isBigInt ofInt64 ofUInt64 ofBigInt toBigInt suffices =
    {IsSigned = isSigned
     IsBigInt = isBigInt
     Suffices = suffices
     ToInt64 = int64
     OfInt64 = ofInt64
     ToUInt64 = uint64
     OfUInt64 = ofUInt64
     ToBigInt = toBigInt
     OfBigInt = ofBigInt}

  type Integral () =
    inherit Rules ()

    static member  Int8  = mk true  false  int8   int8   int8  (int32 >> bigint) ["y"]
    static member  Int16 = mk true  false  int16  int16  int16 (int32 >> bigint) ["s"]
    static member  Int32 = mk true  false  int32  int32  int32 bigint [""; "l"]
    static member  Int64 = mk true  false  int64  int64  int64 bigint ["L"]

    static member UInt8  = mk false false uint8  uint8  uint8  (int32 >> bigint) ["uy"]
    static member UInt16 = mk false false uint16 uint16 uint16 (int32 >> bigint) ["us"]
    static member UInt32 = mk false false uint32 uint32 uint32 bigint ["u"; "ul"]
    static member UInt64 = mk false false uint64 uint64 uint64 bigint ["UL"]

    static member  NativeInt = mk true  false  nativeint  nativeint  (int64 >>  nativeint)  (int64 >> bigint) ["n"]
    static member UNativeInt = mk false false unativeint unativeint (uint64 >> unativeint) (uint64 >> bigint) ["un"]

    static member BigInt = mk true true bigint bigint id id ["I"]
