// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module Integral =
  open Infers

  /// Provides information on the integral type `'t`.
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

  type Integral =
    inherit Rules
    new: unit -> Integral

    static member Int8: Integral<int8>
    static member Int16: Integral<int16>
    static member Int32: Integral<int32>
    static member Int64: Integral<int64>

    static member UInt8: Integral<uint8>
    static member UInt16: Integral<uint16>
    static member UInt32: Integral<uint32>
    static member UInt64: Integral<uint64>

    static member NativeInt: Integral<nativeint>
    static member UNativeInt: Integral<unativeint>

    static member BigInt: Integral<bigint>
