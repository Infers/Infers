// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module Integral =
  open Infers

  /// Provides information on the integral type `'t`.
  type Integral<'t> = {
      /// The suffix used by F# on literals of the integral type.
      Suffix: string
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
