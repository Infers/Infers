// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module Bitwise =
  open Infers

  /// Isomorphism between `'t` and `'b`, which is either `byte`, `int16`,
  /// `int32` or `int64`.
  #if DOC
  ///
  /// It would be more logical to map bitwise types to unsigned integral types.
  /// However, except for `byte`, unsigned integral types are rarely used in
  /// user defined types.  A generic based on handling the signed integral types
  /// and bytes directly and the unsigned integral types bitwise is likely to be
  /// faster and more practical.
  #endif
  type Bitwise<'b, 't> = {
      ToBits: 't -> 'b
      OfBits: 'b -> 't
    }

  type Bitwise =
    inherit Rules
    new: unit -> Bitwise

    static member Bool: Bitwise<uint8, bool>

    static member Char: Bitwise<int16, char>

    /// This is very slow.  Pull requests are welcome!
    static member Float32: Bitwise<int32, float32>
    static member Float64: Bitwise<int64, float>

    static member Int8: Bitwise<uint8,  int8>

    static member UInt16: Bitwise<int16, uint16>
    static member UInt32: Bitwise<int32, uint32>
    static member UInt64: Bitwise<int64, uint64>
