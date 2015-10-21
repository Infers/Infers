// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module Infers.Toys.Integral

open Infers

/// Provides information on the integral type `'t`.
type Integral<'t> = {
    /// The suffix used by F# on literals of the integral type.
    Suffix: string
  }

type [<InferenceRules>] Integral =
  new: unit -> Integral

  member Int8: Integral<int8>
  member Int16: Integral<int16>
  member Int32: Integral<int32>
  member Int64: Integral<int64>

  member UInt8: Integral<uint8>
  member UInt16: Integral<uint16>
  member UInt32: Integral<uint32>
  member UInt64: Integral<uint64>
