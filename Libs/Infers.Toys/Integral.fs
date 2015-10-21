// Copyright (C) by Vesa Karvonen

module Infers.Toys.Integral

open Infers

type Integral<'t> = {
    Suffix: string
  }

type [<InferenceRules>] Integral () =
  member t.Int8 : Integral<int8>  = {Suffix = "y"}
  member t.Int16: Integral<int16> = {Suffix = "s"}
  member t.Int32: Integral<int32> = {Suffix = ""}
  member t.Int64: Integral<int64> = {Suffix = "L"}

  member t.UInt8 : Integral<uint8>  = {Suffix = "uy"}
  member t.UInt16: Integral<uint16> = {Suffix = "us"}
  member t.UInt32: Integral<uint32> = {Suffix = "u"}
  member t.UInt64: Integral<uint64> = {Suffix = "UL"}
