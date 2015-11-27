// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Integral =
  open Infers

  type Integral<'t> = {
      Suffix: string
    }

  type Integral () =
    inherit Rules ()

    static member Int8 : Integral<int8>  = {Suffix = "y"}
    static member Int16: Integral<int16> = {Suffix = "s"}
    static member Int32: Integral<int32> = {Suffix = ""}
    static member Int64: Integral<int64> = {Suffix = "L"}

    static member UInt8 : Integral<uint8>  = {Suffix = "uy"}
    static member UInt16: Integral<uint16> = {Suffix = "us"}
    static member UInt32: Integral<uint32> = {Suffix = "u"}
    static member UInt64: Integral<uint64> = {Suffix = "UL"}
