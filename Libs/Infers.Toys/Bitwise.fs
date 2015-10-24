// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open System

[<AutoOpen>]
module Bitwise =
  open Infers

  type Bitwise<'b, 't> = {
      ToBits: 't -> 'b
      OfBits: 'b -> 't
    }

  type BC = BitConverter

  type Bitwise () =
    inherit Rules ()

    static member Bool = {ToBits = function false -> 0uy | _ -> 1uy
                          OfBits = function 0uy -> false | _ -> true}

    static member Char = {ToBits = int16; OfBits = char}

    static member Float32 =
      {ToBits = fun (x: float32) -> BC.ToInt32 (BC.GetBytes x, 0)
       OfBits = fun (x: int32) -> BC.ToSingle (BC.GetBytes x, 0)}
    static member Float64 = {ToBits = BC.DoubleToInt64Bits
                             OfBits = BC.Int64BitsToDouble}

    static member Int8  = {ToBits = uint8;  OfBits = int8}

    static member UInt16 = {ToBits = int16; OfBits = uint16}
    static member UInt32 = {ToBits = int32; OfBits = uint32}
    static member UInt64 = {ToBits = int64; OfBits = uint64}
