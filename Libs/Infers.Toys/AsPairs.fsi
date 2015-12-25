// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers.Rep

[<AutoOpen>]
module AsPairs =
  type AsPairs<'p,'t> with
    member Extract: from: array<obj> * into: byref<'p> -> unit
    member Create: from: array<obj> -> 't
    member Overwrite: record: Record<'t> * into: 't * from: array<obj> -> unit
