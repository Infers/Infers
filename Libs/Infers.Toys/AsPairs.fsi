// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers.Rep

[<AutoOpen>]
module AsPairs =
  type AsPairs<'p,'t> with
    member Extract: from: 't * into: array<obj> -> unit
    member Create: from: array<obj> -> 't
    member Overwrite: record: Record<'t> * into: 't * from: array<obj> -> unit
