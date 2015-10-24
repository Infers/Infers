// Copyright (C) by Vesa Karvonen

module Toys.Iso

open Infers
open Infers.Rep

/// `Iso<'x, 'y>` represents an isomorphism between `'x` and `'y`.
type Iso<'x, 'y>

/// A few rules for deriving isomorphisms.
type Iso =
  inherit Rules
  new: unit -> Iso

  static member Identity: unit -> Iso<'x, 'x>
  static member Invert: Iso<'x, 'y> -> Iso<'y, 'x>
  static member Compose: Iso<'x, 'y> * Iso<'y, 'z> -> Iso<'x, 'z>

  static member Swap: unit -> Iso<'x * 'y, 'y * 'x>
  static member Restructure: unit -> Iso<'x * ('y * 'z), 'y * ('x * 'z)>
  static member First: Iso<'x1, 'x2> -> Iso<'x1 * 'y, 'x2 * 'y>

  static member ``Pair<->Tuple``: unit -> Iso<Pair<'x, 'y>, 'x * 'y>

val iso<'x, 'y> : Iso<'x, 'y>
val fwd: Iso<'x, 'y> -> ('x -> 'y)
val bwd: Iso<'x, 'y> -> ('y -> 'x)
val convert: 'x -> 'y
