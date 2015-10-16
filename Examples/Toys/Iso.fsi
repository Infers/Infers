// Copyright (C) by Vesa Karvonen

module Toys.Iso

open Infers
open Infers.Rep

/// `Iso<'x, 'y>` represents an isomorphism between `'x` and `'y`.
type Iso<'x, 'y>

/// A few rules for deriving isomorphisms.
type [<InferenceRules>] Iso =
  new: unit -> Iso

  member Identity: unit -> Iso<'x, 'x>
  member Invert: Iso<'x, 'y> -> Iso<'y, 'x>
  member Compose: Iso<'x, 'y> * Iso<'y, 'z> -> Iso<'x, 'z>

  member Swap: unit -> Iso<'x * 'y, 'y * 'x>
  member Restructure: unit -> Iso<'x * ('y * 'z), 'y * ('x * 'z)>
  member First: Iso<'x1, 'x2> -> Iso<'x1 * 'y, 'x2 * 'y>

  member ``Pair<->Tuple``: unit -> Iso<Pair<'x, 'y>, 'x * 'y>

val iso<'x, 'y> : Iso<'x, 'y>
val fwd: Iso<'x, 'y> -> ('x -> 'y)
val bwd: Iso<'x, 'y> -> ('y -> 'x)
val convert: 'x -> 'y
