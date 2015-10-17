// Copyright (C) by Vesa Karvonen

module Toys.GMap

open Infers
open Infers.Rep

type GMap<'w, 'h> = ('h -> 'h) -> 'w -> 'w
type GM<'w, 'h>
type GMP<'e, 'r, 'o, 'w, 'h>
type GMS<'p, 'o, 'w, 'h>

type [<InferenceRules>] GMap =
  new: unit -> GMap

  member Same: unit -> GMap<'h, 'h>
  member NotSame: Rep * RecFn * GM<'w, 'h> -> GMap<'w, 'h>

  member Prim: Prim<'w> -> GM<'w, 'h>

  member Elem: Elem<'e, 'r, 'o, 'w> * GMap<'e, 'h> -> GMP<'e, 'r, 'o, 'w, 'h>

  member Pair: GMP<     'e     , Pair<'e, 'r>, 'o, 'w, 'h>
             * GMP<         'r ,          'r , 'o, 'w, 'h>
            -> GMP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'w, 'h>

  member Product: AsProduct<'p, 'o, 'w> * GMP<'p, 'p, 'o, 'w, 'h> -> GM<'w, 'h>

  member Case: Case<Empty, 'o, 'w> -> GMS<Empty, 'o, 'w, 'h>

  member Case: Case<'p, 'o, 'w> * GMP<'p, 'p, 'o, 'w, 'h> -> GMS<'p, 'o, 'w, 'h>

  member Choice: GMS<       'p     , Choice<'p, 'o>, 'w, 'h>
               * GMS<           'o ,            'o , 'w, 'h>
              -> GMS<Choice<'p, 'o>, Choice<'p, 'o>, 'w, 'h>

  member Sum: AsSum<'s, 'w> * GMS<'s, 's, 'w, 'h> -> GM<'w, 'h>

val gmap: ('h -> 'h) -> 'w -> 'w
