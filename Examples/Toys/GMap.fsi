// Copyright (C) by Vesa Karvonen

module Toys.GMap

open Infers
open Infers.Rep

type GMap<'w, 'h> = ('h -> 'h) -> 'w -> 'w
type GM<'w, 'h>
type GMP<'e, 'r, 'o, 'w, 'h>
type GMS<'p, 'o, 'w, 'h>

type GMap =
  inherit Rules
  new: unit -> GMap

  static member Same: unit -> GMap<'h, 'h>
  static member NotSame: GM<'w, 'h> -> GMap<'w, 'h>

  static member Prim: Prim<'w> -> GM<'w, 'h>

  static member String: unit -> GM<string, 'h>

  static member Elem: Elem<'e, 'r, 'o, 'w> * GMap<'e, 'h> -> GMP<'e, 'r, 'o, 'w, 'h>

  static member Pair: GMP<     'e     , Pair<'e, 'r>, 'o, 'w, 'h>
                    * GMP<         'r ,          'r , 'o, 'w, 'h>
                   -> GMP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'w, 'h>

  static member Product: AsPairs<'p, 'o, 'w> * GMP<'p, 'p, 'o, 'w, 'h> -> GM<'w, 'h>

  static member Case: Case<Empty, 'o, 'w> -> GMS<Empty, 'o, 'w, 'h>

  static member Case: Case<'p, 'o, 'w> * GMP<'p, 'p, 'o, 'w, 'h> -> GMS<'p, 'o, 'w, 'h>

  static member Choice: GMS<       'p     , Choice<'p, 'o>, 'w, 'h>
                      * GMS<           'o ,            'o , 'w, 'h>
                     -> GMS<Choice<'p, 'o>, Choice<'p, 'o>, 'w, 'h>

  static member Sum: AsChoices<'s, 'w> * GMS<'s, 's, 'w, 'h> -> GM<'w, 'h>

val gmap: ('h -> 'h) -> 'w -> 'w
