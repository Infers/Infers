// Copyright (C) by Vesa Karvonen

module Infers.Toys.Rec

open Infers
open Infers.Rep

type RecP<'e, 'r, 'o, 't>

/// Rules for computing fixed points over products, single case union types (aka
/// newtypes) and functions.
type [<InferenceRules>] Rec =
  inherit RecFn
  new: unit -> Rec
  member Elem: Elem<'e, 'r, 'o, 't> * Rec<'e> -> RecP<'e, 'r, 'o, 't>
  member Pair: RecP<     'e     , Pair<'e, 'r>, 'o, 't>
             * RecP<         'r ,          'r , 'o, 't>
            -> RecP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't>
  member Product: Rep * AsPairs<'p, 'o, 't> * RecP<'p, 'p, 'o, 't> -> Rec<'t>
