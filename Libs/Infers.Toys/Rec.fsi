// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module Rec =
  open Infers
  open Infers.Rep

  type RecP<'e, 'r, 'o, 't>

  /// Rules for computing fixed points over products, single case union types (aka
  /// newtypes) and functions.
  type Rec =
    inherit Rules
    new: unit -> Rec
    static member Elem: Elem<'e, 'r, 'o, 't> * Rec<'e> -> RecP<'e, 'r, 'o, 't>
    static member Pair: RecP<     'e     , Pair<'e, 'r>, 'o, 't>
                      * RecP<         'r ,          'r , 'o, 't>
                     -> RecP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't>
    static member Product: AsPairs<'p, 'o, 't> * RecP<'p, 'p, 'o, 't> -> Rec<'t>
