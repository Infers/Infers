// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open System
open Infers
open Infers.Rep

[<AutoOpen>]
module Rec =

  /// Combination of `ref<'t>` and `Rec<'t>`.
  type RecVal<'t> =
    inherit Rec<'t>
    val mutable Rec: 't
    new: (RecVal<'t> -> 't) -> RecVal<'t>

  /// Conveniently creates a `RecVal<'t>` and returns it as a `Rec<'t>`.
  val inline recVal<'t> : (RecVal<'t> -> 't) -> Rec<'t>

  type RecP<'e, 'r, 'o, 't>

  /// Rules for computing fixed points over products, single case union types (aka
  /// newtypes) and functions.
  type Rec =
    inherit Rules
    new: unit -> Rec
    static member Func: unit -> Rec<'x -> 'y>
    static member Func0: unit -> Rec<Func<'x>>
    static member Func1: unit -> Rec<Func<'x, 'y>>
    static member Func2: unit -> Rec<Func<'x, 'y, 'z>>
    static member Elem: Elem<'e, 'r, 'o, 't> * Rec<'e> -> RecP<'e, 'r, 'o, 't>
    static member Pair: RecP<     'e     , Pair<'e, 'r>, 'o, 't>
                      * RecP<         'r ,          'r , 'o, 't>
                     -> RecP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't>
    static member Product: AsPairs<'p,'o,'t> * RecP<'p,'p,'o,'t> -> Rec<'t>
