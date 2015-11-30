// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open System
open Infers
open Infers.Rep

[<AutoOpen>]
module Rec =

  /// Combination of `ref<'t>` and `Rec<'t>`.  See also: `recVal`.
  type RecVal<'t> =
    inherit Rec<'t>

    /// This is initially default initialized, but may later be written with a
    /// value of type `'t` via the `Rec<'t>` base class to tie the knot.
    val mutable Rec: 't

    /// Constructs a new `RecVal<'t>`, given a function that returns a new value
    /// of type `'t` that delegates to `RecVal.Rec`, but does not read it
    /// immediately.  See `recVal`.
    new: (RecVal<'t> -> 't) -> RecVal<'t>

  /// Conveniently creates a `RecVal<'t>` and returns it as a `Rec<'t>`.
#if DOC
  ///
  /// The given function should delegate to `RecVal.Rec`, but must not read it
  /// immediately.  For example, one could define `Rec.Fun` as follows:
  ///
  ///> static member Fun () = recVal <| fun r -> fun x -> r.Rec x
  ///
  /// Note that the above does not read `r.Rec` immediately.  The following
  /// definition
  ///
  ///> static member Fun () = recVal <| fun r -> r.Rec : _ -> _
  ///
  /// reads `r.Rec` immediately and does not work correctly.
#endif
  val inline recVal<'t> : (RecVal<'t> -> 't) -> Rec<'t>

  type RecP<'e, 'r, 'o, 't>

  /// Rules for computing fixed points over products, single case union types
  /// (aka newtypes) and functions.
#if DOC
#endif
  type Rec =
    inherit Rules
    new: unit -> Rec
    static member Fun: unit -> Rec<'x -> 'y>
    static member Func0: unit -> Rec<Func<'x>>
    static member Func1: unit -> Rec<Func<'x, 'y>>
    static member Func2: unit -> Rec<Func<'x, 'y, 'z>>
    static member Elem: Elem<'e, 'r, 'o, 't> * Rec<'e> -> RecP<'e, 'r, 'o, 't>
    static member Pair: RecP<     'e     , Pair<'e, 'r>, 'o, 't>
                      * RecP<         'r ,          'r , 'o, 't>
                     -> RecP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't>
    static member Product: AsPairs<'p,'o,'t> * RecP<'p,'p,'o,'t> -> Rec<'t>
