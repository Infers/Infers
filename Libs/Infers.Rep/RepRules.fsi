// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type Rep =
  inherit Rules
  new: unit -> Rep

  static member Rep: unit -> Rep<'t>

  static member Union: Rep<'t> * Union<'t> -> Union<'t>
  static member Product: Rep<'t> * Product<'t> -> Product<'t>
  static member Record: Rep<'t> * Record<'t> -> Record<'t>
  static member Tuple: Rep<'t> * Tuple<'t> -> Tuple<'t>
  static member Prim: Rep<'t> * Prim<'t> -> Prim<'t>
  static member Unsupported: Rep<'t> * Unsupported<'t> -> Unsupported<'t>

  static member AnyCase: AsChoices<'s,'t> * Case<'p,'o,'t> -> Case<'p,'o,'t>

  static member AsChoices: Rep<'t> * AsChoices<'s,'t> -> AsChoices<'s,'t>

  static member AsPairs: AsPairs<'p,'t,'t> -> AsPairs<'p,'t>
  static member AsPairs: Rep<'t> * AsPairs<'p,'o,'t> -> AsPairs<'p,'o,'t>
  static member AsPairs: AsChoices<'p,'t> * Case<'p,'p,'t> -> AsPairs<'p,'p,'t>

  static member AsElem: Labelled<'e,'r,'o,'t> -> Elem<'e,'r,'o,'t>
  static member AsElem: Item<'e,'r,'t> -> Elem<'e,'r,'t,'t>

  static member AsLabelled: Field<'e,'r,'t> -> Labelled<'e,'r,'t,'t>
  static member AsLabelled: Label<'e,'r,'o,'t> -> Labelled<'e,'r,'o,'t>
