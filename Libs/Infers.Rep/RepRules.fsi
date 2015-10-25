// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type Rep =
  inherit Rules
  new: unit -> Rep

  /// Construct a type representation for the type `'t`.
  static member Rep: unit -> Rep<'t>

  /// Attempts to constructs a union type representation for the type `'t`.
  static member Union: Rep<'t> * Union<'t> -> Union<'t>

  /// Attempts to construct a product type representation for the type `'t`.
  static member Product: Rep<'t> * Product<'t> -> Product<'t>

  /// Attempts to construct a record type representation for the type `'t`.
  static member Record: Rep<'t> * Record<'t> -> Record<'t>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  static member Tuple: Rep<'t> * Tuple<'t> -> Tuple<'t>

  /// Attempts to construct a primitive type representation for the type `'t`.
  static member Prim: Rep<'t> * Prim<'t> -> Prim<'t>

  /// Attempts to construct a type representation for the unsupported type `'t`.
  static member Unsupported: Rep<'t> * Unsupported<'t> -> Unsupported<'t>

  /// Construct a representation as nested choices for the type `'t`'.
  static member AsChoices: Rep<'t> * AsChoices<'s, 't> -> AsChoices<'s, 't>

  /// Construct a representation as nested pairs for the type `'t`'.
  static member AsPairs: Rep<'t> * AsPairs<'p, 'o, 't> -> AsPairs<'p, 'o, 't>

  /// View a single case union type as nested pairs.
  static member AsPairs: AsChoices<'p,'t> * Case<'p,'p,'t> -> AsPairs<'p,'p,'t>

  /// Trivially view a labelled elem as an elem.
  static member AsElem: Labelled<'e, 'r, 'o, 't> -> Elem<'e, 'r, 'o, 't>

  /// Trivially view a tuple item as an elem.
  static member AsElem: Item<'e, 'r, 't> -> Elem<'e, 'r, 't, 't>

  /// Trivially view a record field as a labelled elem.
  static member AsLabelled: Field<'e, 'r, 't> -> Labelled<'e, 'r, 't, 't>

  /// Trivially view a case label as a labelled elem.
  static member AsLabelled: Label<'e, 'r, 'o, 't> -> Labelled<'e, 'r, 'o, 't>

