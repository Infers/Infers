// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type [<InferenceRules>] Rep =
  /// Default constructor.
  new: unit -> Rep

  /// Attempts to construct a type representation for the type `'t`.
  member rep: unit -> Rep<'t>

  /// Attempts to constructs a union type representation for the type `'t`.
  member union: Rep<'t> -> Union<'t>

  /// Attempts to construct a product type representation for the type `'t`.
  member product: Rep<'t> -> Product<'t>

  /// Attempts to construct a record type representation for the type `'t`.
  member record: Rep<'t> -> Record<'t>

  /// Attempts to construct a primitive type representation for the type `'t`.
  member prim: Rep<'t> -> Prim<'t>

  /// Attempts to construct a nested sum for the type `'t`'.
  member asSum: Union<'t> * AsSum<'s, 't> -> AsSum<'s, 't>

  /// Attempts to construct a nested product for the type `'t`'.
  member asProduct: Product<'t> * AsProduct<'p, 'o, 't> -> AsProduct<'p, 'o, 't>

  /// View a single case union type as a product type.
  member viewAsProduct: AsSum<'p, 't> * Case<'p, 'p, 't> -> AsProduct<'p, 'p, 't>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  member tuple: Rep<'t> -> Tuple<'t>

  /// Trivially view a labelled elem as an elem.
  member asElem: Labelled<'e, 'r, 'o, 't> -> Elem<'e, 'r, 'o, 't>

  /// Trivially view a tuple item as an elem.
  member asElem: Tuple<'t> * Item<'e, 'r, 't> -> Elem<'e, 'r, 't, 't>

  /// Trivially view a record field as a labelled elem.
  member asLabelled: Field<'e, 'r, 't> -> Labelled<'e, 'r, 't, 't>

  /// Trivially view a case label as a labelled elem.
  member asLabelled: Union<'t> * Label<'e, 'r, 'o, 't> -> Labelled<'e, 'r, 'o, 't>
