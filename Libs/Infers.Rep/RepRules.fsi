// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type [<InferenceRules>] Rep =
  /// Default constructor.
  new: unit -> Rep

  /// Attempts to construct a type representation for the type `'t`.
  member rep: unit -> Rep<'t>

  /// Attempts to constructs a union type representation for the type `'u`.
  member union: Rep<'u> -> Union<'u>

  /// Attempts to construct a product type representation for the type `'p`.
  member product: Rep<'p> -> Product<'p>

  /// Attempts to construct a record type representation for the type `'r`.
  member record: Rep<'r> -> Record<'r>

  /// Attempts to construct a primitive type representation for the type `'t`.
  member prim: Rep<'t> -> Prim<'t>

  /// Attempts to construct a nested sum for the type `'t`'.
  member asSum: Union<'t> * AsSum<'c, 't> -> AsSum<'c, 't>

  /// Attempts to construct a nested product for the type `'t`'.
  member asProduct: Product<'t> * AsProduct<'p, 'o, 't> -> AsProduct<'p, 'o, 't>

  /// View a single case union type as a product type.
  member viewAsProduct: AsSum<'p, 'u> * Case<'p, 'p, 'u> -> AsProduct<'p, 'p, 'u>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  member tuple: Rep<'t> -> Tuple<'t>

  /// Trivially view a labelled elem as an elem.
  member asElem: Labelled<'e, 'sp, 'c, 't> -> Elem<'e, 'sp, 'c, 't>

  /// Trivially view a tuple item as an elem.
  member asElem: Tuple<'t> * Item<'e, 'sp, 't> -> Elem<'e, 'sp, 't, 't>

  /// Trivially view a record field as a labelled elem.
  member asLabelled: Field<'f, 'sp, 'r> -> Labelled<'f, 'sp, 'r, 'r>

  /// Trivially view a case label as a labelled elem.
  member asLabelled: Union<'u> * Label<'l, 'sp, 'sc, 'u> -> Labelled<'l, 'sp, 'sc, 'u>
