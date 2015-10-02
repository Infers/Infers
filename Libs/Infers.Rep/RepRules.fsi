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

  /// Attempts to construct a nested choice for the type `'t`'.
  member asChoice: Union<'t> * AsChoice<'c, 't> -> AsChoice<'c, 't>

  /// Attempts to construct a nested product for the type `'t`'.
  member asProduct: Product<'t> * AsProduct<'p, 't> -> AsProduct<'p, 't>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  member tuple: Rep<'t> -> Tuple<'t>

  /// Trivially view a labelled elem as an elem.
  member asElem: Labelled<'e, 'sp, 't> -> Elem<'e, 'sp, 't>

  /// Trivially view a tuple item as an elem.
  member asElem: Tuple<'t> * Item<'e, 'sp, 't> -> Elem<'e, 'sp, 't>

  /// Trivially view a record field as an elem.
  member asLabelled: Field<'f, 'sp, 'r> -> Labelled<'f, 'sp, 'r>

  /// Trivially view a case label as an elem.
  member asLabelled: Union<'u> * Label<'l, 'sp, 'sc, 'u> -> Labelled<'l, 'sp, 'u>
