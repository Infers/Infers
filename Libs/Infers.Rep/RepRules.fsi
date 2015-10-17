// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type [<InferenceRules>] Rep =
  /// Default constructor.
  new: unit -> Rep

  /// Construct a type representation for the type `'t`.
  member rep: unit -> Rep<'t>

  /// Attempts to constructs a union type representation for the type `'t`.
  member union: Rep<'t> -> Union<'t>

  /// Attempts to construct a product type representation for the type `'t`.
  member product: Rep<'t> -> Product<'t>

  /// Attempts to construct a record type representation for the type `'t`.
  member record: Rep<'t> -> Record<'t>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  member tuple: Rep<'t> -> Tuple<'t>

  /// Attempts to construct a primitive type representation for the type `'t`.
  member prim: Rep<'t> -> Prim<'t>

  /// Construct a representation as nested choices for the type `'t`'.
  member asChoices: Union<'t> * AsChoices<'s, 't> -> AsChoices<'s, 't>

  /// Construct a representation as nested pairs for the type `'t`'.
  member asPairs: Product<'t> * AsPairs<'p, 'o, 't> -> AsPairs<'p, 'o, 't>

  /// View a single case union type as nested pairs.
  member viewAsPairs: AsChoices<'p, 't> * Case<'p, 'p, 't> -> AsPairs<'p, 'p, 't>

  /// Trivially view a labelled elem as an elem.
  member asElem: Labelled<'e, 'r, 'o, 't> -> Elem<'e, 'r, 'o, 't>

  /// Trivially view a tuple item as an elem.
  member asElem: Tuple<'t> * Item<'e, 'r, 't> -> Elem<'e, 'r, 't, 't>

  /// Trivially view a record field as a labelled elem.
  member asLabelled: Field<'e, 'r, 't> -> Labelled<'e, 'r, 't, 't>

  /// Trivially view a case label as a labelled elem.
  member asLabelled: Union<'t> * Label<'e, 'r, 'o, 't> -> Labelled<'e, 'r, 'o, 't>
