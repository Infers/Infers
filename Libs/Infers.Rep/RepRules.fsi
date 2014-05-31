namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type [<InferenceRules>] Rep =
  /// Default constructor.
  new: unit -> Rep

  /// Attempts to constructs a union type representation for the type `'u`.
  member union: unit -> Union<'u>

  /// Attempts to construct a product type representation for the type `'p`.
  member product: unit -> Product<'p>

  /// Attempts to constructs a record type representation for the type `'r`.
  member record: unit -> Record<'r>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  member tuple: unit -> Tuple<'t>

  /// Trivially view a tuple elem as an elem.
  member asElem: Item<'e, 'sp, 't> -> Elem<'e, 'sp, 't>

  /// Trivially view a record field as an elem.
  member asElem: Field<'f, 'sp, 'r> -> Elem<'f, 'sp, 'r>

  /// Trivially view a case label as an elem.
  member asElem: Label<'l, 'sp, 'sc, 'u> -> Elem<'l, 'sp, 'u>
