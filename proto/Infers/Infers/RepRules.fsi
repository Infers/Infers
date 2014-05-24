namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type [<InferenceRules>] Rep =
  new: unit -> Rep

  /// Attempts to constructs a union type representation for the type `'u`.
  member union: unit -> Union<'u>

  /// Attempts to constructs a record type representation for the type `'r`.
  member record: unit -> Record<'r>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  member tuple: unit -> Tuple<'t>

  /// Trivially view a field as an elem.
  member asElem: Field<'r, 'f, 'sp> -> Elem<'r, 'f, 'sp>

  /// Trivially view a label as an elem.
  member asElem: Label<'u, 'sc, 'l, 'sp> -> Elem<'u, 'l, 'sp>
