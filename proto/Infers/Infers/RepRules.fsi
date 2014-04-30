namespace Infers.Rep

open Infers

/// Inference rules for generic type representations.
type [<InferenceRules>] Rules =
  new: unit -> Rules

  /// Attempts to constructs a record type representation for the type `'r`.
  member record: unit -> Record<'r>

  /// Attempts to constructs a union type representation for the type `'u`.
  member union: unit -> Union<'u>

  /// Attempts to constructs a tuple type representation for the type `'t`.
  member tuple: unit -> Tuple<'t>
