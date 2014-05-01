module Infers.Engine

/// Tries to generate a value of the type `'a` by using the given set of
/// inference rules.  See `InferenceRules`.
val tryGenerate: explain: bool
              -> inferenceRules: seq<obj>
              -> option<'a>
