module Infers.Engine

/// Tries to generate a value of the type `'a` by using the given set of
/// inference rules and the recursion rules.  See `InferenceRules` and
/// `RecursionRules`.
val tryGenerate: inferenceRules: seq<obj> -> recursionRules: obj -> option<'a>
