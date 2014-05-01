#if FSHARP_NON_INTERACTIVE
namespace Infers
#endif

open System

/// A type that has the `InferenceRules` attribute is assumed to contain
/// inference rule methods that are used by the inference engine.
type InferenceRules () = inherit Attribute ()

/// Exception that can be raised by an otherwise matched rule to make the
/// inference engine to treat the rule as a non-match and backtrack.
exception Backtrack
