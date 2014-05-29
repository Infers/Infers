namespace Infers

open System

/// A type that has the `InferenceRules` attribute is assumed to contain rule
/// methods that are used by the inference engine.
type InferenceRules =
  inherit Attribute

  /// If `true` then both public and non-public members are to be considered as
  /// rules.  If `false` only public members are to be considered as rules.
  val NonPublic: bool

  /// Default constructor sets `NonPublic` to false.
  new: unit -> InferenceRules

  /// Allows to specify the value for `NonPublic`.
  new: nonPublic: bool -> InferenceRules

/// Proxy for a potentially recursive value.
type [<AbstractClass>] Rec<'x> =
  /// Empty default constructor.
  new: unit -> Rec<'x>

  /// Returns a wrapper of type `'x` that corresponds to the value of the
  /// proxy.  Note that `Get` may be called on a `Rec` proxy before `Set` is
  /// called.
  abstract Get: unit -> 'x

  /// Sets the value of the proxy.
  abstract Set: 'x -> unit

/// Exception that can be raised by an otherwise matched rule to make the
/// inference engine to treat the rule as a non-match and backtrack.
exception Backtrack
