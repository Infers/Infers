// Copyright (C) by Vesa Karvonen

/// `Infers` is a library for deriving F# values from their types and, in a way,
/// a direct application of the Curry-Howard correspondence. 
///
/// The basic idea is to view the types of F# member functions as Horn clauses.
/// Using a Prolog-style resolution engine it is then possible to attempt to
/// prove goals given as simple F# types.  During the resolution process the
/// engine invokes the member functions to build a value of the type given as
/// the goal.
///
/// Infers can be useful, for example, in situations where one might wish to use
/// something like type classes or when one might want to do datatype generic
/// programming.  Other kinds of applications are also quite possible.
namespace Infers

open System

/// Specifies which members to consider as rules.
type Members =
  /// Only consider public methods as rules.
  | PublicOnly = 1
  /// Consider both public and non-public methods as rules.
  | PublicAndPrivate = 3

/// Specifies whether results are memoized with a `StaticMap`.
type StaticMap =
  /// Do not cache results.
  | Nothing = 0
  /// Cache rule results.
  | Results = 1

/// A type that has the `InferenceRules` attribute is assumed to contain rule
/// methods that are used by the inference engine.
type InferenceRules =
  inherit Attribute

  /// Default constructor.  The defaults are safe, but in most cases you will
  /// want to say `InferenceRules (StaticMap = StaticMap.Results)`.
  new: unit -> InferenceRules

  /// Specifies which methods are considered as rules.
  ///
  /// Default: `Members.PublicOnly`.
  member Members: Members with get, set

  /// Specifies whether to memoize results with a `StaticMap`.
  ///
  /// Default: `StaticMap.Nothing`.
  member StaticMap: StaticMap with get, set

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
