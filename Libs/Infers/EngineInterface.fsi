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

/// A type that inherits `Rules` is assumed to contain pure static rule methods
/// that are used by the inference engine.  Dependencies to other rule classes
/// are specified as attributes.
type [<AbstractClass>] Rules =
  inherit Attribute

  /// Default constructor.
  new: unit -> Rules

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
