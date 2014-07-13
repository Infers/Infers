// Copyright (C) by Vesa Karvonen

namespace Infers

/// Represents a kind of static, or code generation time, mapping of types to
/// values.
///
/// A single value of type `'v` can be stored for a type of `'k`.  A module
/// typically gives some private type for `'k` and then stores unique values
/// for many types `'v`.
///
/// `StaticMap<'k, 'v>` is designed for situations in which the mapping is
/// polytypic and static.  Use in other kinds of contexts is unlikely to make
/// sense.
type [<Sealed>] StaticMap<'k> =
  /// This is used internally by `Memoize`. 
  static member MemoizeLocked: (unit -> 'v) -> 'v

  /// Memoizes the thunk using `StaticMap<'k, option<'v>>`.
  static member inline Memoize: (unit -> 'v) -> 'v
