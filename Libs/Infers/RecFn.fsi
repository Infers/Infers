// Copyright (C) by Vesa Karvonen

namespace Infers

open System

/// Inference rules for creating proxies for creating recursive functions.
type [<InferenceRules>] RecFn =
  /// Default constructor.
  new: unit -> RecFn

  /// Creates a proxy for a F# function.
  member func: unit -> Rec<'x -> 'y>

  /// Creates a proxy for a .Net function delegate.
  member func0: unit -> Rec<Func<'x>>

  /// Creates a proxy for a .Net function delegate.
  member func1: unit -> Rec<Func<'x, 'y>>

  /// Creates a proxy for a .Net function delegate.
  member func2: unit -> Rec<Func<'x, 'y, 'z>>
