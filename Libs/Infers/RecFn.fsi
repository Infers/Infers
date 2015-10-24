// Copyright (C) by Vesa Karvonen

namespace Infers

open System

/// Inference rules for creating proxies for creating recursive functions.
type RecFn =
  inherit Rules

  new: unit -> RecFn

  /// Creates a proxy for a F# function.
  static member func: unit -> Rec<'x -> 'y>

  /// Creates a proxy for a .Net function delegate.
  static member func0: unit -> Rec<Func<'x>>

  /// Creates a proxy for a .Net function delegate.
  static member func1: unit -> Rec<Func<'x, 'y>>

  /// Creates a proxy for a .Net function delegate.
  static member func2: unit -> Rec<Func<'x, 'y, 'z>>
