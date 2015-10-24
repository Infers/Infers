// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module GFlip =
  /// Derives a function that reorders, i.e. "generically" flips, the arguments
  /// of a given function.
  ///
  /// Note that if the arguments do not have unique types the resulting ordering
  /// may not be the one you want.
  ///
  /// Due to limitations of the F# type system, the function must have a
  /// monomorphic type.  If the function does not have a monomorphic type, you
  /// must constrain it.
  val gflip: ('a -> 'b) -> ('c -> 'd)
