// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module GUncurry =
  /// Derives a function that uncurries a given n-ary curried function.  For
  /// example, `guncurry (sprintf "%d %s") (1, "a") = "1 a"`.
#if DOC
  ///
  /// Due to limitations of the F# type system, the function must have a
  /// monomorphic type.  If the function does not have a monomorphic type, you
  /// must constrain it.
#endif
  val guncurry: ('a -> 'b) -> ('c -> 'd)
