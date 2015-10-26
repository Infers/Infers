// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module SomeOf =
  /// `someOf<'t>` attempts to generate a valid value of type `'t`.
  val someOf<'t> : 't
