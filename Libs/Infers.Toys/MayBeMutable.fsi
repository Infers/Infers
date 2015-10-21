// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module MayBeMutable =
  /// Determines whether a value of the given type may contain mutable objects.
  val mayBeMutable<'t> : bool
