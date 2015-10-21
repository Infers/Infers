// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module Infers.Toys.MayBeMutable

/// Determines whether a value of the given type may contain mutable objects.
val mayBeMutable<'t> : bool
