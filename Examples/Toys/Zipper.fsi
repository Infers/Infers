// Copyright (C) by Vesa Karvonen

module Toys.Zipper

////////////////////////////////////////////////////////////////////////////////
// Zipper classes

/// Zipper over type `'w` pointint at any type of hole.
type [<AbstractClass>] Zipper<'w> =
  abstract DownHeadAny: unit -> option<Zipper<'w>>
  abstract DownHeadThe: unit -> option<Zipper<'w, 'w>>
  abstract DownLastAny: unit -> option<Zipper<'w>>
  abstract DownLastThe: unit -> option<Zipper<'w, 'w>>
  abstract NextAny: unit -> option<Zipper<'w>>
  abstract NextThe: unit -> option<Zipper<'w, 'w>>
  abstract PrevAny: unit -> option<Zipper<'w>>
  abstract PrevThe: unit -> option<Zipper<'w, 'w>>
  abstract Up: unit -> option<Zipper<'w>>

/// Zipper over type `'w' pointing at a hole of type `'h`.
and [<AbstractClass>] Zipper<'w, 'h> =
  inherit Zipper<'w>
  abstract Get: unit -> 'h
  abstract Set: 'h -> Zipper<'w, 'h>

////////////////////////////////////////////////////////////////////////////////
// Common interface

val fromZipper: Zipper<'x> -> 'x

////////////////////////////////////////////////////////////////////////////////
// Heterogenous interface

val toZipperAny: 'x -> Zipper<'x>

val getAny: Zipper<'x> -> option<'y>

val setAny: 'y -> Zipper<'x> -> option<Zipper<'x>>

val inline downHeadAny: Zipper<'x> -> option<Zipper<'x>>
val inline downLastAny: Zipper<'x> -> option<Zipper<'x>>

val inline upAny: Zipper<'x> -> option<Zipper<'x>>

val inline nextAny: Zipper<'x> -> option<Zipper<'x>>
val inline prevAny: Zipper<'x> -> option<Zipper<'x>>

////////////////////////////////////////////////////////////////////////////////
// Homogenous interface

val toZipperThe: 'x -> Zipper<'x, 'x>

val inline getThe: Zipper<'x, 'x> -> 'x
val inline setThe: 'x -> Zipper<'x, 'x> -> Zipper<'x, 'x>

val inline downHeadThe: Zipper<'x> -> option<Zipper<'x, 'x>>
val inline downLastThe: Zipper<'x> -> option<Zipper<'x, 'x>>

val upThe: Zipper<'x> -> option<Zipper<'x, 'x>>

val inline nextThe: Zipper<'x> -> option<Zipper<'x, 'x>>
val inline prevThe: Zipper<'x> -> option<Zipper<'x, 'x>>
