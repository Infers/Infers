// Copyright (C) by Vesa Karvonen

module Toys.Zipper

open Infers
open Infers.Rep
open Toys.Rec
open Toys.Basic

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

and [<AbstractClass>] Zipper<'w, 'h> =
  inherit Zipper<'w>
  abstract Get: unit -> 'h
  abstract Set: 'h -> Zipper<'w, 'h>

////////////////////////////////////////////////////////////////////////////////

val toZipperAny: 'x -> Zipper<'x>
val toZipperThe: 'x -> Zipper<'x, 'x>

val fromZipper: Zipper<'x> -> 'x

val getAny: Zipper<'x> -> option<'y>
val inline getThe: Zipper<'x, 'x> -> 'x

val setAny: 'y -> Zipper<'x> -> option<Zipper<'x>>
val inline setThe: 'x -> Zipper<'x, 'x> -> Zipper<'x, 'x>

val inline downHeadAny: Zipper<'x> -> option<Zipper<'x>>
val inline downHeadThe: Zipper<'x> -> option<Zipper<'x, 'x>>
val inline downLastAny: Zipper<'x> -> option<Zipper<'x>>
val inline downLastThe: Zipper<'x> -> option<Zipper<'x, 'x>>

val inline upAny: Zipper<'x> -> option<Zipper<'x>>
val upThe: Zipper<'x> -> option<Zipper<'x, 'x>>

val inline nextAny: Zipper<'x> -> option<Zipper<'x>>
val inline nextThe: Zipper<'x> -> option<Zipper<'x, 'x>>
val inline prevAny: Zipper<'x> -> option<Zipper<'x>>
val inline prevThe: Zipper<'x> -> option<Zipper<'x, 'x>>
