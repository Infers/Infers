// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Zipper =

  //////////////////////////////////////////////////////////////////////////////
  // Zipper classes

  /// Zipper over type `'w` pointing at an unknown type of hole.
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
    abstract GetObj: unit -> obj

  /// Zipper over type `'w` pointing at a hole of type `'h`.
  and [<AbstractClass>] Zipper<'w, 'h> =
    inherit Zipper<'w>
    abstract Get: unit -> 'h
    abstract Set: 'h -> Zipper<'w, 'h>

  //////////////////////////////////////////////////////////////////////////////
  // Common interface

  val fromZipper: Zipper<'w> -> 'w

  //////////////////////////////////////////////////////////////////////////////
  // Heterogeneous interface

  val toZipperAny: 'w -> Zipper<'w>

  val getAny: Zipper<'w> -> option<'h>

  val setAny: 'h -> Zipper<'w> -> option<Zipper<'w>>

  val inline downHeadAny: Zipper<'w> -> option<Zipper<'w>>
  val inline downLastAny: Zipper<'w> -> option<Zipper<'w>>

  val inline upAny: Zipper<'w> -> option<Zipper<'w>>

  val inline nextAny: Zipper<'w> -> option<Zipper<'w>>
  val inline prevAny: Zipper<'w> -> option<Zipper<'w>>

  //////////////////////////////////////////////////////////////////////////////
  // Homogeneous interface

  val toZipperThe: 'w -> Zipper<'w, 'w>

  val inline getThe: Zipper<'w, 'w> -> 'w
  val inline setThe: 'w -> Zipper<'w, 'w> -> Zipper<'w, 'w>

  val inline downHeadThe: Zipper<'w> -> option<Zipper<'w, 'w>>
  val inline downLastThe: Zipper<'w> -> option<Zipper<'w, 'w>>

  val upThe: Zipper<'w> -> option<Zipper<'w, 'w>>

  val inline nextThe: Zipper<'w> -> option<Zipper<'w, 'w>>
  val inline prevThe: Zipper<'w> -> option<Zipper<'w, 'w>>

  val mapBottomUpThe: ('w -> 'w) -> Zipper<'w, 'w> -> Zipper<'w, 'w>
  val mapTopDownThe: ('w -> 'w) -> Zipper<'w, 'w> -> Zipper<'w, 'w>
