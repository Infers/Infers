// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module Infers.Toys.Elems

/// `fetch<'h, 'w> w` returns an array of the elements of type `'h` immediately
/// contained in the given value `w` of a product or sum of products type `'w`.
///
/// `subst` and `fetch` are related by the equation `subst (fetch w) w = w`.
///
/// By design, `fetch` does not descend into the elements of `'w`.  The
/// following examples should help to clarify what this means:
///
///> fetch<int, _> (Some 2) = [|2|]
///> fetch<int, _> (Some (1, 2)) = [||]
///> fetch<int, _> (1, 2.0, 3, 4) = [|1;3;4|]
///> fetch<int, _> ((1, 2.0), 3, 4) = [|3;4|]
///> fetch<int, _> ((1, 2.0), (3, 4)) = [||]
val fetch<'h, 'w> : 'w -> array<'h>

/// `subst hs w` returns a new value of type `'w` which is like the given value
/// `w` except that the immediately contained elements of type `'h` in the
/// product or sum of producs type `w` are replaced with values from the given
/// array `hs`.
///
/// `subst` and `fetch` are related by the equation `subst (fetch w) w = w`.
///
/// It is an error if the number of elements in `hs` does not exactly match the
/// number of immediate elements of type `'h` within `w`.
val subst<'h, 'w> : array<'h> -> 'w -> 'w

/// `map h2h w` is equivalent to `subst (fetch w |> Array.map p2p) w`.
val inline map<'h, 'w> : ('h -> 'h) -> 'w -> 'w
