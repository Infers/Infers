// Copyright (C) by Vesa Karvonen

module Toys.Elems

/// `fetch w : array<'p>` returns an array of the elements of type `'p`
/// immediately contained in the given value `w` of a product or sum type `'w`.
///
/// By design, `fetch` does not descend into the elements of `'w`.  The
/// following examples should help to clarify what this means:
///
///> fetch (1, 2.0, 3, 4) : array<int> = [|1;3;4|]
///> fetch ((1, 2.0), 3, 4) : array<int> = [|3;4|]
///> fetch ((1, 2.0), (3, 4)) : array<int> = [||]
val fetch: 'w -> array<'p>

/// `subst ps w` returns a new value of type `'w` which is like the given value
/// `w` except that the immediately contained elements of type `'p` in `w` are
/// replaced with values from the given array `ps`.  `subst` is characterized by
/// the equation `subst (fetch w) w = w`.  It is an error if the number of
/// elements in `ps` does not exactly match the number of immediate elements of
/// type `'p` within `w`.
val subst: array<'p> -> 'w -> 'w

/// `map p2p w` is equivalent to `subst (fetch w |> Array.map p2p) w`.
val map: ('p -> 'p) -> 'w -> 'w
