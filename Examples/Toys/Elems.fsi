// Copyright (C) by Vesa Karvonen

module Toys.Elems

/// `elems w : array<'p>` returns an array of the elements of type `'p`
/// immediately contained in the given value `w` of a product or sum type `'w`.
///
/// By design, `elems` does not descend into the elements of `'w`.  The
/// following examples should help to clarify what this means:
///
///> elems (1, 2.0, 3, 4) : array<int> = [|1;3;4|]
///> elems ((1, 2.0), 3, 4) : array<int> = [|3;4|]
///> elems ((1, 2.0), (3, 4)) : array<int> = [||]
val elems: 'w -> array<'p>

/// `subst ps w` returns a new value of type `'w` which is like the given value
/// `w` except that the immediately contained elements of type `'p` in `w` are
/// replaced with values from the given array `ps`.  `subst` is characterized by
/// the equation `subst (elems w) w = w`.  It is an error if the number of
/// elements in `ps` does not exactly match the number of immediate elements of
/// type `'p` within `w`.
val subst: array<'p> -> 'w -> 'w
