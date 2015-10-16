// Copyright (C) by Vesa Karvonen

module Toys.Elems

/// `elems w : array<'p>` returns an array of the elements of type `'p`
/// immediately contained in the given value `w` of a product or sum type `'w`.
///
/// By design, `elems` does not descent into the elements of `'w`.  The
/// following examples should help to clarify what this means:
///
///> elems (1, 2.0, 3, 4) : array<int> = [|1;3;4|]
///> elems ((1, 2.0), 3, 4) : array<int> = [|3;4|]
///> elems ((1, 2.0), (3, 4)) : array<int> = [||]
val elems: 'w -> array<'p>
