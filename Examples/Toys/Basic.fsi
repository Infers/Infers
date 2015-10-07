// Copyright (C) by Vesa Karvonen

module Toys.Basic

open Infers

/// Equality: `Eq<'x, 'y>` is derivable when `'x` unifies with `'y`.
type Eq<'x, 'y>

/// Ternary equality: `Eq<'x, 'y, 'z>` is equivalent to `And<Eq<'x, 'y>, Eq<'y,
/// 'z>>`.
type Eq<'x, 'y, 'z>

/// Logical (inclusive) OR: `Or<'x, 'y>` is derivable iff either `'x` or `'y` or
/// both are.
type Or<'l, 'r>

/// Logical AND: `And<'x, 'y>` is derivable iff both `'x` and `'y` are.
type And<'l, 'r>

/// Some basic rules for logic programming.
type [<InferenceRules>] Basic =
  new: unit -> Basic

  member Eq'2: unit -> Eq<'x, 'x>
  member Eq'3: unit -> Eq<'x, 'x, 'x>

  member Or'L: 'l -> Or<'l, 'r>
  member Or'R: 'r -> Or<'l, 'r>

  member And'2: 'l * 'r -> And<'l, 'r>
