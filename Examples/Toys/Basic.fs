// Copyright (C) by Vesa Karvonen

module Toys.Basic

open Infers

/// Equality: `Eq<'x, 'y>` is derivable when `'x` unifies with `'y`.
type Eq<'x, 'y> = | Eq'2

/// Ternary equality: `Eq<'x, 'y, 'z>` is equivalent to `And<Eq<'x, 'y>, Eq<'y,
/// 'z>>`.
type Eq<'x, 'y, 'z> = | Eq'3

/// Logical (inclusive) OR: `Or<'x, 'y>` is derivable iff either `'x` or `'y` or
/// both are.
type Or<'l, 'r> = | Or'2

/// Logical AND: `And<'x, 'y>` is derivable iff both `'x` and `'y` are.
type And<'l, 'r> = | And'2

/// Some basic rules for logic programming.
type [<InferenceRules>] Basic () =
  member g.Eq'2 () : Eq<'x, 'x> = Eq'2
  member g.Eq'3 () : Eq<'x, 'x, 'x> = Eq'3

  member g.Or'L (_: 'l) : Or<'l, 'r> = Or'2
  member g.Or'R (_: 'r) : Or<'l, 'r> = Or'2

  member f.And (_: 'l, _: 'r) : And<'l, 'r> = And'2
