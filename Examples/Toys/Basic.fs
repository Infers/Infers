// Copyright (C) by Vesa Karvonen

module Toys.Basic

open Infers

/// Equality: `Eq<'x, 'y>` is derivable when `'x` unifies with `'y`.
type Eq<'x, 'y> = | Eq2

/// Ternary equality: `Eq<'x, 'y, 'z>` is equivalent to `And<Eq<'x, 'y>, Eq<'y,
/// 'z>>`.
type Eq<'x, 'y, 'z> = | Eq3

/// Logical (inclusive) OR: `Or<'x, 'y>` is derivable iff either `'x` or `'y` or
/// both are.
type Or<'l, 'r> = | Or2

/// Logical AND: `And<'x, 'y>` is derivable iff both `'x` and `'y` are.
type And<'l, 'r> = | And2

/// Some basic rules for logic programming.
type [<InferenceRules>] Basic () =
  member g.Eq2 () : Eq<'x, 'x> = Eq2
  member g.Eq3 () : Eq<'x, 'x, 'x> = Eq3

  member g.OrL (_: 'l) : Or<'l, 'r> = Or2
  member g.OrR (_: 'r) : Or<'l, 'r> = Or2

  member f.And (_: 'l, _: 'r) : And<'l, 'r> = And2
