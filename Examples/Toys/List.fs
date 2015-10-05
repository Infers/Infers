// Copyright (C) by Vesa Karvonen

module Toys.List

open Infers
open Toys.Basic

/// The empty list constructor.
type Nil = | Nil'0

/// The list constructor.
type Cons<'h, 't> = | Cons'2

/// Shorthand for a 1-element list.
type List<'x0> = Cons<'x0, Nil>

/// Shorthand for a 2-element list.
type List<'x0, 'x1> = Cons<'x0, List<'x1>>

/// Shorthand for a 3-element list.
type List<'x0, 'x1, 'x2> = Cons<'x0, List<'x1, 'x2>>

/// Shorthand for a 4-element list.
type List<'x0, 'x1, 'x2, 'x3> = Cons<'x0, List<'x1, 'x2, 'x3>>

/// Shorthand for a 5-element list.
type List<'x0, 'x1, 'x2, 'x3, 'x4> = Cons<'x0, List<'x1, 'x2, 'x3, 'x4>>

/// `IsList<'xs>` is derivable when `'xs` is a proper list.
type IsList<'xs> = | IsList'1

/// `Append<'xs, 'ys, 'zs>` is derivable when appending `'xs` and `'ys` yields
/// `'zs`.
type Append<'xs, 'ys, 'zs> = | Append'3

/// `Member<'x, 'xs>` is derivable when `'x` is a member of the list `'xs`.
type Member<'x, 'xs> = | Member'2

/// `Sublist<'xs, 'ys>` is derivable when `'xs` is a sublist of `'ys`.
type Sublist<'xs, 'ys> = | Sublist'2

/// `Adjacent<'a, 'b, 'xs>` is derivable when `'a` and `'b` are adjacent in the
/// list `'xs`.
type Adjacent<'a, 'b, 'xs> = | Adjacent'3

/// A few rules for deriving lists.
type [<InferenceRules>] List () =
  member g.IsList (): IsList<Nil> = IsList'1
  member g.IsList (_: IsList<'xs>)
                    : IsList<Cons<_, 'xs>> = IsList'1

  member g.Append (_: IsList<'ys>)
                    : Append<Nil, 'ys, 'ys> = Append'3
  member g.Append (_: Append<'xs, 'ys, 'zs>)
                    : Append<Cons<'x, 'xs>, 'ys, Cons<'x, 'zs>> = Append'3

  member g.Sublist (_: Append<'ps, 'xs, 'qs>,
                    _: Append<'qs, 'rs, 'ys>)
                     : Sublist<'xs, 'ys> = Sublist'2

  member g.Member (_: Sublist<List<'x>, 'xs>)
                    : Member<'x, 'xs> = Member'2

  member g.Adjacent (_: Or<Sublist<List<'a, 'b>, 'xs>,
                           Sublist<List<'b, 'a>, 'xs>>)
                      : Adjacent<'a, 'b, 'xs> = Adjacent'3
