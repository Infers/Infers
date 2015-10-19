// Copyright (C) by Vesa Karvonen

module Toys.List

open Infers
open Toys.Basic

/// The empty list constructor.
type Nil

/// The list constructor.
type Cons<'h, 't>

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
type IsList<'xs>

/// `Append<'xs, 'ys, 'zs>` is derivable when appending `'xs` and `'ys` yields
/// `'zs`.
type Append<'xs, 'ys, 'zs>

/// `Member<'x, 'xs>` is derivable when `'x` is a member of the list `'xs`.
type Member<'x, 'xs>

/// `Sublist<'xs, 'ys>` is derivable when `'xs` is a sublist of `'ys`.
type Sublist<'xs, 'ys>

/// `Adjacent<'a, 'b, 'xs>` is derivable when `'a` and `'b` are adjacent in the
/// list `'xs`.
type Adjacent<'a, 'b, 'xs>

/// A few rules for deriving lists.
type [<InferenceRules>] List =
  new: unit -> List

  member IsList: unit -> IsList<Nil>
  member IsList: IsList<'xs> -> IsList<Cons<_, 'xs>>

  member Append: IsList<'ys> -> Append<Nil, 'ys, 'ys>
  member Append: Append<'xs, 'ys, 'zs>
              -> Append<Cons<'x, 'xs>, 'ys, Cons<'x, 'zs>>

  member Sublist: Append<_, 'xs, 'qs>
                * Append<'qs, _, 'ys>
               -> Sublist<'xs, 'ys>

  member Member: Sublist<List<'x>, 'xs> -> Member<'x, 'xs>

  member Adjacent: Choice<Sublist<List<'a, 'b>, 'xs>,
                          Sublist<List<'b, 'a>, 'xs>> -> Adjacent<'a, 'b, 'xs>
