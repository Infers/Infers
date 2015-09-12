// Copyright (C) by Vesa Karvonen

module Toys.List

open Infers
open Toys.Basic

/// The empty list constructor.
type Nil = | Nil

/// The list constructor.
type Cons<'h, 't> = | Cons

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

/// `Member<'x, 'xs>` is derivable when `'x` is a member of the list `'xs`.
type Member<'x, 'xs> = | Member

/// `Next<'x, 'y, 'xs>` is derivable when `'x` and `'y` are adjacent members of
/// the list `'xs`.
type Next<'x, 'y, 'xs> = | Next

/// `Right<'x, 'y, 'xs`>` is derivable when `List<'x, 'y>` is a sublist of the
/// list `'xs`.
type Right<'x, 'y, 'xs> = | Right

/// A few rules for deriving lists.
type [<InferenceRules>] List () =
  member g.Member () : Member<'x, Cons<'x, _>> = Member
  member g.Member (_: Member<'x, 'xs>) : Member<'x, Cons<_, 'xs>> = Member

  member g.Right () : Right<'x, 'y, Cons<'x, Cons<'y, _>>> = Right
  member g.Right (_: Right<'x, 'y, 'xs>) : Right<'x, 'y, Cons<_, 'xs>> = Right

  member g.Next (_: Or<Right<'x, 'y, 'xs>,
                       Right<'y, 'x, 'xs>>)
                  : Next<'x, 'y, 'xs> = Next
