// Copyright (C) by Vesa Karvonen

module Toys.List

open Infers
open Toys.Basic

type Nil = | Nil
type Cons<'h, 't> = | Cons

type List<'x0> = Cons<'x0, Nil>
type List<'x0, 'x1> = Cons<'x0, List<'x1>>
type List<'x0, 'x1, 'x2> = Cons<'x0, List<'x1, 'x2>>
type List<'x0, 'x1, 'x2, 'x3> = Cons<'x0, List<'x1, 'x2, 'x3>>
type List<'x0, 'x1, 'x2, 'x3, 'x4> = Cons<'x0, List<'x1, 'x2, 'x3, 'x4>>

type Member<'x, 'xs> = | Member
type Next<'x, 'y, 'xs> = | Next
type Right<'x, 'y, 'xs> = | Right

type [<PureInferenceRules>] List () =
  member g.Member () : Member<'x, Cons<'x, _>> = Member
  member g.Member (_: Member<'x, 'xs>) : Member<'x, Cons<_, 'xs>> = Member

  member g.Right () : Right<'x, 'y, Cons<'x, Cons<'y, _>>> = Right
  member g.Right (_: Right<'x, 'y, 'xs>) : Right<'x, 'y, Cons<_, 'xs>> = Right

  member g.Next (_: Or<Right<'x, 'y, 'xs>,
                       Right<'y, 'x, 'xs>>)
                  : Next<'x, 'y, 'xs> = Next
