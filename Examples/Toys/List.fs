// Copyright (C) by Vesa Karvonen

module Toys.List

open Infers
open Infers.Toys

type Nil = Nil'0
type Cons<'h, 't> = Cons'2

type List<'x0> = Cons<'x0, Nil>
type List<'x0, 'x1> = Cons<'x0, List<'x1>>
type List<'x0, 'x1, 'x2> = Cons<'x0, List<'x1, 'x2>>
type List<'x0, 'x1, 'x2, 'x3> = Cons<'x0, List<'x1, 'x2, 'x3>>
type List<'x0, 'x1, 'x2, 'x3, 'x4> = Cons<'x0, List<'x1, 'x2, 'x3, 'x4>>

type IsList<'xs> = IsList'1

type Append<'xs, 'ys, 'zs> = Append'3

type Member<'x, 'xs> = Member'2

type Sublist<'xs, 'ys> = Sublist'2

type Adjacent<'a, 'b, 'xs> = Adjacent'3

type [<Basic>] List () =
  inherit Rules ()

  static member IsList (): IsList<Nil> = IsList'1
  static member IsList (_: IsList<'xs>)
                         : IsList<Cons<_, 'xs>> = IsList'1

  static member Append (_: IsList<'ys>)
                         : Append<Nil, 'ys, 'ys> = Append'3
  static member Append (_: Append<'xs, 'ys, 'zs>)
                         : Append<Cons<'x, 'xs>, 'ys, Cons<'x, 'zs>> = Append'3

  static member Sublist (_: Append<_, 'xs, 'qs>,
                         _: Append<'qs, _, 'ys>)
                          : Sublist<'xs, 'ys> = Sublist'2

  static member Member (_: Sublist<List<'x>, 'xs>)
                         : Member<'x, 'xs> = Member'2

  static member Adjacent (_: Choice<Sublist<List<'a, 'b>, 'xs>,
                                    Sublist<List<'b, 'a>, 'xs>>)
                           : Adjacent<'a, 'b, 'xs> = Adjacent'3
