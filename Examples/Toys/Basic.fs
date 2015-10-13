// Copyright (C) by Vesa Karvonen

module Toys.Basic

open Infers

type Eq<'x, 'y> = Eq'2
type Eq<'x, 'y, 'z> = Eq'3
type Or<'l, 'r> = Or'2

type [<InferenceRules>] Basic () =
  member g.Eq'2 () : Eq<'x, 'x> = Eq'2
  member g.Eq'3 () : Eq<'x, 'x, 'x> = Eq'3

  member g.Or'L (_: 'l) : Or<'l, 'r> = Or'2
  member g.Or'R (_: 'r) : Or<'l, 'r> = Or'2
