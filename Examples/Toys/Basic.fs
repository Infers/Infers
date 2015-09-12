// Copyright (C) by Vesa Karvonen

module Toys.Basic

open Infers

type Eq<'x, 'y> = | Eq2
type Eq<'x, 'y, 'z> = | Eq3

type Or<'l, 'r> = | Or2
type And<'l, 'r> = | And2

type [<InferenceRules>] Basic () =
  member g.Eq2 () : Eq<'x, 'x> = Eq2
  member g.Eq3 () : Eq<'x, 'x, 'x> = Eq3

  member g.OrL (_: 'l) : Or<'l, 'r> = Or2
  member g.OrR (_: 'r) : Or<'l, 'r> = Or2

  member f.And (_: 'l, _: 'r) : And<'l, 'r> = And2
