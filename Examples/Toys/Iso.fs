// Copyright (C) by Vesa Karvonen

module Toys.Iso

open Infers
open Infers.Rep

type Iso<'x, 'y> = ('x -> 'y) * ('y -> 'x)

type [<PureInferenceRules>] Iso () =
  member g.Id () : Iso<'x, 'x> = (id, id)
  member g.Inv ((t, o): Iso<'x, 'y>) : Iso<'y, 'x> = (o, t)
  member g.Pairs ((t, o): Iso<     'pairs,         'ands>)
                        : Iso<'e * 'pairs, And<'e, 'ands>> =
    ((fun (e, es) ->
        let mutable ands = Unchecked.defaultof<And<_, _>>
        ands.Elem <- e
        ands.Rest <- t es
        ands),
     (fun ands -> (ands.Elem, o ands.Rest)))
