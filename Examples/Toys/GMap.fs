// Copyright (C) by Vesa Karvonen

module Toys.GMap

open Infers
open Infers.Rep

type GMap<'w, 'h> = ('h -> 'h) -> 'w -> 'w

type GM<'w, 'h> = {gm: GMap<'w, 'h>}
type GMP<'e, 'r, 'o, 'w, 'h> = P of GMap<'e, 'h>
type GMS<'p, 'o, 'w, 'h> = S of list<GMap<'w, 'h>>

type [<InferenceRules>] GMap () =
  member g.Same () : GMap<'h, 'h> = id
  member g.NotSame (_: Rep, _: RecFn, gm: GM<'w, 'h>) = gm.gm

  member g.Prim (_: Prim<'w>) : GM<'w, 'h> = {gm = fun _ -> id}

  member g.Elem (_: Elem<'e, 'r, 'o, 'w>, eG: GMap<'e, 'h>) =
    P eG : GMP<'e, 'r, 'o, 'w, 'h>

  member g.Pair (P eG: GMP<     'e     , Pair<'e, 'r>, 'o, 'w, 'h>,
                 P rG: GMP<         'r ,          'r , 'o, 'w, 'h>)
                     : GMP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'w, 'h> =
    P <| fun h2h (Pair (e, r)) -> Pair (eG h2h e, rG h2h r)

  member g.Product (m: AsProduct<'p, 'w>, P rG: GMP<'p, 'p, 'o, 'w, 'h>) =
    {gm = fun h2h -> m.ToProduct >> rG h2h >> m.OfProduct}

  member g.Case (_: Case<Empty, 'o, 'w>) : GMS<Empty, 'o, 'w, 'h> =
    S [fun _ -> id]

  member g.Case (m: Case<'p, 'o, 'w>, pG: GMP<'p, 'p, 'o, 'w, 'h>) =
    S [g.Product(m, pG).gm] : GMS<'p, 'o, 'w, 'h>

  member g.Choice (S pG: GMS<       'p     , Choice<'p, 'o>, 'w, 'h>,
                   S oG: GMS<           'o ,            'o , 'w, 'h>) =
    S (pG @ oG)        : GMS<Choice<'p, 'o>, Choice<'p, 'o>, 'w, 'h>

  member g.Sum (m: Union<'w>, _: AsSum<'s, 'w>, S sG: GMS<'s, 's, 'w, 'h>) =
    let sG = Array.ofList sG
    {gm = fun h2h w -> sG.[m.Tag w] h2h w}

let gmap (h2h: 'h -> 'h) (w: 'w) : 'w = StaticRules<GMap>.Generate() h2h w
