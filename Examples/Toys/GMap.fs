// Copyright (C) by Vesa Karvonen

module Toys.GMap

open Infers
open Infers.Rep

type GMap<'w, 'p> = ('p -> 'p) -> 'w -> 'w

type GM<'w, 'p> = {gm: GMap<'w, 'p>}
type ProductGM<'e, 'r, 'w, 'p> = P of GMap<'e, 'p>
type SumGM<'c, 's, 'w, 'p> = S of list<GMap<'w, 'p>>

type [<InferenceRules>] GMap () =
  member g.Same () : GMap<'p, 'p> = id
  member g.NotSame (_: Rep, _: RecFn, gm: GM<'w, 'p>) = gm.gm

  member g.Prim (_: Prim<'w>) : GM<'w, 'p> = {gm = fun _ -> id}

  member g.Elem (_: Elem<'e, 'r, 'w>,
                 eG: GMap<'e, 'p>) : ProductGM<'e, 'r, 'w, 'p> =
    P eG

  member g.Times (P eG: ProductGM<    'e     , And<'e, 'r>, 'w, 'p>,
                  P rG: ProductGM<        'r ,         'r , 'w, 'p>)
                      : ProductGM<And<'e, 'r>, And<'e, 'r>, 'w, 'p> =
    P <| fun p2p (And (e, r)) -> And (eG p2p e, rG p2p r)

  member g.Product (m: AsProduct<'r, 'w>, P rG: ProductGM<'r, 'r, 'w, 'p>) =
    {gm = fun p2p -> m.ToProduct >> rG p2p >> m.OfProduct}

  member g.Case (_: Case<Empty, 's, 'w>) : SumGM<Empty, 's, 'w, 'p> =
    S [fun _ -> id]

  member g.Case (m: Case<'e, 's, 'w>,
                 eG: ProductGM<'e, 'e, 'w, 'p>) : SumGM<'e, 's, 'w, 'p> =
    S [g.Product(m, eG).gm]

  member g.Plus (S eG: SumGM<       'e     , Choice<'e, 'r>, 'w, 'p>,
                 S rG: SumGM<           'r ,            'r , 'w, 'p>)
                     : SumGM<Choice<'e, 'r>, Choice<'e, 'r>, 'w, 'p> =
    S (eG @ rG)

  member g.Sum (m: Union<'w>,
                _: AsChoice<'s, 'w>,
                S sG: SumGM<'s, 's, 'w, 'p>) =
    let cs = Array.ofList sG
    {gm = fun p2p w -> cs.[m.Tag w] p2p w}

let gmap (p2p: 'p -> 'p) : 'w -> 'w = StaticRules<GMap>.Generate() p2p
