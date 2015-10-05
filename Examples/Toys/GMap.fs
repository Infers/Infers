// Copyright (C) by Vesa Karvonen

module Toys.GMap

open Infers
open Infers.Rep
open Toys.Rec

type GM<'w, 'p> = ('p -> 'p) -> 'w -> 'w
type GMap<'w, 'p> = {gm: GM<'w, 'p>}
type PGM<'e, 'r, 'w, 'p> = PGM of GM<'e, 'p>
type SGM<'c, 's, 'w, 'p> = SGM of list<GM<'w, 'p>>

type [<InferenceRules>] GMap () =
  member g.Same ()                                  = {gm = id}
  member g.NotSame (_: Rep, _: Rec, gm: GM<'w, 'p>) = {gm = gm}

  member g.Prim (_: Prim<'w>) : GM<'w, 'p> = fun _ -> id

  member g.Elem (_: Elem<'e, 'r, 'w>,
                 eG: GMap<'e, 'p>) : PGM<'e, 'r, 'w, 'p> =
    PGM eG.gm

  member g.Times (PGM eG: PGM<    'e     , And<'e, 'r>, 'w, 'p>,
                  PGM rG: PGM<        'r ,         'r , 'w, 'p>)
                        : PGM<And<'e, 'r>, And<'e, 'r>, 'w, 'p> =
    PGM <| fun p2p (And (e, r)) -> And (eG p2p e, rG p2p r)

  member g.Product (m: AsProduct<'r, 'w>, PGM rG: PGM<'r, 'r, 'w, 'p>) =
    fun p2p -> m.ToProduct >> rG p2p >> m.OfProduct

  member g.Case (_: Case<Empty, 's, 'w>) : SGM<Empty, 's, 'w, 'p> =
    SGM [fun _ -> id]

  member g.Case (m: Case<'e, 's, 'w>,
                 eG: PGM<'e, 'e, 'w, 'p>) : SGM<'e, 's, 'w, 'p> =
    SGM [g.Product (m, eG)]

  member g.Plus (SGM eG: SGM<       'e     , Choice<'e, 'r>, 'w, 'p>,
                 SGM rG: SGM<           'r ,            'r , 'w, 'p>)
                       : SGM<Choice<'e, 'r>, Choice<'e, 'r>, 'w, 'p> =
    SGM (eG @ rG)

  member g.Sum (m: Union<'w>,
                _: AsChoice<'s, 'w>,
                SGM sG: SGM<'s, 's, 'w, 'p>) =
    let cs = Array.ofList sG
    fun p2p w -> cs.[m.Tag w] p2p w

let gmap (p2p: 'p -> 'p) : 'w -> 'w = StaticRules<GMap>.Generate().gm p2p

let test () : unit =
  gmap ((+) 1) [("vesa", 1)] |> printfn "%A"
  gmap (fun (x: string) -> x.ToUpper ()) [("vesa", 1)] |> printfn "%A"
