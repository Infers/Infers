// Copyright (C) by Vesa Karvonen

module Toys.GMap

open Infers.Toys
open Infers
open Infers.Rep

type GMap<'w, 'h> = ('h -> 'h) -> 'w -> 'w

type GM<'w, 'h> = {gm: GMap<'w, 'h>}
type GMP<'e, 'r, 'o, 'w, 'h> = P of GMap<'e, 'h>
type GMS<'p, 'o, 'w, 'h> = S of list<GMap<'w, 'h>>

type [<Rep; Rec>] GMap () =
  inherit Rules ()
  static member Same () : GMap<'h, 'h> = id
  static member NotSame (gm: GM<'w, 'h>) = gm.gm

  static member Prim (_: Prim<'w>) : GM<'w, 'h> = {gm = fun _ -> id}

  static member String () : GM<string, 'h> = {gm = fun _ -> id}

  static member Elem (_: Elem<'e, 'r, 'o, 'w>, eG: GMap<'e, 'h>) =
    P eG : GMP<'e, 'r, 'o, 'w, 'h>

  static member Pair (P eG: GMP<     'e     , Pair<'e, 'r>, 'o, 'w, 'h>,
                      P rG: GMP<         'r ,          'r , 'o, 'w, 'h>)
                          : GMP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'w, 'h> =
    P <| fun h2h (Pair (e, r)) -> Pair (eG h2h e, rG h2h r)

  static member Product (m: AsPairs<'p, 'o, 'w>, P rG: GMP<'p, 'p, 'o, 'w, 'h>) =
    {gm = fun h2h -> m.ToPairs >> rG h2h >> m.OfPairs}

  static member Case (_: Case<Empty, 'o, 'w>) : GMS<Empty, 'o, 'w, 'h> =
    S [fun _ -> id]

  static member Case (m: Case<'p, 'o, 'w>, pG: GMP<'p, 'p, 'o, 'w, 'h>) =
    S [GMap.Product(m, pG).gm] : GMS<'p, 'o, 'w, 'h>

  static member Choice (S pG: GMS<       'p     , Choice<'p, 'o>, 'w, 'h>,
                        S oG: GMS<           'o ,            'o , 'w, 'h>) =
    S (pG @ oG)             : GMS<Choice<'p, 'o>, Choice<'p, 'o>, 'w, 'h>

  static member Sum (m: AsChoices<'s, 'w>, S sG: GMS<'s, 's, 'w, 'h>) =
    let sG = Array.ofList sG
    {gm = fun h2h w -> sG.[m.Tag w] h2h w}

let gmap h2h w = generateDFS<GMap, GMap<_, _>> h2h w
