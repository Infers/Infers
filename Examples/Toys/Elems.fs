// Copyright (C) by Vesa Karvonen

module Toys.Elems

open Infers
open Infers.Rep

type Fetch<'h, 'w> = 'w -> array<'h>

type FetchP<'e, 'r, 'o, 'h, 'w> =
  | Miss
  | Hit of num: int * ext: ('w -> int -> array<'h> -> unit)
type FetchS<'p, 'o, 'h, 'w> = SE of list<Fetch<'h, 'w>>

let missE _ = [||]

type [<InferenceRules>] Fetch () =
  inherit Rep ()
  member g.Elem0 (_: Elem<'e, 'r, 'o, 'w>) : FetchP<'e, 'r, 'o, 'h, 'w> =
    Miss
  member g.Elem1 (hE: Elem<'h, 'r, 'o, 'w>) : FetchP<'h, 'r, 'o, 'h, 'w> =
    Hit (1, fun w i hs -> hs.[i] <- hE.Get w)
  member g.Pair (eF: FetchP<     'e     , Pair<'e, 'r>, 'o, 'p, 'w>,
                 rF: FetchP<         'r ,          'r , 'o, 'p, 'w>)
                   : FetchP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'p, 'w> =
    match (eF, rF) with
     | (Miss, Miss) -> Miss
     | (Hit (n, e), Miss) | (Miss, Hit (n, e)) -> Hit (n, e)
     | (Hit (numE, extE), Hit (numR, extR)) ->
       Hit (numE + numR, fun w i hs -> extE w i hs; extR w (i + numE) hs)
  member g.Product (_: AsProduct<'p, 'o, 'w>, pF: FetchP<'p, 'p, 'o, 'h, 'w>) =
    match pF with
     | Miss -> missE
     | Hit (n, ext) -> fun w -> let hs = Array.zeroCreate n in ext w 0 hs; hs
  member g.Case (_: Case<Empty, 'o, 'w>) : FetchS<Empty, 'o, 'h, 'w> =
    SE [missE]
  member g.Case (m: Case<'p, 'o, 'w>, pF: FetchP<'p, 'p, 'o, 'h, 'w>) =
    SE [g.Product (m, pF)] : FetchS<'p, 'o, 'h, 'w>
  member g.Choice (SE pF: FetchS<       'p     , Choice<'p, 'o>, 'h, 'w>,
                   SE oF: FetchS<           'o ,            'o , 'h, 'w>) =
    SE (pF @ oF)        : FetchS<Choice<'p, 'o>, Choice<'p, 'o>, 'h, 'w>
  member g.Sum (m: AsSum<'s, 'w>, SE sF: FetchS<'s, 's, 'h, 'w>) =
    let sF = Array.ofList sF
    fun w -> sF.[m.Tag w] w

let fetch (w: 'w) : array<'h> =
  (StaticRules<Fetch>.Generate () : Fetch<'h, 'w>) w

////////////////////////////////////////////////////////////////////////////////

type Subst<'h, 'w> = array<'h> -> 'w -> 'w

type [<AbstractClass;AllowNullLiteral>] SubstP<'e, 'r, 'o, 'h, 'w> () =
  abstract Subst: array<'h> * int * byref<'r> -> int

type SubstS<'p, 'o, 'h, 'w> = SS of list<Subst<'h, 'w>>

let check (hs: array<_>) n =
  if n <> hs.Length then
    failwithf "Expected %d elems, but was given %d elems" n hs.Length

let missS hs w = check hs 0; w

type [<InferenceRules>] Subst () =
  inherit Rep ()
  member g.Elem0 (_: Elem<'e, 'r, 'o, 'w>) : SubstP<'e, 'r, 'o, 'h, 'w> =
    null
  member g.Elem1 (_: Elem<'h, 'h, 'o, 'w>) =
    {new SubstP<'h, 'h, 'o, 'h, 'w> () with
      member t.Subst (hs, i, h) = h <- hs.[i]; i+1}
  member g.Elem1 (_: Elem<'h, Pair<'h, 'r>, 'o, 'w>) =
    {new SubstP<'h, Pair<'h, 'r>, 'o, 'h, 'w> () with
      member t.Subst (hs, i, hr) = hr.Elem <- hs.[i] ; i+1}
  member g.Pair (eS: SubstP<     'e     , Pair<'e, 'r>, 'o, 'h, 'w>,
                 rS: SubstP<         'r ,          'r , 'o, 'h, 'w>)
                   : SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> =
    match (eS, rS) with
     | (null, null) -> null
     | (eS, null) ->
       {new SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> () with
          member t.Subst (hs, i, r) = eS.Subst (hs, i, &r)}
     | (null, rS) ->
       {new SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> () with
          member t.Subst (hs, i, r) = rS.Subst (hs, i, &r.Rest)}
     | (eS, rS) ->
       {new SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> () with
          member t.Subst (hs, i, r) =
           rS.Subst (hs, eS.Subst (hs, i, &r), &r.Rest)}
  member g.Product (m: AsProduct<'p, 'o, 'w>, pS: SubstP<'p, 'p, 'o, 'h, 'w>) =
    match pS with
     | null -> missS
     | pS -> fun hs w ->
       let mutable r = m.ToProduct w
       check hs <| pS.Subst (hs, 0, &r)
       m.Create (&r)
  member g.Case (_: Case<Empty, 'o, 'w>) : SubstS<Empty, 'o, 'h, 'w> =
    SS [missS]
  member g.Case (m: Case<'p, 'o, 'w>, pS: SubstP<'p, 'p, 'o, 'h, 'w>) =
    SS [g.Product (m, pS)] : SubstS<'p, 'o, 'h, 'w>
  member g.Choice (SS pS: SubstS<       'p     , Choice<'p, 'o>, 'h, 'w>,
                   SS oS: SubstS<           'o ,            'o , 'h, 'w>) =
    SS (pS @ oS)        : SubstS<Choice<'p, 'o>, Choice<'p, 'o>, 'h, 'w>
  member g.Sum (m: AsSum<'s, 'w>, SS s: SubstS<'s, 's, 'p, 'w>) =
    let s = Array.ofList s
    fun hs w -> s.[m.Tag w] hs w

let subst (hs: array<'h>) (w: 'w) : 'w =
  (StaticRules<Subst>.Generate () : Subst<'h, 'w>) hs w

////////////////////////////////////////////////////////////////////////////////

let map (h2h: 'h -> 'h) (w: 'w) : 'w =
  subst (fetch w |> Array.map h2h) w
