// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Elems =
  open Infers
  open Infers.Rep

  type Elems<'h, 'w> = 'w -> array<'h>

  type ElemsP<'e, 'r, 'o, 'h, 'w> = PE of list<Elem<'h, 'w>>
  type ElemsS<'p, 'o, 'h, 'w> = SE of list<Elems<'h, 'w>>

  let missE _ = [||]

  type [<Rep>] Elems () =
    inherit Rules ()

    static member Elem0(_: Elem<'e,'r,'o,'w>) : ElemsP<'e,'r,'o,'h,'w> =
      PE []

    static member Elem1(hE: Elem<'h,'r,'o,'w>) : ElemsP<'h,'r,'o,'h,'w> =
      PE [hE]

    static member Pair(PE eE: ElemsP<     'e    , Pair<'e,'r>,'o,'p,'w>,
                       PE rE: ElemsP<        'r ,         'r ,'o,'p,'w>) =
      PE <| eE @ rE         : ElemsP<Pair<'e,'r>, Pair<'e,'r>,'o,'p,'w>

    static member Product(_: AsPairs<'p,'o,'w>, PE pE: ElemsP<'p,'p,'o,'h,'w>) =
      let hEs = Array.ofList pE
      fun w -> hEs |> Array.map (fun hE -> hE.Get w)

    static member Case(_: Case<Empty, 'o, 'w>) : ElemsS<Empty, 'o, 'h, 'w> =
      SE [missE]

    static member Case(m: Case<'p,'o,'w>, pE: ElemsP<'p,'p,'o,'h,'w>) =
      SE [Elems.Product(m, pE)] : ElemsS<'p,'o,'h,'w>

    static member Choice(SE pE: ElemsS<       'p    , Choice<'p,'o>,'h,'w>,
                         SE oE: ElemsS<          'o ,           'o ,'h,'w>) =
      SE <| pE @ oE           : ElemsS<Choice<'p,'o>, Choice<'p,'o>,'h,'w>

    static member Sum(m: AsChoices<'s,'w>, SE sE: ElemsS<'s,'s,'h,'w>) =
      let sE = Array.ofList sE
      fun w -> sE.[m.Tag w] w

  let elems<'h, 'w> w = generateDFS<Elems, Elems<'h, 'w>> w

  //////////////////////////////////////////////////////////////////////////////

  let inline children w = elems<'w, 'w> w

  let rec elemsDn<'h, 'w> (w: 'w) : seq<'h> = Seq.delay <| fun () ->
    Seq.append (elems w) (Seq.collect elemsDn (children w))

  let universe w = Seq.append (Seq.singleton w) (elemsDn w)

  let rec para op w = children w |> Array.map (para op) |> op w

  //////////////////////////////////////////////////////////////////////////////

  type Subst<'h, 'w> = ('h -> 'h) -> 'w -> 'w

  type [<AbstractClass;AllowNullLiteral>] SubstP<'e, 'r, 'o, 'h, 'w> () =
    abstract Subst: ('h -> 'h) * byref<'e> -> unit

  type SubstS<'p, 'o, 'h, 'w> = SS of list<Subst<'h, 'w>>

  let missS _ w = w

  type [<Rep>] Subst () =
    inherit Rules ()
    static member Elem0 (_: Elem<'e, 'r, 'o, 'w>) : SubstP<'e, 'r, 'o, 'h, 'w> =
      null
    static member Elem1 (_: Elem<'h, 'r, 'o, 'w>) =
      {new SubstP<'h, 'r, 'o, 'h, 'w> () with
        member t.Subst (h2h, h) = h <- h2h h}
    static member Pair (eS: SubstP<     'e     , Pair<'e, 'r>, 'o, 'h, 'w>,
                        rS: SubstP<         'r ,          'r , 'o, 'h, 'w>)
                          : SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> =
      match (eS, rS) with
       | (null, null) -> null
       | (eS, null) ->
         {new SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> () with
            member t.Subst (h2h, er) = eS.Subst (h2h, &er.Elem)}
       | (null, rS) ->
         {new SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> () with
            member t.Subst (h2h, er) = rS.Subst (h2h, &er.Rest)}
       | (eS, rS) ->
         {new SubstP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'h, 'w> () with
            member t.Subst (h2h, er) =
             eS.Subst (h2h, &er.Elem)
             rS.Subst (h2h, &er.Rest)}
    static member Product (m: AsPairs<'p,'o,'w>, pS: SubstP<'p,'p,'o,'h,'w>) =
      match pS with
       | null -> missS
       | pS -> fun h2h w ->
         let mutable p = m.ToPairs w
         pS.Subst (h2h, &p)
         m.Create (&p)
    static member Case (_: Case<Empty, 'o, 'w>) : SubstS<Empty, 'o, 'h, 'w> =
      SS [missS]
    static member Case (m: Case<'p, 'o, 'w>, pS: SubstP<'p, 'p, 'o, 'h, 'w>) =
      SS [Subst.Product (m, pS)] : SubstS<'p, 'o, 'h, 'w>
    static member Choice (SS pS: SubstS<       'p    , Choice<'p,'o>,'h,'w>,
                          SS oS: SubstS<          'o ,           'o ,'h,'w>) =
      SS (pS @ oS)             : SubstS<Choice<'p,'o>, Choice<'p,'o>,'h,'w>
    static member Sum (m: AsChoices<'s, 'w>, SS s: SubstS<'s, 's, 'p, 'w>) =
      let s = Array.ofList s
      fun hs w -> s.[m.Tag w] hs w

  let subst<'h, 'w> h2h w = generateDFS<Subst, Subst<'h, 'w>> h2h w

  //////////////////////////////////////////////////////////////////////////////

  let inline descend w2w w = subst<'w, 'w> w2w w

  let rec substUp<'h, 'w> (h2h: 'h -> 'h) (w: 'w) =
    w |> descend (substUp h2h) |> subst h2h

  let inline transform w2w w = w |> substUp w2w |> w2w

  let rec rewrite w2wO w =
    transform <| fun w -> match w2wO w with
                           | None -> w
                           | Some w -> rewrite w2wO w
              <| w
