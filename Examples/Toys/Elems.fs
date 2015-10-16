// Copyright (C) by Vesa Karvonen

module Toys.Elems

open Infers
open Infers.Rep

type Fetch<'p, 'w> = 'w -> array<'p>

type ProductF<'e, 'r, 'cs, 'p, 'w> =
  | Miss
  | Hit of num: int * ext: ('w -> int -> array<'p> -> unit)
type SumF<'c, 's, 'p, 'w> = SE of list<Fetch<'p, 'w>>

let missE _ = [||]
let makeE (re: ProductF<'r, 'r, 'cs, 'p, 'w>) =
  match re with
   | Miss -> missE
   | Hit (num, ext) -> fun w ->
     let ps = Array.zeroCreate num
     ext w 0 ps
     ps

type [<InferenceRules>] Fetch () =
  inherit Rep ()
  member g.ElemMiss (_: Elem<'e, 'er, 'cs, 'w>) : ProductF<'e, 'er, 'cs, 'p, 'w> =
    Miss
  member g.ElemHit (e: Elem<'p, 'er, 'cs, 'w>) : ProductF<'p, 'er, 'cs, 'p, 'w> =
    Hit (1, fun w i ps -> ps.[i] <- e.Get w)
  member g.Times (ee: ProductF<    'e     , And<'e, 'r>, 'cs, 'p, 'w>,
                  re: ProductF<        'r ,         'r , 'cs, 'p, 'w>)
                    : ProductF<And<'e, 'r>, And<'e, 'r>, 'cs, 'p, 'w> =
    match (ee, re) with
     | (Miss, Miss) -> Miss
     | (Hit (n, e), Miss)
     | (Miss, Hit (n, e)) -> Hit (n, e)
     | (Hit (numE, extE), Hit (numR, extR)) ->
       Hit (numE + numR,
            fun w i ps ->
              extE w  i         ps
              extR w (i + numE) ps)
  member g.Product (_: Product<'w>,
                    _: AsProduct<'r, 'w>, r: ProductF<'r, 'r, 'w, 'p, 'w>) =
    makeE r

  member g.Case (_: Case<Empty, 's, 'w>) : SumF<Empty, 's, 'p, 'w> =
    SE [missE]
  member g.Case (_: Case<'r, 's, 'w>,
                 r: ProductF<'r, 'r, 's, 'p, 'w>) : SumF<'r, 's, 'p, 'w> =
    SE [makeE r]
  member g.Plus (SE c: SumF<        'c    , Choice<'c, 's>, 'p, 'w>,
                 SE s: SumF<           's ,            's , 'p, 'w>)
                     : SumF<Choice<'c, 's>, Choice<'c, 's>, 'p, 'w> =
    SE (c @ s)
  member g.Sum (m: Union<'w>, _: AsChoice<'s, 'w>, SE s: SumF<'s, 's, 'p, 'w>) =
    let s = Array.ofList s
    fun w -> s.[m.Tag w] w

  member g.Prim (_: Prim<'w>) : Fetch<'p, 'w> =
    failwithf "Fetch only works on sums and products. Given: %A." typeof<'w>

let fetch (w: 'w) : array<'p> =
  (StaticRules<Fetch>.Generate () : Fetch<'p, 'w>) w

////////////////////////////////////////////////////////////////////////////////

type Subst<'p, 'w> = array<'p> -> 'w -> 'w

type [<AbstractClass;AllowNullLiteral>] ProductS<'e, 'r, 'cs, 'p, 'w> () =
  abstract Subst: array<'p> * int * byref<'r> -> int

type SumS<'c, 's, 'p, 'w> = SS of list<Subst<'p, 'w>>

let check (ps: array<_>) n =
  if n <> ps.Length then
    failwithf "Expected %d elems, but was given %d elems" n ps.Length

let missS ps w = check ps 0; w
let makeS (m: AsProduct<'r, 'w>) (rs: ProductS<'r, 'r, 'cs, 'p, 'w>) =
  match rs with
   | null -> missS
   | rs -> fun ps w ->
     let mutable r = m.ToProduct w
     check ps <| rs.Subst (ps, 0, &r)
     m.Create (&r)

type [<InferenceRules>] Subst () =
  inherit Rep ()
  member g.ElemMiss (_: Elem<'e, 'er, 'cs, 'w>) : ProductS<'e, 'er, 'cs, 'p, 'w> = null
  member g.ElemHit (_: Elem<'p, 'p, 'cs, 'w>) =
    {new ProductS<'p, 'p, 'cs, 'p, 'w> () with
      member t.Subst (ps, i, p) = p <- ps.[i]; i+1}
  member g.ElemHit (_: Elem<'p, And<'p, 'r>, 'cs, 'w>) =
    {new ProductS<'p, And<'p, 'r>, 'cs, 'p, 'w> () with
      member t.Subst (ps, i, p) = p.Elem <- ps.[i] ; i+1}
  member g.Times (es: ProductS<    'e     , And<'e, 'r>, 'cs, 'p, 'w>,
                  rs: ProductS<        'r ,         'r , 'cs, 'p, 'w>)
                    : ProductS<And<'e, 'r>, And<'e, 'r>, 'cs, 'p, 'w> =
    match (es, rs) with
     | (null, null) -> null
     | (es, null) ->
       {new ProductS<And<'e, 'r>, And<'e, 'r>, 'cs, 'p, 'w> () with
          member t.Subst (ps, i, r) = es.Subst (ps, i, &r)}
     | (null, rs) ->
       {new ProductS<And<'e, 'r>, And<'e, 'r>, 'cs, 'p, 'w> () with
          member t.Subst (ps, i, r) = rs.Subst (ps, i, &r.Rest)}
     | (es, rs) ->
       {new ProductS<And<'e, 'r>, And<'e, 'r>, 'cs, 'p, 'w> () with
          member t.Subst (ps, i, r) =
           rs.Subst (ps, es.Subst (ps, i, &r), &r.Rest)}

  member g.Product (m: AsProduct<'r, 'w>, rs: ProductS<'r, 'r, 'w, 'p, 'w>) =
    makeS m rs

  member g.Case (_: Case<Empty, 's, 'w>) : SumS<Empty, 's, 'p, 'w> =
    SS [missS]
  member g.Case (m: Case<'r, 's, 'w>,
                 r: ProductS<'r, 'r, 's, 'p, 'w>) : SumS<'r, 's, 'p, 'w> =
    SS [makeS m r]
  member g.Plus (SS c: SumS<        'c    , Choice<'c, 's>, 'p, 'w>,
                 SS s: SumS<           's ,            's , 'p, 'w>)
                     : SumS<Choice<'c, 's>, Choice<'c, 's>, 'p, 'w> =
    SS (c @ s)
  member g.Sum (m: Union<'w>,
                _: AsChoice<'s, 'w>,
                SS s: SumS<'s, 's, 'p, 'w>) =
    let s = Array.ofList s
    fun ps w -> s.[m.Tag w] ps w

  member g.Prim (_: Prim<'w>) : Subst<'p, 'w> =
    failwithf "Subst only works on sums and products. Given: %A." typeof<'w>

let subst (ps: array<'p>) (w: 'w) : 'w =
  (StaticRules<Subst>.Generate () : Subst<'p, 'w>) ps w

////////////////////////////////////////////////////////////////////////////////

let map (p2p: 'p -> 'p) (w: 'w) : 'w =
  subst (fetch w |> Array.map p2p) w
