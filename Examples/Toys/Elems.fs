// Copyright (C) by Vesa Karvonen

module Toys.Elems

open Infers
open Infers.Rep

type Elems<'p, 'w> = 'w -> array<'p>

type ProductE<'e, 'r, 'p, 'w> =
  | Miss
  | Hit of num: int * ext: ('w -> int -> array<'p> -> unit)
type SumE<'c, 's, 'p, 'w> = SE of list<Elems<'p, 'w>>

let missE _ = [||]
let makeE = function
  | Miss -> missE
  | Hit (num, ext) -> fun w ->
    let ps = Array.zeroCreate num
    ext w 0 ps
    ps

type [<InferenceRules>] Elems () =
  inherit Rep ()
  member g.ElemMiss (_: Elem<'e, 'er, _, 'w>) : ProductE<'e, 'er, 'p, 'w> =
    Miss
  member g.ElemHit (e: Elem<'p, 'er, _, 'w>) : ProductE<'p, 'er, 'p, 'w> =
    Hit (1, fun w i ps -> ps.[i] <- e.Get w)
  member g.Times (ee: ProductE<    'e     , And<'e, 'r>, 'p, 'w>,
                  re: ProductE<        'r ,         'r , 'p, 'w>)
                    : ProductE<And<'e, 'r>, And<'e, 'r>, 'p, 'w> =
    match (ee, re) with
     | (Miss, Miss) -> Miss
     | (Hit (n, e), Miss)
     | (Miss, Hit (n, e)) -> Hit (n, e)
     | (Hit (numE, extE), Hit (numR, extR)) ->
       Hit (numE + numR,
            fun w i ps ->
              extE w  i         ps
              extR w (i + numE) ps)
  member g.Product (_: AsProduct<'r, 'w>, r: ProductE<'r, 'r, 'p, 'w>) =
    makeE r

  member g.Case (_: Case<Empty, 's, 'w>) : SumE<Empty, 's, 'p, 'w> =
    SE [missE]
  member g.Case (_: Case<'r, 's, 'w>,
                 r: ProductE<'r, 'r, 'p, 'w>) : SumE<'r, 's, 'p, 'w> =
    SE [makeE r]
  member g.Plus (SE c: SumE<        'c    , Choice<'c, 's>, 'p, 'w>,
                 SE s: SumE<           's ,            's , 'p, 'w>)
                     : SumE<Choice<'c, 's>, Choice<'c, 's>, 'p, 'w> =
    SE (c @ s)
  member g.Sum (m: Union<'w>, _: AsChoice<'s, 'w>, SE s: SumE<'s, 's, 'p, 'w>) =
    let s = Array.ofList s
    fun w -> s.[m.Tag w] w

  member g.Prim (_: Prim<'w>) : Elems<'p, 'w> =
    failwithf "Elems only works on sums and products. Given: %A." typeof<'w>

let elems (w: 'w) : array<'p> =
  (StaticRules<Elems>.Generate () : Elems<'p, 'w>) w

////////////////////////////////////////////////////////////////////////////////

type Subst<'p, 'w> = array<'p> -> 'w -> 'w

type [<AbstractClass;AllowNullLiteral>] ProductS<'e, 'r, 'p, 'w> () =
  abstract Subst: array<'p> * int * byref<'r> -> int

type SumS<'c, 's, 'p, 'w> = SS of list<Subst<'p, 'w>>

let check (ps: array<_>) n =
  if n <> ps.Length then
    failwithf "Expected %d elems, but was given %d elems" n ps.Length

let missS ps w = check ps 0; w
let makeS (m: AsProduct<'r, 'w>) (rs: ProductS<'r, 'r, 'p, 'w>) =
  match rs with
   | null -> missS
   | rs -> fun ps w ->
     let mutable r = m.ToProduct w
     check ps <| rs.Subst (ps, 0, &r)
     m.Create (&r)

type [<InferenceRules>] Subst () =
  inherit Rep ()
  member g.ElemMiss (_: Elem<'e, 'er, _, 'w>) : ProductS<'e, 'er, 'p, 'w> = null
  member g.ElemHit (_: Elem<'p, 'p, _, 'w>) =
    {new ProductS<'p, 'p, 'p, 'w> () with
      member t.Subst (ps, i, p) = p <- ps.[i]; i+1}
  member g.ElemHit (_: Elem<'p, And<'p, 'r>, _, 'w>) =
    {new ProductS<'p, And<'p, 'r>, 'p, 'w> () with
      member t.Subst (ps, i, p) = p.Elem <- ps.[i] ; i+1}
  member g.Times (es: ProductS<    'e     , And<'e, 'r>, 'p, 'w>,
                  rs: ProductS<        'r ,         'r , 'p, 'w>)
                    : ProductS<And<'e, 'r>, And<'e, 'r>, 'p, 'w> =
    match (es, rs) with
     | (null, null) -> null
     | (es, null) ->
       {new ProductS<And<'e, 'r>, And<'e, 'r>, 'p, 'w> () with
          member t.Subst (ps, i, r) = es.Subst (ps, i, &r)}
     | (null, rs) ->
       {new ProductS<And<'e, 'r>, And<'e, 'r>, 'p, 'w> () with
          member t.Subst (ps, i, r) = rs.Subst (ps, i, &r.Rest)}
     | (es, rs) ->
       {new ProductS<And<'e, 'r>, And<'e, 'r>, 'p, 'w> () with
          member t.Subst (ps, i, r) =
           rs.Subst (ps, es.Subst (ps, i, &r), &r.Rest)}

  member g.Product (m: AsProduct<'r, 'w>, rs: ProductS<'r, 'r, 'p, 'w>) =
    makeS m rs

  member g.Case (_: Case<Empty, 's, 'w>) : SumS<Empty, 's, 'p, 'w> =
    SS [missS]
  member g.Case (m: Case<'r, 's, 'w>,
                 r: ProductS<'r, 'r, 'p, 'w>) : SumS<'r, 's, 'p, 'w> =
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
