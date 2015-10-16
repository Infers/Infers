// Copyright (C) by Vesa Karvonen

module Toys.Elems

open Infers
open Infers.Rep

type Elems<'p, 'w> = 'w -> array<'p>

type E<'p, 'w> =
  | Miss
  | Hit of num: int * ext: ('w -> int -> array<'p> -> unit)
type ProductE<'e, 'r, 'p, 'w> = P of E<'p, 'w>
type SumE<'c, 's, 'p, 'w> = S of list<E<'p, 'w>>

let miss _ = [||]
let hit num ext w =
  let ps = Array.zeroCreate num
  ext w 0 ps
  ps
let make = function
  | Miss -> miss
  | Hit (num, ext) -> hit num ext

type [<InferenceRules>] Elems () =
  inherit Rep ()
  member g.ElemMiss (_: Elem<'e, 'er, _, 'w>) : ProductE<'e, 'er, 'p, 'w> =
    P Miss
  member g.ElemHit (e: Elem<'p, 'er, _, 'w>) : ProductE<'p, 'er, 'p, 'w> =
    P <| Hit (1, fun w i ps -> ps.[i] <- e.Get w)
  member g.Times (P e: ProductE<    'e     , And<'e, 'r>, 'p, 'w>,
                  P r: ProductE<        'r ,         'r , 'p, 'w>)
                     : ProductE<And<'e, 'r>, And<'e, 'r>, 'p, 'w> =
    P <| match (e, r) with
          | (Miss, Miss) -> Miss
          | (Hit _, Miss) -> e
          | (Miss, Hit _) -> r
          | (Hit (numE, extE), Hit (numR, extR)) ->
            Hit (numE + numR,
                 fun w i ps ->
                   extE w  i         ps
                   extR w (i + numE) ps)
  member g.Product (_: AsProduct<'r, 'w>, P r: ProductE<'r, 'r, 'p, 'w>) =
    make r

  member g.Case (_: Case<Empty, 's, 'w>) : SumE<Empty, 's, 'p, 'w> =
    S [Miss]
  member g.Case (_: Case<'r, 's, 'w>,
                 P r: ProductE<'r, 'r, 'p, 'w>) : SumE<'r, 's, 'p, 'w> =
    S [r]
  member g.Plus (S c: SumE<        'c    , Choice<'c, 's>, 'p, 'w>,
                 S s: SumE<           's ,            's , 'p, 'w>)
                    : SumE<Choice<'c, 's>, Choice<'c, 's>, 'p, 'w> =
    S (c @ s)
  member g.Sum (m: Union<'w>, _: AsChoice<'s, 'w>, S s: SumE<'s, 's, 'p, 'w>) =
    let s = Array.ofList s |> Array.map make
    fun w -> s.[m.Tag w] w

  member g.Prim (_: Prim<'w>) : Elems<'p, 'w> =
    failwithf "Elems only works on sums and products. Given: %A." typeof<'w>

let elems (w: 'w) : array<'p> =
  (StaticRules<Elems>.Generate () : Elems<'p, 'w>) w
