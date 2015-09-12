// Copyright (C) by Vesa Karvonen

module Toys.GUncurry

open Infers
open Infers.Rep
open Toys.Iso

type Pairs<'xsp> = | Pairs'

type [<InferenceRules>] Pairs () =
  member g.Nested (_: Pairs<'xsp>) : Pairs<'x * 'xsp> = Pairs'
  member g.Pair () : Pairs<'x * 'y> = Pairs'

type [<InferenceRules>] GUncurry () =
  member g.Uncurry (_: Rep, _: Iso, _: Pairs,
                    _: Tuple<'xst>,
                    p: AsProduct<'xsa, 'xst>,
                    _: Pairs<'xsp>,
                    (_, o): Iso<'xsp, 'xsa>,
                    u: 'xs2y -> 'xsp -> 'y) : 'xs2y -> 'xst -> 'y =
    fun xs2y -> p.ToProduct >> o >> u xs2y
  member g.Nested u = fun x2xs2y (x, xs) -> u (x2xs2y x) xs
  member g.Finish () = id

/// Derives a function that uncurries a given n-ary curried function.
let guncurry f = Option.get (NextGen.tryGenerate (GUncurry ())) f

let test () : unit =
  guncurry (printfn "%d %s") (1, "2")
  guncurry (printfn "%s %d %f %b") ("1", 2, 3.0, true)
