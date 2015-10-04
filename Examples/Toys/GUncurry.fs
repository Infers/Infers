// Copyright (C) by Vesa Karvonen

module Toys.GUncurry

open Infers
open Infers.Rep

type [<InferenceRules>] GUncurry () =
  member g.Uncurry (_: Rep,
                    _: Tuple<'xst>,
                    p: AsProduct<'xsa, 'xst>,
                    u: 'xs2y -> 'xsa -> 'y) : 'xs2y -> 'xst -> 'y =
    fun xs2y -> p.ToProduct >> u xs2y
  member g.Nested u = fun x2xs2y (And (x, xs)) -> u (x2xs2y x) xs
  member g.Finish () = id

/// Derives a function that uncurries a given n-ary curried function.
let guncurry f = StaticRules<GUncurry>.Generate() f

let test () : unit =
  guncurry (printfn "%d %s") (1, "2")
  guncurry (printfn "%s %d %f %b") ("1", 2, 3.0, true)
