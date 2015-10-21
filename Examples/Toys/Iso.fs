// Copyright (C) by Vesa Karvonen

module Toys.Iso

open Infers
open Infers.Rep

type Iso<'x, 'y> = I of ('x -> 'y) * ('y -> 'x)

type [<InferenceRules>] Iso () =
  member g.Identity () = I (id, id)
  member g.Invert (I (t, o)) = I (o, t)
  member g.Compose (I (x2y, y2x), I (y2z, z2y)) =
    I (x2y >> y2z, z2y >> y2x)

  member g.Swap () = let swap (x, y) = (y, x) in I (swap, swap)
  member g.Restructure () = let re (x, (y, z)) = (y, (x, z)) in I (re, re)
  member g.First (I (a2c, c2a)) =
    I ((fun (a, b) -> (a2c a, b)),
       (fun (c, b) -> (c2a c, b)))

  member g.``Pair<->Tuple`` () =
    I ((fun (Pair (a, b)) -> (a, b)),
       (fun (a, b) -> Pair (a, b)))

let iso<'x, 'y> = Engine.generate<Iso, Iso<'x, 'y>>
let fwd (I (x2y, _)) = x2y
let bwd (I (_, y2x)) = y2x
let convert (x: 'x) : 'y = fwd iso x
