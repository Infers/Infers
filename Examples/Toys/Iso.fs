// Copyright (C) by Vesa Karvonen

module Toys.Iso

open Infers
open Infers.Rep

type Iso<'x, 'y> = I of ('x -> 'y) * ('y -> 'x)

type Iso () =
  inherit Rules ()
  static member Identity () = I (id, id)
  static member Invert (I (t, o)) = I (o, t)
  static member Compose (I (x2y, y2x), I (y2z, z2y)) =
    I (x2y >> y2z, z2y >> y2x)

  static member Swap () = let swap (x, y) = (y, x) in I (swap, swap)
  static member Restructure () = let re (x, (y, z)) = (y, (x, z)) in I (re, re)
  static member First (I (a2c, c2a)) =
    I ((fun (a, b) -> (a2c a, b)),
       (fun (c, b) -> (c2a c, b)))

  static member ``Pair<->Tuple`` () =
    I ((fun (Pair (a, b)) -> (a, b)),
       (fun (a, b) -> Pair (a, b)))

let iso<'x, 'y> = Engine.generate<Iso, Iso<'x, 'y>>
let fwd (I (x2y, _)) = x2y
let bwd (I (_, y2x)) = y2x
let convert (x: 'x) : 'y = fwd iso x
