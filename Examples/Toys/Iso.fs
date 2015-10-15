// Copyright (C) by Vesa Karvonen

module Toys.Iso

open Infers
open Infers.Rep

/// `Iso<'x, 'y>` represents an isomorphism between `'x` and `'y`.
type Iso<'x, 'y> = ('x -> 'y) * ('y -> 'x)

/// A few rules for deriving isomorphisms.
type [<InferenceRules>] Iso () =
  member g.Id () = (id, id)
  member g.Inv ((t, o): Iso<_, _>) : Iso<_, _> = (o, t)
  member g.Compose ((x2y, y2x): Iso<_, _>,
                    (y2z, z2y): Iso<_, _>) =
    (x2y >> y2z, z2y >> y2x)
  member g.Swap () : Iso<_, _> =
    ((fun (x, y) -> (y, x)),
     (fun (x, y) -> (y, x)))
  member g.Jump () : Iso<_, _> =
    ((fun (x, (y, z)) -> (y, (x, z))),
     (fun (y, (x, z)) -> (x, (y, z))))
  member g.Pair ((a2c, c2a): Iso<_, _>,
                 (b2d, d2b): Iso<_, _>) : Iso<_, _> =
    ((fun (a, b) -> (a2c a, b2d b)),
     (fun (c, d) -> (c2a c, d2b d)))
  member g.AndIsoPair () : Iso<And<_, _>, _ * _> =
    ((fun (And (a, b)) -> (a, b)),
     (fun (a, b) -> And (a, b)))

let iso () : Iso<'x, 'y> = StaticRules<Iso>.Generate ()
let convert (x: 'x) : 'y = fst <| iso () <| x
