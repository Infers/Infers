// Copyright (C) by Vesa Karvonen

module Toys.Iso

open Infers
open Infers.Rep

/// `Iso<'x, 'y>` represents an isomorphism between `'x` and `'y`.
type Iso<'x, 'y> = ('x -> 'y) * ('y -> 'x)

/// A few rules for deriving isomorphisms.
type [<InferenceRules>] Iso () =
  member g.Id () : Iso<'x, 'x> = (id, id)
  member g.Inv ((t, o): Iso<'x, 'y>) : Iso<'y, 'x> = (o, t)
  member g.Compose ((x2y, y2x): Iso<'x, 'y>,
                    (y2z, z2y): Iso<'y, 'z>) : Iso<'x, 'z> =
    (x2y >> y2z, z2y >> y2x)
  member g.Pairs ((t, o): Iso<     'pairs,         'ands>)
                        : Iso<'e * 'pairs, And<'e, 'ands>> =
    ((fun (e, es) -> And (e, t es)),
     (fun (And (e, es)) -> (e, o es)))
