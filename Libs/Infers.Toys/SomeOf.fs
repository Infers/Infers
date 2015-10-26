// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers.Rep
open Infers

module SomeOf =
  type [<Struct>] SomeOf<'t> =
    val Value: 't
    new (value) = {Value = value}

  type [<Rep>] SomeOf () =
    inherit Rules ()

    static member Unit = SomeOf<_> (())

    static member String = SomeOf<_> ""

    static member Array () = SomeOf<_> [||]

    static member Prim (_: Prim<'t>) = Unchecked.defaultof<SomeOf<'t>>

    static member Pair (eS: SomeOf<'e>, rS: SomeOf<'r>) =
      SomeOf<_> (Pair (eS.Value, rS.Value))

    static member Product (asP: AsPairs<'p,'t>, pS: SomeOf<'p>) =
      let mutable p = pS.Value in SomeOf<'t> (asP.Create (&p))

    static member Rec (tR: Record<'t>, asP: AsPairs<'p, 't, 't>) =
      let t = asP.Default tR
      {new Rec<SomeOf<'t>> () with
        member r.Get () = SomeOf<_> (t)
        member r.Set (tS) =
          let mutable p = asP.ToPairs tS.Value
          asP.Overwrite (tR, t, &p)}

    static member Empty : SomeOf<Empty> = Unchecked.defaultof<_>

    static member Sum (c: Case<'p,_,'t>, pS: SomeOf<'p>) =
      SomeOf.Product (c, pS)

  let someOf<'t> = generateDFS<SomeOf, SomeOf<'t>>.Value
