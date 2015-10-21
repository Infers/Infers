// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Rec =
  open Infers
  open Infers.Rep

  type [<AbstractClass>] RecP'<'e> () =
    abstract Get: byref<'e> -> unit
    abstract Set: byref<'e> -> unit

  type RecP<'e, 'r, 'o, 't> = P of RecP'<'e>

  type [<InferenceRules>] Rec () =
    inherit RecFn ()

    member t.Elem (_: Elem<'e, 'r, 'o, 't>, eR: Rec<'e>) : RecP<'e, 'r, 'o, 't> =
      P {new RecP'<_> () with
          override pR.Get e = e <- eR.Get ()
          override pR.Set e = eR.Set e}

    member t.Pair (P eR: RecP<'e, Pair<'e, 'r>, 'o, 't>,
                   P rR: RecP<'r,          'r , 'o, 't>)
                       : RecP<Pair<_, _>, Pair<'e, 'r>, 'o, 't> =
      P {new RecP'<_> () with
          override pR.Get er = eR.Get (&er.Elem) ; rR.Get (&er.Rest)
          override pR.Set er = eR.Set (&er.Elem) ; rR.Set (&er.Rest)}

    member t.Product (_: Rep,
                      m: AsPairs<'p, 'o, 't>,
                      P pR: RecP<'p, 'p, 'o, 't>) =
      {new Rec<_> () with
        override tR.Get () =
          let mutable p = Unchecked.defaultof<_>
          pR.Get (&p)
          m.Create (&p)
        override tR.Set t =
          let mutable p = m.ToPairs t
          pR.Set (&p)}
