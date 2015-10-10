// Copyright (C) by Vesa Karvonen

module Toys.Rec

open Infers
open Infers.Rep

type [<AbstractClass>] ProductRec'<'e> () =
  abstract Get: byref<'e> -> unit
  abstract Set: byref<'e> -> unit

type ProductRec<'e, 'r, 't> = P of ProductRec'<'e>

type [<InferenceRules>] Rec () =
  inherit RecFn ()

  member t.Elem (_: Elem<'e, 'r, 'c, 't>, eR: Rec<'e>) : ProductRec<'e, 'r, 't> =
    P {new ProductRec'<_> () with
        override pR.Get e = e <- eR.Get ()
        override pR.Set e = eR.Set e}

  member t.Times (P eR: ProductRec<'e, And<'e, 'r>, 't>,
                  P rR: ProductRec<'r, 'r, 't>)
                      : ProductRec<And<_, _>, And<'e, 'r>, 't> =
    P {new ProductRec'<_> () with
        override pR.Get er = eR.Get (&er.Elem) ; rR.Get (&er.Rest)
        override pR.Set er = eR.Set (&er.Elem) ; rR.Set (&er.Rest)}

  member t.Product (_: Rep,
                    m: AsProduct<'p, 't>,
                    P pR: ProductRec<'p, 'p, 't>) =
    {new Rec<_> () with
      override tR.Get () =
        let mutable p = Unchecked.defaultof<_>
        pR.Get (&p)
        m.Create (&p)
      override tR.Set t =
        let mutable p = m.ToProduct t
        pR.Set (&p)}
