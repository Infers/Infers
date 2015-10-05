// Copyright (C) by Vesa Karvonen

module Toys.Rec

open Infers
open Infers.Rep

type [<AbstractClass>] ProductRec<'e, 'r, 't> () =
  abstract Get: byref<'e> -> unit
  abstract Set: byref<'e> -> unit

/// Rules for computing fixed points over products, single case union types (aka
/// newtypes) and functions.
type [<InferenceRules>] Rec () =
  inherit RecFn ()

  member t.Elem (_: Elem<'e, 'r, 't>, eR: Rec<'e>) =
    {new ProductRec<'e, 'r, 't> () with
      override pR.Get e = e <- eR.Get ()
      override pR.Set e = eR.Set e}

  member t.Times (eR: ProductRec<'e, And<'e, 'r>, 't>,
                  rR: ProductRec<'r, 'r, 't>) =
    {new ProductRec<And<'e, 'r>, And<'e, 'r>, 't> () with
      override pR.Get er = eR.Get (&er.Elem) ; rR.Get (&er.Rest)
      override pR.Set er = eR.Set (&er.Elem) ; rR.Set (&er.Rest)}

  member t.Product (_: Rep,
                    m: AsProduct<'p, 't>,
                    pR: ProductRec<'p, 'p, 't>) =
    {new Rec<'t> () with
      override tR.Get () =
        let mutable p = Unchecked.defaultof<'p>
        pR.Get (&p)
        m.Create (&p)
      override tR.Set t =
        let mutable p = m.ToProduct t
        pR.Set (&p)}
