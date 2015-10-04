// Copyright (C) by Vesa Karvonen

module Toys.Rec

open Infers
open Infers.Rep

type [<InferenceRules>] Rec () =
  inherit RecFn ()

  member r.Newtype (_: Rep,
                    _: AsChoice<'c, 'u>,
                    m: Case<'c, 'c, 'u>,
                    cR: Rec<'c>) : Rec<'u> =
    {new Rec<'u> () with
      override uR.Get () = cR.Get () |> m.OfProduct
      override uR.Set u = m.ToProduct u |> cR.Set}
