// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers
open Infers.Rep

module Product =
  type [<Rep>] Get () =
    inherit Rules ()
    static member Get () = id
    static member Get (_: AsPairs<'p,'t,'t>,
                       m: Elem<'e,'r,'t,'t>) =
      m.Get
    static member Get (_: AsChoices<'p,'t>,
                       _: Case<'p,'p,'t>,
                       m: Label<'e,'r,'p,'t>) =
      m.Get

  let get (t: 't) : 'e =
    generateDFS<Get, 't -> 'e> t
