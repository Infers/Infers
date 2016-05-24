// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers
open Infers.Rep

module Product =
  type [<Rep>] Get () =
    inherit Rules ()
    static member Id () = id
    static member Product (_: AsPairs<'p,'t,'t>,
                           m: Elem<'e,'r,'t,'t>) =
      m.Get
    static member Union (_: AsChoices<'p,'t>,
                         _: Case<'p,'p,'t>,
                         m: Label<'e,'r,'p,'t>) =
      m.Get

  let get (t: 't) : 'e =
    generateDFS<Get, 't -> 'e> t

  type [<AbstractClass>] Iter<'e,'r,'o,'t,'hs> () =
    abstract Do: byref<'e> * 'hs -> unit

  type [<Rep; Get>] Iter () =
    inherit Rules ()

    static member Labelled (m: Labelled<'e,'r,'o,'t>,
                            get: 'hs -> (string -> 'e -> unit)) =
      let n = m.Name
      {new Iter<'e,'r,'o,'t,'hs> () with
        member t.Do (e, hs) = get hs n e}

    static member Elem (m: Elem<'e,'r,'o,'t>, get: 'hs -> (int -> 'e -> unit)) =
      let i = m.Index
      {new Iter<'e,'r,'o,'t,'hs> () with
        member t.Do (e, hs) = get hs i e}

    static member Elem (_: Elem<'e,'r,'o,'t>, get: 'hs -> ('e -> unit)) =
      {new Iter<'e,'r,'o,'t,'hs> () with
        member t.Do (e, hs) = get hs e}

    static member Pair (eI: Iter<'e,Pair<'e,'r>,'o,'t,'hs>,
                        rI: Iter<'r,        'r, 'o,'t,'hs>) =
      {new Iter<Pair<'e,'r>,Pair<'e,'r>,'o,'t,'hs> () with
        member t.Do (er, hs) =
          eI.Do (&er.Elem, hs)
          rI.Do (&er.Rest, hs)}

    static member Id (get: 'hs -> ('t -> unit)) =
      fun hs t ->
        get hs t

    static member Product (m: AsPairs<'p,'o,'t>, iter: Iter<'p,'p,'o,'t,'hs>) =
      fun hs t ->
        let mutable p = m.ToPairs t
        iter.Do (&p, hs)

    static member Union (_: AsChoices<'p,'t>, m: Case<'p,'p,'t>, iter: Iter<'p,'p,'p,'t,'hs>) =
      Iter.Product (m, iter)

  let iter (hs: 'hs) (p: 'p) =
    generateDFS<Iter, 'hs -> 'p -> unit> hs p
