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

  let get (p: 'p) : 'e =
    generateDFS<Get, 'p -> 'e> p

  //

  type [<AbstractClass>] Iter<'e,'r,'o,'t,'hs> () =
    abstract Do: byref<'e> * 'hs -> unit

  type [<Rep; Get>] Iter () =
    inherit Rules ()
    static member Labelled (m: Labelled<'e,'r,'o,'t>,
                            get: 'hs -> (int -> string -> 'e -> unit)) =
      {new Iter<'e,'r,'o,'t,'hs> () with
        member t.Do (e, hs) = get hs m.Index m.Name e}
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
    static member Product (m: AsPairs<'p,'o,'t>, pI: Iter<'p,'p,'o,'t,'hs>) =
      fun hs t ->
        let mutable p = m.ToPairs t
        pI.Do (&p, hs)
    static member Union (_: AsChoices<'p,'t>,
                         m: Case<'p,'p,'t>,
                         pI: Iter<'p,'p,'p,'t,'hs>) =
      Iter.Product (m, pI)

  let iter hs p =
    generateDFS<Iter, 'hs -> 'p -> unit> hs p

  //

  type [<AbstractClass>] Init<'e,'r,'o,'t,'hs> () =
    abstract Do: 'hs * byref<'e> -> unit

  type [<Rep; Get>] Init () =
    inherit Rules ()
    static member Labelled (m: Labelled<'e,'r,'o,'t>,
                            get: 'hs -> (int -> string -> 'e)) =
      {new Init<'e,'r,'o,'t,'hs> () with
        member t.Do (hs, e) = e <- get hs m.Index m.Name}
    static member Labelled (m: Labelled<'e,'r,'o,'t>,
                            get: 'hs -> (string -> 'e)) =
      let n = m.Name
      {new Init<'e,'r,'o,'t,'hs> () with
        member t.Do (hs, e) = e <- get hs n}
    static member Elem (m: Elem<'e,'r,'o,'t>, get: 'hs -> (int -> 'e)) =
      let i = m.Index
      {new Init<'e,'r,'o,'t,'hs> () with
        member t.Do (hs, e) = e <- get hs i}
    static member Elem (_: Elem<'e,'r,'o,'t>, get: 'hs -> 'e) =
      {new Init<'e,'r,'o,'t,'hs> () with
        member t.Do (hs, e) = e <- get hs}
    static member Pair (eI: Init<'e,Pair<'e,'r>,'o,'t,'hs>,
                        rI: Init<'r,        'r, 'o,'t,'hs>) =
      {new Init<Pair<'e,'r>,Pair<'e,'r>,'o,'t,'hs> () with
        member t.Do (hs, er) =
          eI.Do (hs, &er.Elem)
          rI.Do (hs, &er.Rest)}
    static member Id (get: 'hs -> 't) = get
    static member Product (m: AsPairs<'p,'o,'t>, pI: Init<'p,'p,'o,'t,'hs>) =
      fun hs ->
        let mutable p = Unchecked.defaultof<_>
        pI.Do (hs, &p)
        m.Create (&p)
    static member Union (_: AsChoices<'p,'t>,
                         m: Case<'p,'p,'t>,
                         pI: Init<'p,'p,'p,'t,'hs>) =
      Init.Product (m, pI)

  let init hs =
    generateDFS<Init, 'hs -> 'p> hs
