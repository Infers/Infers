// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers
open Infers.Rep

module AsPairs =
  type [<AbstractClass>] Extract<'e> () =
    abstract IntoPairs: byref<'e> * array<obj> * int -> unit
    abstract IntoArray: array<obj> * int * byref<'e> -> unit
  type [<AbstractClass>] Extract<'e, 'r> () =
    abstract IntoPairs: byref<Pair<'e,'r>> * array<obj> * int -> unit
    abstract IntoArray: array<obj> * int * byref<Pair<'e,'r>> -> unit

  type Extract () =
    inherit Rules ()
    static member Empty =
      {new Extract<Empty> () with
        override t.IntoPairs (_, _, _) = ()
        override t.IntoArray (_, _, _) = ()}
    static member Elem () =
      {new Extract<'e> () with
        override t.IntoPairs (e, os, i) =
          e <- unbox<'e> os.[i]
        override t.IntoArray (os, i, e) =
          os.[i] <- box<'e> e}
    static member Pair (rE: Extract<'r>) =
      {new Extract<Pair<'e,'r>> () with
        override t.IntoPairs (er, os, i) =
          er.Elem <- unbox<'e> os.[i]
          rE.IntoPairs (&er.Rest, os, i+1)
        override t.IntoArray (os, i, er) =
          os.[i] <- box<'e> er.Elem
          rE.IntoArray (os, i, &er.Rest)}

  let extract<'p> = generateDFS<Extract, Extract<'p>>

  type AsPairs<'p,'t> with
    member asP.Extract (from: 't, into: array<obj>) =
      let mutable p = Unchecked.defaultof<'p>
      asP.Extract (from, &p)
      extract.IntoArray (into, 0, &p)
    member asP.Create (from: array<obj>) =
      let mutable p = Unchecked.defaultof<'p>
      extract.IntoPairs (&p, from, 0)
      asP.Create (&p)
    member asP.Overwrite (record, into, from: array<obj>) =
      let mutable p = Unchecked.defaultof<'p>
      extract.IntoPairs (&p, from, 0)
      asP.Overwrite (record, into, &p)
