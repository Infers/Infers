// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers
open Infers.Rep

module AsPairs =
  type [<AbstractClass>] Extract<'e> () =
    abstract Extract: byref<'e> * array<obj> * int -> unit
  type [<AbstractClass>] Extract<'e, 'r> () =
    abstract Extract: byref<Pair<'e,'r>> * array<obj> * int -> unit

  type Extract () =
    inherit Rules ()
    static member Empty =
      {new Extract<Empty> () with
        override t.Extract (_, _, _) = ()}
    static member Elem () =
      {new Extract<'e> () with
        override t.Extract (e, os, i) =
          e <- unbox<'e> os.[i]}
    static member Pair (rW: Extract<'r>) =
      {new Extract<Pair<'e,'r>> () with
        override t.Extract (er, os, i) =
          er.Elem <- unbox<'e> os.[i]
          rW.Extract (&er.Rest, os, i+1)}

  type AsPairs<'p,'t> with
    member asP.Extract (from: array<obj>, into: byref<'p>) =
      if asP.Arity <> from.Length then
        failwithf "Expected %d elems, but given %d elems" asP.Arity from.Length
      generateDFS<Extract, Extract<'p>>.Extract (&into, from, 0)
    member asP.Create (from: array<obj>) =
      let mutable p = Unchecked.defaultof<'p>
      asP.Extract (from, &p)
      asP.Create (&p)
    member asP.Overwrite (record, into, from: array<obj>) =
      let mutable p = Unchecked.defaultof<'p>
      asP.Extract (from, &p)
      asP.Overwrite (record, into, &p)
