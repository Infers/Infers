// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open System
open Infers

type Empty = struct end

type [<Struct>] Pair<'x, 'xs> =
  val mutable Elem: 'x
  val mutable Rest: 'xs
  new (x, xs) = {Elem = x; Rest = xs}
  override t.ToString () = sprintf "Pair (%A, %A)" t.Elem t.Rest

[<AutoOpen>]
module Pair =
  let inline (|Pair|) (xxs: Pair<_, _>) = (xxs.Elem, xxs.Rest)

type [<AbstractClass>] Rep<'t> () =
  inherit Rules ()

type [<AbstractClass>] Prim<'t> () =
  inherit Rep<'t> ()

type [<AbstractClass>] Unsupported<'t> () =
  inherit Rep<'t> ()

type [<AbstractClass>] Product<'r> () =
  inherit Rep<'r> ()

type [<AbstractClass>] Record<'t> () =
  inherit Product<'t> ()

type B = Reflection.BindingFlags

type [<AbstractClass>] AsPairs<'p, 't> =
  inherit Rules
  new () = {inherit Rules (); Arity = 0; IsMutable = false}
  val Arity: int
  val IsMutable: bool
  abstract Extract: 't * byref<'p> -> unit
  abstract Create: byref<'p> -> 't
  abstract Overwrite: Record<'t> * into: 't * from: byref<'p> -> unit
  abstract ToPairs: 't -> 'p
  abstract OfPairs: 'p -> 't
  abstract Default: Record<'t> -> 't
  default this.ToPairs (t: 't) : 'p =
    let mutable p = Unchecked.defaultof<_>
    this.Extract (t, &p)
    p
  default this.OfPairs (p: 'p) : 't =
    let mutable p = p
    this.Create (&p)
  default this.Overwrite (_, into, from) =
    let from = this.Create (&from)
    for field in typeof<'t>.GetFields (B.Instance|||B.Public|||B.NonPublic) do
      field.SetValue (into, field.GetValue from)
  default this.Default (_) =
    let mutable p = Unchecked.defaultof<_>
    this.Create (&p)

type [<AbstractClass>] AsPairs<'p, 'o, 't> () =
  inherit AsPairs<'p, 't> ()

type [<AbstractClass>] AsChoices<'t> =
  inherit Rules
  new () = {inherit Rules (); Arity = 0}
  val Arity: int
  abstract Tag: 't -> int

type [<AbstractClass>] AsChoices<'s, 't> () =
  inherit AsChoices<'t> ()

type [<AbstractClass>] Elem<'e, 't> =
  new () = {Index = 0}
  val Index: int
  abstract Get: 't -> 'e

type [<AbstractClass>] Elem<'e, 'r, 'o, 't> () =
  inherit Elem<'e, 't> ()

type [<AbstractClass>] Labelled<'e, 'r, 'o, 't> =
  inherit Elem<'e, 'r, 'o, 't>
  new () = {inherit Elem<'e, 'r, 'o, 't>(); Name = null}
  val Name: string

type [<AbstractClass>] Tuple<'t> () =
  inherit Product<'t> ()

type [<AbstractClass>] Item<'e, 'r, 't> () =
  inherit Elem<'e, 'r, 't, 't> ()

type [<AbstractClass>] Union<'t> () =
  inherit Rep<'t> ()

type [<AbstractClass>] Case<'p, 'o, 't> =
  inherit AsPairs<'p, 'o, 't>
  new () = {inherit AsPairs<'p, 'o, 't>(); Name = null; Tag = 0}
  val Name: string
  val Tag: int

type [<AbstractClass>] Label<'e, 'r, 'o, 't> =
  inherit Labelled<'e, 'r, 'o, 't>
  new () = {inherit Labelled<'e, 'r, 'o, 't> ()}

type [<AbstractClass>] Field<'e, 'r, 't> =
  inherit Labelled<'e, 'r, 't, 't>
  new () = {inherit Labelled<'e, 'r, 't, 't> (); IsMutable = false}
  val IsMutable: bool
  abstract Set: 't * 'e -> unit
  default f.Set (_: 't, _: 'e) = raise <| NotImplementedException ()
