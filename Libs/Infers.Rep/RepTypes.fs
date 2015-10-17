// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

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

type [<AbstractClass; InferenceRules>] AsProduct<'p, 'o, 't> =
  new (arity, isMutable) = {Arity = arity; IsMutable = isMutable}
  val Arity: int
  val IsMutable: bool
  abstract Extract: 't * byref<'p> -> unit
  abstract Create: byref<'p> -> 't
  abstract ToProduct: 't -> 'p
  abstract OfProduct: 'p -> 't
  default this.ToProduct (t: 't) : 'p =
    let mutable p = Unchecked.defaultof<_>
    this.Extract (t, &p)
    p
  default this.OfProduct (p: 'p) : 't =
    let mutable p = p
    this.Create (&p)

type [<AbstractClass; InferenceRules>] AsSum<'s, 't> =
  new (arity) = {Arity = arity}
  val Arity: int
  abstract Tag: 't -> int

type [<InferenceRules>] Rep<'t> () = class end

type [<AbstractClass>] Prim<'t> () =
  inherit Rep<'t> ()

type [<AbstractClass>] Product<'r> () =
  inherit Rep<'r> ()

type [<AbstractClass>] Elem<'e, 'r, 'o, 't> =
  new (index) = {Index = index}
  val Index: int
  abstract Get: 't -> 'e

type [<AbstractClass>] Labelled<'e, 'r, 'o, 't> =
  inherit Elem<'e, 'r, 'o, 't>
  new (index, name) = {inherit Elem<'e, 'r, 'o, 't>(index); Name = name}
  val Name: string

type [<AbstractClass>] Tuple<'t> () =
  inherit Product<'t> ()

type [<AbstractClass>] Item<'e, 'r, 't> (index) =
  inherit Elem<'e, 'r, 't, 't> (index)

type [<AbstractClass>] Union<'t> () =
  inherit Rep<'t> ()

type [<AbstractClass>] Case<'p, 'o, 't> =
  inherit AsProduct<'p, 'o, 't>
  new (name, arity, tag) =
    {inherit AsProduct<'p, 'o, 't>(arity, false); Name = name; Tag = tag}
  val Name: string
  val Tag: int

type [<AbstractClass>] Label<'e, 'r, 'o, 't> =
  inherit Labelled<'e, 'r, 'o, 't>
  new (index, name) = {inherit Labelled<'e, 'r, 'o, 't> (index, name)}

type [<AbstractClass>] Record<'t> () =
  inherit Product<'t> ()

type [<AbstractClass>] Field<'e, 'r, 't> =
  inherit Labelled<'e, 'r, 't, 't>
  new (index, name, isMutable) =
    {inherit Labelled<'e, 'r, 't, 't> (index, name); IsMutable = isMutable}
  val IsMutable: bool
  abstract Set: 't * 'e -> unit
  default f.Set (_: 't, _: 'e) = raise <| System.NotImplementedException ()
