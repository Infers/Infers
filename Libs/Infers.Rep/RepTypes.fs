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

type [<AbstractClass; InferenceRules>] AsProduct<'p, 't> () =
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

type [<AbstractClass; InferenceRules>] AsSum<'c, 'u> () = class
  end

type [<InferenceRules>] Rep<'x> () = class
  end

type [<AbstractClass>] Prim<'x> () =
  inherit Rep<'x> ()

type [<AbstractClass>] Product<'r> =
  inherit Rep<'r>
  new (arity, isMutable) = {Arity = arity; IsMutable = isMutable}
  val Arity: int
  val IsMutable: bool

type [<AbstractClass>] Elem<'e, 'p, 'c, 't> =
  new (index) = {Index = index}
  val Index: int
  abstract Get: 't -> 'e

type [<AbstractClass>] Labelled<'e, 'p, 'c, 't> =
  inherit Elem<'e, 'p, 'c, 't>
  new (index, name) = {inherit Elem<'e, 'p, 'c, 't>(index); Name = name}
  val Name: string

type [<AbstractClass>] Tuple<'t> =
  inherit Product<'t>
  new (arity) = {inherit Product<'t> (arity, false)}

type [<AbstractClass>] Item<'e, 'p, 't> (index) =
  inherit Elem<'e, 'p, 't, 't> (index)

type [<AbstractClass>] Union<'u> =
  inherit Rep<'u>
  new (arity) = {Arity = arity}
  val Arity: int
  abstract Tag: 'u -> int

type [<AbstractClass>] Case<'lp, 'sc, 'u> =
  inherit AsProduct<'lp, 'u>
  new (name, arity, tag) = {Name = name; Arity = arity; Tag = tag}
  val Name: string
  val Arity: int
  val Tag: int

type [<AbstractClass>] Label<'l, 'sp, 'sc, 'u> =
  inherit Labelled<'l, 'sp, 'sc, 'u>
  new (index, name) = {inherit Labelled<'l, 'sp, 'sc, 'u> (index, name)}

type [<AbstractClass>] Record<'r> =
  inherit Product<'r>
  new (arity, isMutable) = {inherit Product<'r>(arity, isMutable)}

type [<AbstractClass>] Field<'f, 'p, 'r> =
  inherit Labelled<'f, 'p, 'r, 'r>
  new (index, name, isMutable) =
    {inherit Labelled<'f, 'p, 'r, 'r> (index, name); IsMutable = isMutable}
  val IsMutable: bool
  abstract Set: 'r * 'f -> unit
  default f.Set (_: 'r, _: 'f) = raise (System.NotImplementedException ())
