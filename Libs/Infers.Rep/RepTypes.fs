// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open Infers

type Empty = struct end

type [<Struct>] And<'x, 'xs> =
  val mutable Elem: 'x
  val mutable Rest: 'xs
  new (x, xs) = {Elem = x; Rest = xs}

[<AutoOpen>]
module And =
  let inline (|And|) (xxs: And<_, _>) = (xxs.Elem, xxs.Rest)

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

type [<AbstractClass; InferenceRules>] AsChoice<'c, 'u> () = class
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

type [<AbstractClass>] Elem<'e, 'p, 't> =
  new (index) = {Index = index}
  val Index: int
  abstract Get: 't -> 'e

type [<AbstractClass>] Labelled<'e, 'p, 't> =
  inherit Elem<'e, 'p, 't>
  new (index, name) = {inherit Elem<'e, 'p, 't>(index); Name = name}
  val Name: string

type [<AbstractClass>] Tuple<'t> =
  inherit Product<'t>
  new (arity) = {inherit Product<'t> (arity, false)}

type [<AbstractClass>] Item<'e, 'p, 't> (index) =
  inherit Elem<'e, 'p, 't> (index)

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
  inherit Labelled<'l, 'sp, 'u>
  new (index, name) = {inherit Labelled<'l, 'sp, 'u> (index, name)}

type [<AbstractClass>] Record<'r> =
  inherit Product<'r>
  new (arity, isMutable) = {inherit Product<'r>(arity, isMutable)}

type [<AbstractClass>] Field<'f, 'p, 'r> =
  inherit Labelled<'f, 'p, 'r>
  new (index, name, isMutable) =
    {inherit Labelled<'f, 'p, 'r> (index, name); IsMutable = isMutable}
  val IsMutable: bool
  abstract Set: 'r * 'f -> unit
  default f.Set (_: 'r, _: 'f) = raise (System.NotImplementedException ())
