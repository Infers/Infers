namespace Infers.Rep

open Infers

type Empty = struct end

type [<Struct>] And<'x, 'xs> =
  val mutable Elem: 'x
  val mutable Rest: 'xs

type [<AbstractClass; InferenceRules>] AsProduct<'t, 'p> () =
  abstract Extract: 't * byref<'p> -> unit
  abstract Create: byref<'p> -> 't

type [<AbstractClass; InferenceRules>] AsChoice<'u, 'c> () = class
  end

type [<AllowNullLiteral; InferenceRules>] Rep<'x> () = class
  end

type [<AbstractClass>] Product<'r> =
  inherit Rep<'r>
  new (arity, isMutable) = {Arity = arity; IsMutable = isMutable}
  val Arity: int
  val IsMutable: bool

type [<AbstractClass>] Tuple<'t> =
  inherit Product<'t>
  new (arity) = {inherit Product<'t> (arity, false)}

type [<AbstractClass>] Elem<'t, 'e, 'p> =
  new (index) = {Index = index}
  val Index: int
  abstract Get: 't -> 'e

type [<AbstractClass>] Union<'u> =
  inherit Rep<'u>
  new (arity) = {Arity = arity}
  val Arity: int
  abstract Tag: 'u -> int

type [<AbstractClass>] Case<'u, 'lp, 'sc> =
  inherit AsProduct<'u, 'lp>
  new (name, arity, tag) = {Name = name; Arity = arity; Tag = tag}
  val Name: string
  val Arity: int
  val Tag: int

type [<AbstractClass>] Label<'u, 'sc, 'l, 'sp> =
  inherit Elem<'u, 'l, 'sp>
  new (index, name) = {inherit Elem<'u, 'l, 'sp> (index); Name = name}
  val Name: string

type [<AbstractClass>] Record<'r> =
  inherit Product<'r>
  new (arity, isMutable) = {inherit Product<'r>(arity, isMutable)}

type [<AbstractClass>] Field<'r, 'f, 'p> =
  inherit Elem<'r, 'f, 'p>
  new (index, name, isMutable) =
    {inherit Elem<'r, 'f, 'p> (index); Name = name; IsMutable = isMutable}
  val Name: string
  val IsMutable: bool
  abstract Set: 'r * 'f -> unit
  default f.Set (_: 'r, _: 'f) = notImplemented ()
