namespace Infers.Rep

open Infers

type Empty = struct end

type [<Struct>] And<'x, 'xs> =
  val mutable Elem: 'x
  val mutable Rest: 'xs

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

type [<AbstractClass>] Elem<'e, 'p, 't> =
  new (index) = {Index = index}
  val Index: int
  abstract Get: 't -> 'e

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
  inherit Elem<'l, 'sp, 'u>
  new (index, name) = {inherit Elem<'l, 'sp, 'u> (index); Name = name}
  val Name: string

type [<AbstractClass>] Record<'r> =
  inherit Product<'r>
  new (arity, isMutable) = {inherit Product<'r>(arity, isMutable)}

type [<AbstractClass>] Field<'f, 'p, 'r> =
  inherit Elem<'f, 'p, 'r>
  new (index, name, isMutable) =
    {inherit Elem<'f, 'p, 'r> (index); Name = name; IsMutable = isMutable}
  val Name: string
  val IsMutable: bool
  abstract Set: 'r * 'f -> unit
  default f.Set (_: 'r, _: 'f) = notImplemented ()
