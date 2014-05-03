namespace Infers.Rep

open Infers

/////////////////////////////////////////////////////////////////////////

/// Represents an empty product as a special case.
type Empty = struct end

/// Represents a product of the types `'x` and `'xs`.
type [<Struct>] And<'x, 'xs> =
  val mutable Elem: 'x
  val mutable Rest: 'xs

/////////////////////////////////////////////////////////////////////////

/// Representation of the type `'t` as nested structs of type `'p`.
type [<AbstractClass>] AsProduct<'t, 'p> () =
  /// Copies fields of type `'t` to the generic product of type `'p`.
  abstract Extract: 't * byref<'p> -> unit

  /// Creates a new instance of type `'t` from the generic product of type
  /// `'p`.
  abstract Create: byref<'p> -> 't

/////////////////////////////////////////////////////////////////////////

/// Representation of a discriminated union type `'u` as nested choices of
/// type `'c`.
type [<AbstractClass>] AsChoice<'u, 'c> () = class
  // XXX Missing design & implementation
  end

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# tuple type `'t`.
type [<AbstractClass; InferenceRules>] Tuple<'t> =
  new (arity) = {Arity = arity}

  /// The number of elements the tuple type `'t` has.
  val Arity: int

//member _: AsProduct<'t, 'p>
//member _: Elem<'t, 'e, 'p>
//...

type [<AbstractClass>] Elem<'t, 'e, 'p> =
  new (index) = {Index = index}

  /// The index of the element.
  val Index: int

  /// Returns the value of the element.
  abstract Get: 't -> 'e

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# discriminated union type `'u`.
type [<AbstractClass; InferenceRules>] Union<'u> =
  new (arity) = {Arity = arity}

  /// The number of cases the discriminated union type `'u` has.
  val Arity: int

  /// Returns the integer tag of the given discriminated union value.
  abstract Tag: 'u -> int

/// Representation of a case of the F# discriminated union type `'u`.
type [<AbstractClass>] Case<'u, 'ls, 'cs> =
  inherit AsProduct<'u, 'ls>

  new (name, arity, tag) = {Name = name; Arity = arity; Tag = tag}

  /// The name of the case.
  val Name: string

  /// The number of elements, or labels, the case contains.
  val Arity: int

  /// The integer tag of the case.
  val Tag: int

/// Representation of a possibly labelled element of type `'l` of a case of the
/// F# discriminated union type `'u`.
type [<AbstractClass>] Label<'u, 'cs, 'l, 'ls> =
  inherit Elem<'u, 'l, 'ls>

  new (index, name) = {inherit Elem<'u, 'l, 'ls> (index); Name = name}

  /// The name of the label.
  val Name: string
  (*
type LabelTest (x, y) =
  inherit Label<int, int, int, int>(x, y)
  override t.Get (_) = failwith "XXX"
  *)
/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# record type `'r`.
type [<AbstractClass; InferenceRules>] Record<'r> =
  new (arity, isMutable) = {Arity = arity; IsMutable = isMutable}

  /// The number of fields the record type `'r` has.
  val Arity: int

  /// Whether the record type 'r has mutable fields.
  val IsMutable: bool

//member _: AsProduct<'r, 'p>
//member _: Field<'r, 'x, 'p>

/// Representation of a field of type `'f` of the record type `'r`.
type [<AbstractClass>] Field<'r, 'f, 'p> =
  inherit Elem<'r, 'f, 'p>

  new (index, name, isMutable) =
    {inherit Elem<'r, 'f, 'p> (index); Name = name; IsMutable = isMutable}

  /// The name of the field.
  val Name: string

  /// Whether the field is mutable.
  val IsMutable: bool 

  /// Sets the value of the field assuming this is a mutable field.
  abstract Set: 'r * 'f -> unit
