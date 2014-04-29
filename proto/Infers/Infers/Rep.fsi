module Infers.Rep

open Infers.Engine

/////////////////////////////////////////////////////////////////////////

/// Represents an empty product as a special case.
type Empty = struct end

/// Represents a product of the types 'x and 'xs.
type And<'x, 'xs> = struct
  val mutable Elem: 'x
  val mutable Rest: 'xs
end

/////////////////////////////////////////////////////////////////////////

/// Representation of the type 't as nested structs of type 'p.
type [<AbstractClass>] AsProduct<'t, 'p> = class
  /// Copies fields of type 't to the generic product of type 'p.
  abstract Extract: 't * byref<'p> -> unit

  /// Creates a new instance of type 't from the generic product of type 'p.
  abstract Create: byref<'p> -> 't
end

/////////////////////////////////////////////////////////////////////////

/// Representation of a discriminated union type 'u as nested choices of
/// type 'c.
type [<AbstractClass>] AsChoice<'u, 'c> = class end

/////////////////////////////////////////////////////////////////////////

/// Base class for type representations.
type [<AbstractClass; AllowNullLiteral; InferenceRules>] Rep<'t> = class end

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# tuple type 't.
type [<AbstractClass; AllowNullLiteral>] Tuple<'t> =
  inherit Rep<'t>

  /// The number of elements the tuple type 't has.
  val mutable Arity: int

//member _: AsProduct<'t, 'p>
//member _: Elem<'t, 'e, 'p>
//...

type [<AbstractClass>] Elem<'t, 'e, 'p> =
  /// The index of the element.
  val mutable Index: int

  /// Returns the value of the element.
  abstract Get: 't -> 'e

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# discriminated union type 'u.
type [<AbstractClass; AllowNullLiteral>] Union<'u> =
  inherit Rep<'u>

  /// The number of cases the discriminated union type 'u has.
  val mutable Arity: int

  /// Returns the integer tag of the given discriminated union value.
  abstract Tag: 'u -> int

/// Representation of a case of the F# discriminated union type 'u.
type [<AbstractClass>] Case<'u, 'ls, 'cs> =
  inherit AsProduct<'u, 'ls>

  /// The name of the case.
  val mutable Name: string

  /// The number of elements, or labels, the case contains.
  val mutable Arity: int

  /// The integer tag of the case.
  val mutable Tag: int

/// Representation of a possibly labelled element of type 'l of a case of the
/// F# discriminated union type 'u.
type [<AbstractClass>] Label<'u, 'cs, 'l, 'ls> =
  inherit Elem<'u, 'l, 'ls>

  /// The name of the label.
  val mutable Name: string

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# record type 'r.
type [<AbstractClass; AllowNullLiteral>] Record<'r> =
  inherit Rep<'r>

  /// The number of fields the record type 'r has.
  val mutable Arity: int

  /// Whether the record type 'r has mutable fields.
  val mutable IsMutable: bool

//member _: AsProduct<'r, 'p>
//member _: Field<'r, 'x, 'p>

/// Representation of a field of type 'f of the record type 'r.
type [<AbstractClass>] Field<'r, 'f, 'p> =
  inherit Elem<'r, 'f, 'p>

  /// The name of the field.
  val mutable Name: string

  /// Whether the field is mutable.
  val mutable IsMutable: bool 

  /// Sets the value of the field assuming this is a mutable field.
  abstract Set: 'r * 'f -> unit

/////////////////////////////////////////////////////////////////////////

type [<Class; InferenceRules>] Rules =
  new: unit -> Rules

  /// Attempts to constructs a type representation for the type 'r.
  member record: unit -> Record<'r>

  /// Attempts to constructs a type representation for the type 'u.
  member union: unit -> Union<'u>

  /// Attempts to constructs a type representation for the type 't.
  member tuple: unit -> Tuple<'t>
