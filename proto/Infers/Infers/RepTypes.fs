namespace Infers.Rep

open Infers

/////////////////////////////////////////////////////////////////////////

/// Represents an empty product as a special case for union cases.
type Empty = struct end

/// Represents a product of the types `'x` and `'xs`.
type [<Struct>] And<'x, 'xs> =
  val mutable Elem: 'x
  val mutable Rest: 'xs

/////////////////////////////////////////////////////////////////////////

/// Representation of the type `'t` as nested structs of type `'p`.
///
/// A product object also contains members for accessing the elements of the
/// product.  Depending on the type `'t` those members are of one of the
/// following forms:
///
///> member _: Elem<'t, 'e, 'sp>
///> member _: Label<'u, 'sc, 'lp, 'sp>
///> member _: Field<'r, 'f, 'sp>
///
/// Those members are visible to inference rules, but they cannot be given a
/// signature in F#.
type [<AbstractClass; InferenceRules>] AsProduct<'t, 'p> () =
  /// Copies fields of type `'t` to the generic product of type `'p`.
  abstract Extract: 't * byref<'p> -> unit

  /// Creates a new instance of type `'t` from the generic product of type
  /// `'p`.
  abstract Create: byref<'p> -> 't

/////////////////////////////////////////////////////////////////////////

/// Representation of a discriminated union type `'u` as nested choices of
/// type `'c`.
///
/// A choice object also contains mebers for accessing individual cases of the
/// choice.  Those members are of the form
///
///> member _: Case<'u, 'lp, 'sc>
///
/// where `'lp` is a representation of the case as product and `'sc` is a nested
/// choice that identifies the particular case.
type [<AbstractClass; InferenceRules>] AsChoice<'u, 'c> () = class
  /// 
//  abstract ToChoice: 'u -> 'c

  ///
//  abstract OfChoice: 'c -> 't
  end

/////////////////////////////////////////////////////////////////////////

type [<AllowNullLiteral>] Rep<'x> () = class
  end

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# tuple type `'t`.
///
/// A tuple object also contains a member of the form
///
///> member _: AsProduct<'t, 'p>
///
/// where the type `'p` is a representation of the tuple as a nested record.
/// The member is visible to inference rules, but it cannot be given a
/// signature in F#.
type [<AbstractClass; InferenceRules>] Tuple<'t> =
  inherit Rep<'t>

  new (arity) = {Arity = arity}

  /// The number of elements the tuple type `'t` has.
  val Arity: int

/// Representation of an element of type `'e` of a tuple of type `'t`.
type [<AbstractClass>] Elem<'t, 'e, 'p> =
  new (index) = {Index = index}

  /// The index of the element.
  val Index: int

  /// Returns the value of the element.
  abstract Get: 't -> 'e

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# discriminated union type `'u`.
///
/// A union object also contains a member of the form
///
///> member _: AsChoice<'u, 'c>
///
/// where type `'c` is a representation of the union as nested binary choices.
/// The member is visible to inference rules, but it cannot be given a signature
/// in F#.
type [<AbstractClass; InferenceRules>] Union<'u> =
  inherit Rep<'u>

  new (arity) = {Arity = arity}

  /// The number of cases the discriminated union type `'u` has.
  val Arity: int

  /// Returns the integer tag of the given discriminated union value.
  abstract Tag: 'u -> int

/// Representation of a case of the F# discriminated union type `'u`.
type [<AbstractClass>] Case<'u, 'lp, 'sc> =
  inherit AsProduct<'u, 'lp>

  new (name, arity, tag) = {Name = name; Arity = arity; Tag = tag}

  /// The name of the case.
  val Name: string

  /// The number of elements, or labels, the case contains.
  val Arity: int

  /// The integer tag of the case.
  val Tag: int

/// Representation of a possibly labelled element of type `'l` of a case of the
/// F# discriminated union type `'u`.
type [<AbstractClass>] Label<'u, 'sc, 'l, 'sp> =
  inherit Elem<'u, 'l, 'sp>

  new (index, name) = {inherit Elem<'u, 'l, 'sp> (index); Name = name}

  /// The name of the label.
  val Name: string

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# record type `'r`.
type [<AbstractClass; InferenceRules>] Record<'r> =
  inherit Rep<'r>

  new (arity, isMutable) = {Arity = arity; IsMutable = isMutable}

  /// The number of fields the record type `'r` has.
  val Arity: int

  /// Whether the record type 'r has mutable fields.
  val IsMutable: bool

//member _: AsProduct<'r, 'p>

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
  default f.Set (_: 'r, _: 'f) = notImplemented ()
