namespace Infers.Rep

open Infers

/////////////////////////////////////////////////////////////////////////

/// Represents an empty product as a special case for union cases.
type Empty = struct end

/// Represents a pair of the types `'x` and `'xs`.
#if DOC
///
/// Note that while it is in no way enforced, the idea is that in a nested
/// product the `Elem` field is the current singleton element and `Rest` is
/// the remainder of the nested produced.  For example, the nested product
/// of the type
///
///> char * int * float * bool
///
/// would be
///
///> And<char, And<int, And<float, bool>>>
///
/// The `Rep` rules generate products in this manner and it is good to know
/// this so that the processing of the singleton `Elem` field and the remainder
/// product `Rest` can be done in the desired order.
#endif
type [<Struct>] And<'x, 'xs> =
  /// The current element.
  val mutable Elem: 'x

  /// The remainder of the product.
  val mutable Rest: 'xs

/////////////////////////////////////////////////////////////////////////

/// Representation of the type `'t` as nested structs of type `'p`.
#if DOC
///
/// A product object also contains members for accessing the elements of the
/// product.  Depending on the type `'t` those members are of one of the
/// following forms:
///
///> member _:  Elem<'t,      'e, 'sp>
///> member _: Label<'u, 'sc, 'l, 'sp>
///> member _: Field<'r,      'f, 'sp>
///
/// Those members are visible to inference rules, but they cannot be given a
/// signature in F#.
#endif
type [<AbstractClass; InferenceRules>] AsProduct<'t, 'p> =
  /// Copies fields of type `'t` to the generic product of type `'p`.
  abstract Extract: 't * byref<'p> -> unit

  /// Creates a new instance of type `'t` from the generic product of type
  /// `'p`.
  abstract Create: byref<'p> -> 't

/////////////////////////////////////////////////////////////////////////

/// Representation of a discriminated union type `'u` as nested choices of
/// type `'c`.
#if DOC
///
/// A choice object also contains members for accessing individual cases of the
/// choice.  Those members are of the form
///
///> member _: Case<'u, 'lp, 'sc>
///
/// where `'lp` is a representation of the case as product and `'sc` is a nested
/// choice that identifies the particular case.
#endif
type [<AbstractClass; InferenceRules>] AsChoice<'u, 'c> = class
  /// 
//  abstract ToChoice: 'u -> 'c

  ///
//  abstract OfChoice: 'c -> 't
  end

/////////////////////////////////////////////////////////////////////////

type [<AllowNullLiteral; InferenceRules>] Rep<'x> = class
  end

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# product type (tuple or record) `'t`.
///
/// A product object also contains a member of the form
///
///> member _: AsProduct<'t, 'p>
///
/// where the type `'p` is a representation of the product as a nested record.
/// The member is visible to inference rules, but it cannot be given a signature
/// in F#.
type [<AbstractClass>] Product<'t> =
  inherit Rep<'t>

  /// The number of elements the product type has.
  val Arity: int

  /// Whether the product type is mutable.
  val IsMutable: bool

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# tuple type `'t`.
type [<AbstractClass>] Tuple<'t> =
  inherit Product<'t>

/// Representation of an element of type `'e` of a tuple of type `'t`.
type [<AbstractClass>] Elem<'t, 'e, 'p> =
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
type [<AbstractClass>] Union<'u> =
  inherit Rep<'u>

  /// The number of cases the discriminated union type `'u` has.
  val Arity: int

  /// Returns the integer tag of the given discriminated union value.
  abstract Tag: 'u -> int

/// Representation of a case of the F# discriminated union type `'u`.
type [<AbstractClass>] Case<'u, 'lp, 'sc> =
  inherit AsProduct<'u, 'lp>

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

  /// The name of the label.
  val Name: string

/////////////////////////////////////////////////////////////////////////

/// Type representation for the F# record type `'r`.
type [<AbstractClass>] Record<'r> =
  inherit Product<'r>

/// Representation of a field of type `'f` of the record type `'r`.
type [<AbstractClass>] Field<'r, 'f, 'sp> =
  inherit Elem<'r, 'f, 'sp>

  /// The name of the field.
  val Name: string

  /// Whether the field is mutable.
  val IsMutable: bool

  /// Sets the value of the field assuming this is a mutable field.
  abstract Set: 'r * 'f -> unit
