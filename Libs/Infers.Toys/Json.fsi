// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open PPrint

/// Provides a mapping between a subset of F# and JSON values.  The goal is to
/// make it possible to map typical existing JSON APIs to F# types and to
/// specify new JSON APIs using F# types.
#if DOC
///
/// Note that the goal here isn't to provide an encoding of all F# types in
/// JSON.  Such encodings can be useful for the internal communication or
/// persistence of an application, but tend not to work well for mapping
/// existing JSON APIs or for specifying new APIs to be used by external
/// applications.  Neither is the goal to support entirely arbitrary JSON
/// mapping, although one can always fall back to the JSON AST, but to rather
/// provide a mapping that should lead to usable F# types when mapping typical
/// APIs.
///
/// A mapping is always specified as a F# type.  In the following we specify the
/// (almost) bidirectional translation of the supported subset of F# values to
/// JSON values.
///
/// Simple values are translated as is:
///
///>                      [< true >]  ~  true
///>                     [< false >]  ~  false
///>                [< number:int >]  ~  number
///>              [< number:float >]  ~  number
///>              [< number:int64 >]  ~  number
///>                  [< "string" >]  ~  "string"
///
/// Special numbers are translated to null and the translation is obviously not
/// bidirectional:
///
///>                  [< nan:float>]  ~  null
///>             [< infinity:float>]  ~  null
///>            [< -infinity:float>]  ~  null
///
/// Note that special numbers do not have standard representation in JSON and
/// the above treatment of special numbers corresponds to what JavaScript's
/// `JSON.stringify` does by default.  If you need to translate special numbers
/// more intelligently, you need to do it explicitly.
///
/// The unit value, `()`, is translated to null:
///
///>                        [< () >]  ~  null
///
/// Records are translated to objects where record fields of `option<_>` type
/// are considered optional properties:
///
///>   [< {l1 = v1; ...; lN = vN} >]  ~  {[< "l1": v1 >], ..., [< "lN": vN >]}
///
///>               [< "l": None   >]  ~
///>               [< "l": Some v >]  ~  "l": [< v >]
///>               [< "l":      v >]  ~  "l": [< v >]
///
/// Note that `option<_>` types are only treated specially when they are fields
/// of record types (or labeled elements of a union case as specified below).
/// One might expect to able to use `option<_>` types anywhere and to have
/// `None` translated to `null`, but that would lead to a kind of ambiguity when
/// trying to map an object property that has `null` as a valid value.
///
/// Tuples are translated to fixed length arrays with non-uniform types while
/// lists and arrays are translated to arrays with uniform type:
///
///>           [<  (v1, ..., vN)  >]  ~  [[< v1 >], ..., [< vN >]]
///>           [<  [v1, ..., vN]  >]  ~  [[< v1 >], ..., [< vN >]]
///>           [< [|v1, ..., vN|] >]  ~  [[< v1 >], ..., [< vN >]]
///
/// Union cases are translated by ignoring the union constructor `C` and
/// treating the carried value(s) either as a value by itself or as a record:
///
///>                       [< C v >]  ~  [< v >]
///> [< C (l1 = v1, ..., lN = vN) >]  ~  {[< "l1": v1 >], ..., [< "lN": vN >]}
///
/// That union constructors are ignored makes it possible to use them in
/// encodings of both discriminated and non-discriminated unions.  For the
/// purpose of encoding typical discriminated unions, the special `Is<_>` value
/// is translated to a string constant:
///
///>              [< Is<TypeName> >]  ~  "TypeName"
///
/// Maps and dictionaries whose keys are strings are translated to objects:
///
///>       [< map [("k", v); ...] >]  ~  {"k": [< v >], ...}
///>      [< dict [("k", v); ...] >]  ~  {"k": [< v >], ...}
///
/// Finally, JSON AST values are traslated to JSON:
///
///>                   [< Obj map >]  ~  [< map >]
///>                 [< List list >]  ~  [< list >]
///>           [< String "string" >]  ~  "string"
///>           [< Number "number" >]  ~  number
///>                 [< Bool bool >]  ~  [< bool >]
///>                       [< Nil >]  ~  null
#endif
[<AutoOpen>]
module Json =
  /// Represents a Json object.
  type Obj = Map<string, Value>
  /// Represents a Json value.
  and Value =
    |    Obj of Obj
    |   List of list<Value>
    | String of string
    | Number of string
    |   Bool of bool
    |    Nil

  type Is<'t> = | Is

  val toString: Value -> string
  val toDoc:    Value -> Doc

  val    ofString: string ->        Value
  val tryOfString: string -> Choice<Value, string>

  val       toJson<'t> : 't -> Value
  val toJsonString<'t> : 't -> string

  val    ofJson<'t> : Value ->        't
  val tryOfJson<'t> : Value -> Choice<'t, string>

  val    ofJsonString<'t> : string ->        't
  val tryOfJsonString<'t> : string -> Choice<'t, string>
