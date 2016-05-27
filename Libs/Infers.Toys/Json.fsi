// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open System.Collections.Generic
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
/// semi bidirectional translation of the supported subset of F# values to JSON
/// values.  The notation below uses `[<` and `>]` as "semantic brackets" inside
/// of which translation proceeds.
///
/// Simple values are translated as is:
///
///>                 [< true >]  ~  true
///>                [< false >]  ~  false
///>           [< number:int >]  ~  number
///>         [< number:float >]  ~  number
///>         [< number:int64 >]  ~  number
///>             [< "string" >]  ~  "string"
///
/// Special numbers are translated to null and the translation is obviously not
/// bidirectional:
///
///>             [< nan:float>]  ~  null
///>        [< infinity:float>]  ~  null
///>       [< -infinity:float>]  ~  null
///
/// Note that special numbers do not have standard representation in JSON and
/// the above treatment of special numbers corresponds to what JavaScript's
/// `JSON.stringify` does by default.  If you need to translate special numbers
/// more intelligently, you need to do it explicitly.
///
/// The unit value, `()`, is translated to null:
///
///>                   [< () >]  ~  null
///
/// Records are translated to objects where record fields of `option<_>` type
/// are considered optional properties:
///
///>       [< {l1 = v1; ...} >]  ~  {[< l1 = v1 >], ...}
///
///>           [< l = None   >]  ~
///>           [< l = Some v >]  ~  "l": [< v >]
///>           [< l =      v >]  ~  "l": [< v >]
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
///>           [<  (v, ...)  >]  ~  [[< v >], ...]
///
///>           [<  [v, ...]  >]  ~  [[< v >], ...]
///>           [< [|v, ...|] >]  ~  [[< v >], ...]
///
/// Union cases are translated by ignoring the union constructor `C` and
/// treating the carried value(s) either as a value by itself or as a record:
///
///>                  [< C v >]  ~  [< v >]
///
///>       [< C (l = v, ...) >]  ~  {[< l1 = v1 >], ...}
///
/// Note that if you do not specify labels for elements of a union case product,
/// F# will implicitly assign names starting with `Item1` to such elements.
///
/// That union constructors are ignored makes it possible to use them in
/// encodings of both discriminated and non-discriminated unions.  For the
/// purpose of encoding typical discriminated unions, the special `Is<_>` value
/// is translated to a string constant:
///
///>         [< Is<TypeName> >]  ~  "TypeName"
///
/// Maps and dictionaries whose keys are strings are translated to objects:
///
///>  [< map [("k", v); ...] >]  ~  {"k": [< v >], ...}
///> [< dict [("k", v); ...] >]  ~  {"k": [< v >], ...}
///
/// Finally, JSON `AST` values are translated to JSON:
///
///>          [< Object dict >]  ~  [< dict >]
///>            [< Array seq >]  ~  [< seq >]
///>      [< String "string" >]  ~  "string"
///>      [< Number "number" >]  ~  number
///>         [< Boolean bool >]  ~  [< bool >]
///>                 [< Null >]  ~  null
#endif
module Json =
  /// Represents an arbitrary JSON value.
  type AST =
    |  Object of Dictionary<string, AST>
    |   Array of ResizeArray<AST>
    |  String of string
    |  Number of string
    | Boolean of bool
    |    Null

  /// Use type name as a simple constant string.
  type Is<'t> = | Is

  /// Converts a value of type `'t` to a string in JSON format.  See the `Json`
  /// module documentation for a description of the type directed conversion and
  /// allowed types.
  val toString: 't -> string

  /// Converts a JSON format string to a value of type `'t`.  See the `Json`
  /// module documentation for a description of the type directed conversion and
  /// allowed types.
  val ofString<'t> : string -> 't

  /// Converts a value of type `'t` to a pretty printable document in JSON
  /// format.  See the `Json` module documentation for a description of the type
  /// directed conversion and allowed types.
  val toDoc: 't -> Doc

  /// Tries to parse a JSON format string to a value of type `'t`.  See the
  /// `Json` module documentation for a description of the type directed
  /// conversion and allowed types.
  val tryParseString<'t> : string -> Choice<'t, string>
