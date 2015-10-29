// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open PPrint

[<AutoOpen>]
module Json =
  /// Represents a Json object.
  type Obj = Map<string, Value>
  /// Represents a Json value.
  and Value =
   | Obj of Obj
   | List of list<Value>
   | String of string
   | Number of string
   | Bool of bool
   | Nil

  type Is<'t> = | Is

  val toString: Value -> string
  val toDoc: Value -> Doc

  val ofString: string -> Value
  val tryOfString: string -> Choice<Value, string>

  val toJson<'t> : 't -> Value
  val ofJson<'t> : Value -> 't
  val tryOfJson<'t> : Value -> Choice<'t, string>

  val toJsonString<'t> : 't -> string
  val ofJsonString<'t> : string -> 't
  val tryOfJsonString<'t> : string -> Choice<'t, string>
