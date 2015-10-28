// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open PPrint

module Json =
  type Obj = Map<string, Value>
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

  val ofString: string -> Choice<Value, string>

  val toJson<'t> : 't -> Value
  val ofJson<'t> : Value -> Choice<'t, string>
