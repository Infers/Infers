module Infers.Rec

open System
open Infers.Engine

type [<Sealed; RecursionRules>] RecFun =
  new: unit -> RecFun
  member Tier: ('a -> 'b) -> ref<'a -> 'b>
  member Untie: ref<'a -> 'b> -> ('a -> 'b)
  member Tie: ref<'a -> 'b> * ('a -> 'b) -> unit

type [<Sealed; RecursionRules>] RecFunc0 =
  new: unit -> RecFunc0
  member Tier: Func<'a> -> ref<Func<'a>>
  member Untie: ref<Func<'a>> -> Func<'a>
  member Tie: ref<Func<'a>> * Func<'a> -> unit

type [<Sealed; RecursionRules>] RecFunc1 =
  new: unit -> RecFunc1
  member Tier: Func<'a, 'b> -> ref<Func<'a, 'b>>
  member Untie: ref<Func<'a, 'b>> -> Func<'a, 'b>
  member Tie: ref<Func<'a, 'b>> * Func<'a, 'b> -> unit

type [<Sealed; RecursionRules>] RecFunc2 =
  new: unit -> RecFunc2
  member Tier: Func<'a, 'b, 'c> -> ref<Func<'a, 'b, 'c>>
  member Untie: ref<Func<'a, 'b, 'c>> -> Func<'a, 'b, 'c>
  member Tie: ref<Func<'a, 'b, 'c>> * Func<'a, 'b, 'c> -> unit
