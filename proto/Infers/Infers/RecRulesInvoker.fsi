namespace Infers

open System

type RecRulesInvoker =
  new: obj -> RecRulesInvoker
  member Tier: Type -> option<obj>
  member Untie: Type * obj -> obj
  member Tie: Type * obj * obj -> unit
