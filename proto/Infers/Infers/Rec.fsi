namespace Infers

open System

module Rec =
  type [<InferenceRules>] Rules =
    new: unit -> Rules

    member func: unit -> Rec<'x -> 'y>

    member func0: unit -> Rec<Func<'x>>
    member func1: unit -> Rec<Func<'x, 'y>>
    member func2: unit -> Rec<Func<'x, 'y, 'z>>
