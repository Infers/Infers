namespace Infers

open System

type [<InferenceRules>] RecFn =
  new: unit -> RecFn

  member func: unit -> Rec<'x -> 'y>

  member func0: unit -> Rec<Func<'x>>
  member func1: unit -> Rec<Func<'x, 'y>>
  member func2: unit -> Rec<Func<'x, 'y, 'z>>
