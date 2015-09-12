// Copyright (C) by Vesa Karvonen

namespace Infers

type [<Sealed>] StaticRules<'rules when 'rules : (new : unit -> 'rules)> =
  static member Generate () : 'result =
    StaticMap<'rules>.Memoize <| fun () ->
    match Engine.TryGenerate (new 'rules () :> obj) with
     | None -> failwithf "%A: Unsupported type %A" typeof<'rules> typeof<'result>
     | Some result -> result

  static member GenerateIDDFS () : 'result =
    StaticMap<'rules>.Memoize <| fun () ->
    match Engine.TryGenerate (true, 1, 5, [new 'rules () :> obj]) with
     | None -> failwithf "%A: Unsupported type %A" typeof<'rules> typeof<'result>
     | Some result -> result
