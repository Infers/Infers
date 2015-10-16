// Copyright (C) by Vesa Karvonen

namespace Infers

type [<Sealed>] StaticRules<'rules when 'rules : (new : unit -> 'rules)> () =
  static let rules = new 'rules ()

  static member Generate () : 'result =
    StaticMap<'rules>.Memoize <| fun () ->
    match Engine.tryGenerate (rules :> obj) with
     | None -> failwithf "%A cannot derive %A." typeof<'rules> typeof<'result>
     | Some result -> result
