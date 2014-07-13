// Copyright (C) by Vesa Karvonen

namespace Infers

open Infers.Core
open System.Threading

type [<Sealed>] StaticMap<'k> =
  static member MemoizeLocked (mk: unit -> 'v) =
    match StaticMap<'k, option<'v>>.Get () with
     | None ->
       Some (mk ())
       |> StaticMap<'k, option<'v>>.TrySetAndGet
       |> Option.get
     | Some v -> v
  static member inline Memoize (mk: unit -> 'v) =
    match StaticMap<'k, option<'v>>.Get () with
     | Some v -> v
     | None -> StaticMap<'k>.MemoizeLocked mk
