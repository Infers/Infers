// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module internal Infers.Toys.Util

open System.Collections.Generic

let physicalComparer = {new IEqualityComparer<obj> with
  member t.GetHashCode (x) = LanguagePrimitives.PhysicalHash x
  member t.Equals (l, r) = LanguagePrimitives.PhysicalEquality l r}

let inline (^) x = x

module Map =
  let ofDictionary (d: Dictionary<_, _>) =
    d
    |> Seq.map ^ fun kv -> (kv.Key, kv.Value)
    |> Map

module Choice =
  let inline fold f g = function Choice1Of2 x -> f x
                               | Choice2Of2 y -> g y
  let inline map f g = fold (f >> Choice1Of2) (g >> Choice2Of2)

module Option =
  let inline orElse u2xO xO =
    match xO with
     | None -> u2xO ()
     | some -> some
