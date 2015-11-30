// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module internal Infers.Toys.Util

open System.Collections.Generic

let physicalComparer = {new IEqualityComparer<obj> with
  member t.GetHashCode (x) = LanguagePrimitives.PhysicalHash x
  member t.Equals (l, r) = LanguagePrimitives.PhysicalEquality l r}

let inline (^) x = x
