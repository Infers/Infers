// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers
open Infers.Rep

[<AutoOpen>]
module Flatten =

  type [<Rep>] Flatten () =
    inherit Rules ()

    static member Flat () : ('xs -> seq<'x>) when 'xs :> seq<'x> =
      fun xs -> upcast xs

    static member Nested (flatten: 'xss -> seq<'x>) =
      Seq.collect flatten : ('xsss -> seq<'x>) when 'xsss :> seq<'xss>

  let flatten (xxs: 'xxs) : seq<'x> when 'xxs :> seq<_> =
    generate<Flatten, _> xxs
