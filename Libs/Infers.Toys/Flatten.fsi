// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers
open Infers.Rep

[<AutoOpen>]
module Flatten =
  val flatten: 'xxs -> seq<'x> when 'xxs :> seq<_>

  type Flatten =
    inherit Rules
    new: unit -> Flatten

    static member Flat: unit -> ('xs -> seq<'x>) when 'xs :> seq<'x>

    static member Nested: ('xss -> seq<'x>)
                       -> ('xsss -> seq<'x>) when 'xsss :> seq<'xss>
