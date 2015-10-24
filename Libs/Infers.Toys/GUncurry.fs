// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers.Rep
open Infers

[<AutoOpen>]
module GUncurry =
  type [<Rep>] GUncurry () =
    inherit Rules ()
    static member Uncurry (p: AsPairs<'xsa, _, 'xst>,
                           u: 'xs2y -> 'xsa -> 'y) : 'xs2y -> 'xst -> 'y =
      fun xs2y -> p.ToPairs >> u xs2y
    static member Nested u = fun x2xs2y (Pair (x, xs)) -> u (x2xs2y x) xs
    static member Finish () = id

  let guncurry f = generate<GUncurry, (_ -> _) -> _ -> _> f
