// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open Infers

module GFlip =
  type GFlip () =
    inherit Rules ()
    static member Id () = id
    static member First ab2yz = fun xb -> xb >> ab2yz
    static member Rest (ab2axc, ac2y) = fun ab ->
      let axc = ab2axc ab
      let xac = fun x a -> axc a x
      xac >> ac2y

  let gflip f = generate<GFlip, (_ -> _) -> _ -> _> f
