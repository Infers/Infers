// Copyright (C) by Vesa Karvonen

module Toys.GFlip

open Infers

/// Rules for deriving a function that reorders the arguments of a curried
/// function.
type GFlip () =
  inherit Rules ()
  static member id () = id
  static member first (ab2yz) = fun xb -> xb >> ab2yz
  static member rest (ab2axc, ac2y) = fun ab ->
    let axc = ab2axc ab
    let xac = fun x a -> axc a x
    xac >> ac2y

/// Derives a function that reorders, i.e. "generically" flips, the arguments of
/// a given function.  For this to work, arguments must have unique types.
/// Also, due to limitations of the F# type system, the function must have a
/// monomorphic type.  If the function does not have a monomorphic type, one
/// must constrain the type to be monomorphic.
let gflip f = Engine.generate<GFlip, (_ -> _) -> _ -> _> f

let test () : unit =
  gflip (printfn "%s %d %b!") true 2 "Hello"
  gflip (List.map: (int -> int) -> list<int> -> list<int>)
    [1;2] ((+) 1) = [2; 3] |> printfn "%A"
