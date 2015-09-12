// Copyright (C) by Vesa Karvonen

module Toys.GFlip

open Infers

type [<InferenceRules>] GFlip () =
  member f.id () = id
  member f.first (ab2yz) = fun xb -> xb >> ab2yz
  member f.rest (ab2axc, ac2y) = fun ab ->
    let axc = ab2axc ab
    let xac = fun x a -> axc a x
    xac >> ac2y

let gflip f = Option.get (NextGen.tryGenerate (GFlip ())) f

let test () : unit =
  gflip (printfn "%s %d %b!") true 2 "Hello"
  gflip (List.map: (int -> int) -> list<int> -> list<int>) [1;2] ((+) 1) = [2; 3] |> printfn "%A"
