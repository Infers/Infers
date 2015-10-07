// Copyright (C) by Vesa Karvonen

module Toys.Toys

open System.Diagnostics

let time ef =
  let start = Stopwatch.StartNew ()
  ef ()
  printf "Took %A\n\n" start.Elapsed

module GMap =
  let test () : unit =
    GMap.gmap ((+) 1) [("vesa", 1)] |> printfn "%A"
    GMap.gmap (fun (x: string) -> x.ToUpper ()) [("vesa", 1)] |> printfn "%A"

[<EntryPoint>]
let main _ =
  try
    time GMap.test
    time GFlip.test
    time GUncurry.Optimized.test
    time GUncurry.Naive.test
    time Zipper.test
    time Goat.test
    time Zebra.test
  with
   | :? System.Reflection.TargetInvocationException as e ->
     printfn "%s\n%s" e.InnerException.Message e.InnerException.StackTrace
   | e ->
     printfn "%s\n%s" e.Message e.StackTrace
  0
