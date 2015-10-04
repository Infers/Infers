// Copyright (C) by Vesa Karvonen

module Toys.Toys

open System.Diagnostics

let time ef =
  let start = Stopwatch.StartNew ()
  ef ()
  printf "Took %A\n\n" start.Elapsed

[<EntryPoint>]
let main _ =
  try
    time GUncurry.Optimized.test
    time GUncurry.Naive.test
    time Zipper.test
    time Goat.test
    time Zebra.test
    time GFlip.test
    time GMap.test
  with
   | :? System.Reflection.TargetInvocationException as e ->
     printfn "%s" e.InnerException.Message
   | e ->
     printfn "%s" e.Message
  0
