// Copyright (C) by Vesa Karvonen

module Toys.Toys

[<EntryPoint>]
let main _ =
  try
    Zipper.test ()
    Goat.test ()
    Zebra.test ()
    GFlip.test ()
    GMap.test ()
    GUncurry.test ()
  with
   | :? System.Reflection.TargetInvocationException as e ->
     printfn "%s" e.InnerException.Message
   | e ->
     printfn "%s" e.Message
  0
