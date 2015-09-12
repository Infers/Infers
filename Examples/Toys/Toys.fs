// Copyright (C) by Vesa Karvonen

module Toys.Toys

[<EntryPoint>]
let main _ =
  try
    Goat.test ()
    Zebra.test ()
    GFlip.test ()
    GMap.test ()
    GUncurry.test ()
  with e ->
    printfn "%s" e.Message
  0
