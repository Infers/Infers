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

module Zipper =
  open Zipper

  type Term =
    | Var of string
    | Lambda of string * Term
    | App of Term * Term
    | If of Term * Term * Term

  let test () =
    do let x = 1
       printfn "%A" x
       let z = toZipperAny x
       printfn "%d" (getAny z |> Option.get)
    do let x = (1, 2.0)
       printfn "%A" x
       let z = toZipperAny x
       let z = z |> downHeadAny |> Option.get
       printfn "%d" (getAny z |> Option.get)
       let z = z |> nextAny |> Option.get
       printfn "%f" (getAny z |> Option.get)
    do let x = (1, 2.0, "3")
       printfn "%A" x
       let z = toZipperAny x
       let z = z |> downLastAny |> Option.get
       printfn "%s" (getAny z |> Option.get)
       let z = z |> prevAny |> Option.get
       printfn "%f" (getAny z |> Option.get)
       let z = setAny 2.5 z |> Option.get
       printfn "%f" (getAny z |> Option.get)
       let z = z |> prevAny |> Option.get
       printfn "%d" (getAny z |> Option.get)
       printfn "%A" (fromZipper z)
    do let x = App (Lambda ("x", If (Var "x", Var "a", Var "a")), Var "true")
       printfn "%A" x
       let z = toZipperThe x
       printfn "%A" (getThe z)
       let z = z |> downHeadThe |> Option.get
       printfn "%A" (getThe z)
       let z = z |> downHeadThe |> Option.get
       printfn "%A" (getThe z)
       let z = z |> downHeadThe |> Option.get
       printfn "%A" (getThe z)
       let z = z |> nextThe |> Option.get
       printfn "%A" (getThe z)
       let z = z |> nextThe |> Option.get
       printfn "%A" (getThe z)
       let z = z |> setThe (Var "b")
       printfn "%A" (getThe z)
       printfn "%A" (fromZipper z)

module Iso =
  module Iso = Toys.Iso
  open Infers.Rep

  let test () =
    let t : And<int, And<float, string>> = Iso.convert (And (And(1.0, "2"), 3))
    printfn "%A" t

[<EntryPoint>]
let main _ =
  try
    time Iso.test
    time GMap.test
    time GFlip.test
    time GUncurry.Optimized.test
    time GUncurry.Naive.test
    time Zipper.test
    time Goat.test
    time Zebra.test
  with
   | e ->
     let rec explain (e: System.Exception) =
       match e.InnerException with
        | null -> ()
        | e -> explain e
       printfn "%s\n%s" e.Message e.StackTrace
     explain e
  0
