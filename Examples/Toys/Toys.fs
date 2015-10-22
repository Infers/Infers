// Copyright (C) by Vesa Karvonen

module Toys.Toys

open System.Diagnostics
open Infers.Rep
open Infers.Toys

let time ef =
  let start = Stopwatch.StartNew ()
  ef ()
  printf "Took %A\n\n" start.Elapsed

module GMap =
  type P = {name: string; number: int}

  let test () : unit =
    GMap.gmap ((+) 1) [{name = "vesa"; number = 1}] |> printfn "%A"
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
    do let xs = [1;1;1;1;1;1]
       printfn "%A" xs
       let inc x = x := !x+1 ; !x
       let i = ref 0
       xs |> toZipperThe |> mapTopDownThe (function [] -> [] | _::xs -> inc i::xs) |> fromZipper |> printfn "%A"
       let i = ref 0
       xs |> toZipperThe |> mapBottomUpThe (function [] -> [] | _::xs -> inc i::xs) |> fromZipper |> printfn "%A"
    do let x = App (Lambda ("x", If (Var "x", Var "a", Var "a")), Var "true")
       printfn "%A" x
       let z = toZipperThe x
       printfn "%A" (getThe z)
       do let i = ref 0
          let f = function Var x -> i := !i+1 ; Var (sprintf "%s/%d" x !i)
                         | other -> other
          let z = mapTopDownThe f z
          printfn "%A" (getThe z)
       do let i = ref 0
          let f = function Var x -> i := !i+1 ; Var (sprintf "%s/%d" x !i)
                         | other -> other
          let z = mapBottomUpThe f z
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
  let test () =
    let t : Pair<int, Pair<float, string>> =
      Iso.convert (Pair (Pair (1.0, "2"), 3))
    printfn "%A" t

module Elems =
  type Range = R of int
  type Term =
    | Var of string * Range
    | Lambda of string * Term * Range
    | App of Term * Term * Range
    | If of Term * Term * Term * Range

  let allRanges = elemsDn<Range, Term>
  let incRanges = substUp (fun (R i) -> R (i+1))

  let term = App (Lambda ("x",
                          If (Var ("x", R 7),
                              Var ("a", R 6),
                              Var ("a", R 5),
                              R 4), R 3),
                  Var ("true", R 2),
                  R 1)

  let test () =
    allRanges term
    |> Array.ofSeq
    |> printfn "%A"

    incRanges term
    |> printfn "%A"

module PU =
  type Easy = {too: list<string>; easy: float * int}
  type Tricky = {Val: int; Tricky: Tricky}

  let test () =
    printfn "%A" (pickle {too=["1";"2"]; easy=(3.0, 4)} |> unpickle : Easy)
    let rec tricky = {Val = 1; Tricky = {Val = 2; Tricky = tricky}}
    let p = pickle tricky
    let t : Tricky = unpickle p
    printfn "%A %A %A" t.Val t.Tricky.Val t.Tricky.Tricky.Val

module Pretty =
  type Foo = {bar: string}
  let test () =
    show (Some 10, [2], [|{bar = "3"}|]) |> printfn "%s"

[<EntryPoint>]
let main _ =
  try
    time Elems.test
    time Iso.test
    time GMap.test
    time GFlip.test
    time GUncurry.Optimized.test
    time GUncurry.Naive.test
    time Zipper.test
    time Goat.test
    time PU.test
    time Pretty.test
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
