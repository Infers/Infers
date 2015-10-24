// Copyright (C) by Vesa Karvonen

module Toys.Solver

open System.Text.RegularExpressions
open Infers

type Solution<'Puzzle> = Solution'1

type Result<'x> = Result'1

let prefix = Regex "[a-zA-Z0-9.-]+[+.]"
let suffix = Regex "`[0-9]+"

let replace (p: Regex) (r: string) (s: string) =
  p.Replace (s, r)

type Solver () =
  inherit Rules ()
  static member Solve (_: Result<'x>) : Solution<'Puzzle> =
    sprintf "%A" typeof<'x>
    |> replace prefix ""
    |> replace suffix ""
    |> printfn "%s"
    Solution'1
