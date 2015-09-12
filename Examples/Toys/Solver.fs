// Copyright (C) by Vesa Karvonen

module Toys.Solver

open System.Text.RegularExpressions
open Infers

type Solution = | Solution
type Result<'x> = | Result

let private prefix = Regex "[a-zA-Z0-9.-]+[+.]"
let private suffix = Regex "`[0-9]+"

let private replace (p: Regex) (r: string) (s: string) =
  p.Replace (s, r)

type [<PureInferenceRules>] Solver<'Puzzle> () =
  member g.Solve (_: 'Puzzle, _: Result<'x>) =
    sprintf "%A" typeof<'x>
    |> replace prefix ""
    |> replace suffix ""
    |> printfn "%s"
    Solution
