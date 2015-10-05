// Copyright (C) by Vesa Karvonen

module Toys.Solver

open System.Text.RegularExpressions
open Infers

/// Represents a solution to a logic puzzle.
type Solution = | Solution'0

/// Represents the result `'x` of a logic puzzle.
type Result<'x> = | Result'1

let private prefix = Regex "[a-zA-Z0-9.-]+[+.]"
let private suffix = Regex "`[0-9]+"

let private replace (p: Regex) (r: string) (s: string) =
  p.Replace (s, r)

/// A framework for solving logic puzzles.  The `'Puzzle` type parameter must
/// define inference rules that can derive a `Result<'x>`.  `Solver<'Puzzle>`
/// defines a rule for a `Solution`, which prints out the result `'x`.
type [<InferenceRules>] Solver<'Puzzle> () =
  member g.Solve (_: 'Puzzle, _: Result<'x>) =
    sprintf "%A" typeof<'x>
    |> replace prefix ""
    |> replace suffix ""
    |> printfn "%s"
    Solution'0
