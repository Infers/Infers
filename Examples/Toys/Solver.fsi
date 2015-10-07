// Copyright (C) by Vesa Karvonen

module Toys.Solver

open Infers

/// Represents a solution to a logic puzzle.
type Solution = | Solution'0

/// Represents the result `'x` of a logic puzzle.
type Result<'x> = | Result'1

/// A framework for solving logic puzzles.  The `'Puzzle` type parameter must
/// define inference rules that can derive a `Result<'x>`.  `Solver<'Puzzle>`
/// defines a rule for a `Solution`, which prints out the result `'x`.
type [<InferenceRules>] Solver<'Puzzle> =
  new: unit -> Solver<'Puzzle>
  member Solve: 'Puzzle * Result<'x> -> Solution
