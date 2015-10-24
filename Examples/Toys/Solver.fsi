// Copyright (C) by Vesa Karvonen

module Toys.Solver

open Infers

/// Represents a solution to a logic puzzle.
type Solution<'Puzzle> = Solution'1

/// Represents the result `'x` of a logic puzzle.
type Result<'x> = Result'1

/// A framework for solving logic puzzles.  The `'Puzzle` type parameter must
/// define inference rules that can derive a `Result<'x>`.  `Solver<'Puzzle>`
/// defines a rule for a `Solution`, which prints out the result `'x`.
type Solver =
  inherit Rules
  new: unit -> Solver
  static member Solve: Result<'x> -> Solution<'Puzzle>
