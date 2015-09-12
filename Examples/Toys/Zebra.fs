// Copyright (C) by Vesa Karvonen

module Toys.Zebra

// Title: Finding the zebra
//
// http://www.anselm.edu/internet/compsci/faculty_staff/mmalita/HOMEPAGE/logic/index.html

open Infers

open Toys.Basic
open Toys.List
open Toys.Solver

////////////////////////////////////////////////////////////////////////////////

type blue = class end
type chesterfield = class end
type coffee = class end
type dog = class end
type english = class end
type fox = class end
type green = class end
type horse = class end
type japonese = class end
type juice = class end
type kent = class end
type kool = class end
type lucky = class end
type milk = class end
type norvegian = class end
type norwegian = class end
type red = class end
type snake = class end
type spanish = class end
type tea = class end
type ukrainian = class end
type water = class end
type white = class end
type winston = class end
type yellow = class end
type zebra = class end

/// Rules for deriving the result of the Zebra puzzle.  This is a straighforward
/// translation of a Prolog program.
type [<InferenceRules>] Zebra () =
  member g.Puzzle
    (_: Basic, _: List,
     _: Eq<'s, List<'x0, 'x1, 'x2, 'x3, 'x4>>,
     _: Member<english * _ * _ * _ * red, 's>,
     _: Member<spanish * dog * _ * _ * _, 's>,
     _: Member<_ * _ * _ * coffee * green, 's>,
     _: Member<ukrainian * _ * _ * tea * _, 's>,
     _: Right<_ * _ * _ * _ * green, _ * _ * _ * _ * white, 's>,
     _: Member<_ * snake * winston * _ * _, 's>,
     _: Member<_ * _ * kool * _ * yellow, 's>,
     _: Eq<'s, List<_, _, _ * _ * _ * milk * _, _, _>>,
     _: Eq<'s, List<norwegian * _ * _ * _ * _, _, _, _, _>>,
     _: Next<_ * _ * chesterfield * _ * _, _ * fox * _ * _ * _, 's>,
     _: Next<_ * _ * kool * _ * _, _ * horse * _ * _ * _, 's>,
     _: Member<_ * _ * lucky * juice * _, 's>,
     _: Member<japonese * _ * kent * _ * _, 's>,
     _: Next<norwegian * _ * _ * _ * _, _ * _ * _ * _ * blue, 's>,
     _: Member<_ * _ * _ * water * _, 's>,
     _: Member<_ * zebra * _ * _ * _, 's>)
      : Result<'x0 * 'x1 * 'x2 * 'x3 * 'x4> = Result

let test () : unit =
  if NextGen.tryGenerate (Solver<Zebra> ()) = Some Solution
  then printfn "Got solution!"
  else printfn "No solution?"

// Tuple[Tuple[norwegian, fox,   kool,         water,  yellow],
//       Tuple[ukrainian, horse, chesterfield, tea,    blue],
//       Tuple[english,   snake, winston,      milk,   red],
//       Tuple[japonese,  zebra, kent,         coffee, green],
//       Tuple[spanish,   dog,   lucky,        juice,  white]]
