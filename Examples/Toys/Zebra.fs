// Copyright (C) by Vesa Karvonen

module Toys.Zebra

// Title: Finding the zebra
//
// https://en.wikipedia.org/wiki/Zebra_Puzzle
// http://www.anselm.edu/internet/compsci/faculty_staff/mmalita/HOMEPAGE/logic/index.html
//
//  1. There are five houses.
//  2. The Englishman lives in the red house.
//  3. The Spaniard owns the dog.
//  4. Coffee is drunk in the green house.
//  5. The Ukrainian drinks tea.
//  6. The green house is immediately to the right of the ivory house.
//  7. The Old Gold smoker owns snails.
//  8. Kools are smoked in the yellow house.
//  9. Milk is drunk in the middle house.
// 10. The Norwegian lives in the first house.
// 11. The man who smokes Chesterfields lives in the house next to the man with
//     the fox.
// 12. Kools are smoked in the house next to the house where the horse is kept.
// 13. The Lucky Strike smoker drinks orange juice.
// 14. The Japanese smokes Parliaments.
// 15. The Norwegian lives next to the blue house.
//
// Now, who drinks water?  Who owns the zebra?
//
// In the interest of clarity, it must be added that each of the five houses is
// painted a different color, and their inhabitants are of different national
// extractions, own different pets, drink different beverages and smoke
// different brands of American cigarets [sic].  One other thing: in statement
// 6, right means your right.

open Infers

open Toys.Basic
open Toys.List
open Toys.Solver

////////////////////////////////////////////////////////////////////////////////

type Englishman = class end
type Japanese = class end
type Norwegian = class end
type Spaniard = class end
type Ukrainian = class end

type Dog = class end
type Fox = class end
type Horse = class end
type Snails = class end
type Zebra = class end

type Chesterfields = class end
type Kools = class end
type LuckyStrikes = class end
type OldGolds = class end
type Parliaments = class end

type Coffee = class end
type Milk = class end
type OrangeJuice = class end
type Tea = class end
type Water = class end

type Blue = class end
type Green = class end
type Ivory = class end
type Red = class end
type Yellow = class end

/// Rules for deriving the result of the Zebra puzzle.  This is a straighforward
/// translation of a Prolog program.
type [<InferenceRules>] Puzzle () =
  member g.Puzzle
    (_: Basic, _: List,
     _: Eq<'s, List<'x0, 'x1, 'x2, 'x3, 'x4>>,
     _: Member<Englishman * _ * _ * _ * Red, 's>,
     _: Member<Spaniard * Dog * _ * _ * _, 's>,
     _: Member<_ * _ * _ * Coffee * Green, 's>,
     _: Member<Ukrainian * _ * _ * Tea * _, 's>,
     _: Sublist<List<_ * _ * _ * _ * Ivory, _ * _ * _ * _ * Green>, 's>,
     _: Member<_ * Snails * OldGolds * _ * _, 's>,
     _: Member<_ * _ * Kools * _ * Yellow, 's>,
     _: Eq<'s, List<_, _, _ * _ * _ * Milk * _, _, _>>,
     _: Eq<'s, List<Norwegian * _ * _ * _ * _, _, _, _, _>>,
     _: Adjacent<_ * _ * Chesterfields * _ * _, _ * Fox * _ * _ * _, 's>,
     _: Adjacent<_ * _ * Kools * _ * _, _ * Horse * _ * _ * _, 's>,
     _: Member<_ * _ * LuckyStrikes * OrangeJuice * _, 's>,
     _: Member<Japanese * _ * Parliaments * _ * _, 's>,
     _: Adjacent<Norwegian * _ * _ * _ * _, _ * _ * _ * _ * Blue, 's>,
     _: Member<_ * _ * _ * Water * _, 's>,
     _: Member<_ * Zebra * _ * _ * _, 's>)
      : Result<'x0 * 'x1 * 'x2 * 'x3 * 'x4> = Result

let test () : unit =
  if Engine.tryGenerate (Solver<Puzzle> ()) = Some Solution
  then printfn "Got solution!"
  else printfn "No solution?"

// Tuple[Tuple[Norwegian,  Fox,    Kools,         Water,       Yellow],
//       Tuple[Ukrainian,  Horse,  Chesterfields, Tea,         Blue],
//       Tuple[Englishman, Snails, OldGolds,      Milk,        Red],
//       Tuple[Spaniard,   Dog,    LuckyStrikes,  OrangeJuice, Ivory],
//       Tuple[Japanese,   Zebra,  Parliaments,   Coffee,      Green]]
