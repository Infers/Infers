// Copyright (C) by Vesa Karvonen

module Toys.Goat

// Title: The goat problem
//
// http://www.anselm.edu/internet/compsci/faculty_staff/mmalita/HOMEPAGE/logic/index.html

open Infers
open Toys.Basic
open Toys.List
open Toys.Solver

type S = class end
type D = class end

type Opp<'x, 'y> = | Opp

type Valid<'s> = | Valid

type Move<'s1, 's2> = | Move

type Farmer<'x> = class end
type Goat<'x> = class end
type Cabbage<'x> = class end
type Wolf<'x> = class end

type Search<'p, 'r> = | Search

type [<InferenceRules>] Goat () =
  member g.Puzzle
    (_: Basic, _: List,
     _: Eq<'initial, Farmer<S> * Goat<S> * Cabbage<S> * Wolf<S>>,
     _: Eq<'final, Farmer<D> * Goat<D> * Cabbage<D> * Wolf<D>>,
     _: Search<Cons<'initial, 'p>, 'final>)
      : Result<Cons<'initial, 'p>> = Result

  member g.Opp1: Opp<S, D> = Opp
  member g.Opp2: Opp<D, S> = Opp

  member g.Valid (_: Or<Opp<'g, 'c>, Eq<'g, 'c, 'f>>,
                  _: Or<Opp<'g, 'w>, Eq<'g, 'w, 'f>>)
   : Valid<Farmer<'f> * Goat<'g> * Cabbage<'c> * Wolf<'w>> = Valid

  member g.Goat (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'f1> * Cabbage<'c> * Wolf<'w>,
          Farmer<'f2> * Goat<'f2> * Cabbage<'c> * Wolf<'w>> = Move
  member g.Cabbage (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'g> * Cabbage<'f1> * Wolf<'w>,
          Farmer<'f2> * Goat<'g> * Cabbage<'f2> * Wolf<'w>> = Move
  member g.Wolf (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'g> * Cabbage<'c> * Wolf<'f1>,
          Farmer<'f2> * Goat<'g> * Cabbage<'c> * Wolf<'f2>> = Move
  member g.Alone (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'g> * Cabbage<'c> * Wolf<'w>,
          Farmer<'f2> * Goat<'g> * Cabbage<'c> * Wolf<'w>> = Move

  member g.Search () : Search<List<'d>, 'd> = Search
  member g.Search (_: Move<'s1, 's2>,
                   _: Valid<'s2>,
                   _: Search<Cons<'s2, 'p>, 'd>)
                    : Search<Cons<'s1, Cons<'s2, 'p>>, 'd> = Search
        
let test () : unit =
  if NextGen.tryGenerate (Solver<Goat> ()) = Some Solution
  then printfn "Got solution!"
  else printfn "No solution?"

// Cons[Tuple[Farmer[S],Goat[S],Cabbage[S],Wolf[S]],
// Cons[Tuple[Farmer[D],Goat[D],Cabbage[S],Wolf[S]],
// Cons[Tuple[Farmer[S],Goat[D],Cabbage[S],Wolf[S]],
// Cons[Tuple[Farmer[D],Goat[D],Cabbage[S],Wolf[D]],
// Cons[Tuple[Farmer[S],Goat[S],Cabbage[S],Wolf[D]],
// Cons[Tuple[Farmer[D],Goat[S],Cabbage[D],Wolf[D]],
// Cons[Tuple[Farmer[S],Goat[S],Cabbage[D],Wolf[D]],
// Cons[Tuple[Farmer[D],Goat[D],Cabbage[D],Wolf[D]],
// Nil]]]]]]]]
