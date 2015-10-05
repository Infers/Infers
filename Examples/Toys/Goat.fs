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

type Opp<'x, 'y> = | Opp'2

type Valid<'s> = | Valid'1

type Move<'s1, 's2> = | Move'2

type Farmer<'x> = class end
type Goat<'x> = class end
type Cabbage<'x> = class end
type Wolf<'x> = class end

type Search<'p, 'r> = | Search'2

/// Rules for solving the Goat problem.  This is similar to the original Prolog
/// program, but has been modified in a couple of ways.  Most importantly,
/// negation is not supported by Infers, so the rules have been changed to
/// eliminate negation.
type [<InferenceRules>] Goat () =
  member g.Puzzle
    (_: Basic, _: List,
     _: Eq<'initial, Farmer<S> * Goat<S> * Cabbage<S> * Wolf<S>>,
     _: Eq<'final, Farmer<D> * Goat<D> * Cabbage<D> * Wolf<D>>,
     _: Search<Cons<'initial, 'p>, 'final>)
      : Result<Cons<'initial, 'p>> = Result'1

  member g.Opp1: Opp<S, D> = Opp'2
  member g.Opp2: Opp<D, S> = Opp'2

  member g.Valid (_: Or<Opp<'g, 'c>, Eq<'g, 'c, 'f>>,
                  _: Or<Opp<'g, 'w>, Eq<'g, 'w, 'f>>)
   : Valid<Farmer<'f> * Goat<'g> * Cabbage<'c> * Wolf<'w>> = Valid'1

  member g.Goat (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'f1> * Cabbage<'c> * Wolf<'w>,
          Farmer<'f2> * Goat<'f2> * Cabbage<'c> * Wolf<'w>> = Move'2
  member g.Cabbage (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'g> * Cabbage<'f1> * Wolf<'w>,
          Farmer<'f2> * Goat<'g> * Cabbage<'f2> * Wolf<'w>> = Move'2
  member g.Wolf (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'g> * Cabbage<'c> * Wolf<'f1>,
          Farmer<'f2> * Goat<'g> * Cabbage<'c> * Wolf<'f2>> = Move'2
  member g.Alone (_: Opp<'f1, 'f2>)
   : Move<Farmer<'f1> * Goat<'g> * Cabbage<'c> * Wolf<'w>,
          Farmer<'f2> * Goat<'g> * Cabbage<'c> * Wolf<'w>> = Move'2

  member g.Search () : Search<List<'d>, 'd> = Search'2
  member g.Search (_: Move<'s1, 's2>,
                   _: Valid<'s2>,
                   _: Search<Cons<'s2, 'p>, 'd>)
                    : Search<Cons<'s1, Cons<'s2, 'p>>, 'd> = Search'2
        
let test () : unit =
  if Engine.tryGenerate (Solver<Goat> ()) = Some Solution'0
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
