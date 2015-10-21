// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module Infers.Toys.Pretty

open System
open System.Numerics
open Infers
open Infers.Rep
open PPrint

////////////////////////////////////////////////////////////////////////////////

val pretty: 'x -> Doc

val show: 'x -> string

////////////////////////////////////////////////////////////////////////////////

type Pretty<'t> = 't -> Doc
type PrettyO<'t>
type PrettyP<'e, 'r, 'o, 't>
type PrettyS<'p, 'o, 't>

type [<InferenceRules>] Pretty =
  new: unit -> Pretty

  // ---------------------------------------------------------------------------

  member Enter: Rep * Integral
              * PrettyO<'t> -> Pretty<'t>

  // Rec -----------------------------------------------------------------------

  member Rec: unit -> Rec<PrettyO<'t>>

  // Base Types ----------------------------------------------------------------

  member Unit: PrettyO<unit>

  member Bool: PrettyO<bool>

  member Integral: Integral<'t> -> PrettyO<'t>

  member Float32: PrettyO<float32>
  member Float64: PrettyO<float>

  member Char: PrettyO<char>
  member String: PrettyO<string>

  // Special -------------------------------------------------------------------

  member Option: PrettyO<'t> -> PrettyO<option<'t>>

  member Ref: PrettyO<'t> -> PrettyO<ref<'t>>

  member List: PrettyO<'t> -> PrettyO<list<'t>>

  member Array: PrettyO<'t> -> PrettyO<array<'t>>

  // Tuples and Records --------------------------------------------------------

  member Item: Item<'e, 'r, 't> * PrettyO<'e> -> PrettyP<'e, 'r, 't, 't>

  member Labelled: Labelled<'e, 'r, 'o, 't>
                  * PrettyO<'e>
                 -> PrettyP<'e, 'r, 'o, 't>

  member Pair: PrettyP<     'e      , Pair<'e, 'es>, 'o, 't>
             * PrettyP<         'es ,          'es , 'o, 't>
            -> PrettyP<Pair<'e, 'es>, Pair<'e, 'es>, 'o, 't>

  member Product: AsPairs<'p, 't, 't> * PrettyP<'p, 'p, 't, 't> -> PrettyO<'t>

  // Discriminated Unions ------------------------------------------------------

  member Case: Case<Empty, 'cs, 't> -> PrettyS<Empty, 'cs, 't>

  member Case: Case<'p, 'o, 't> * PrettyP<'p, 'p, 'o, 't> -> PrettyS<'p, 'o, 't>

  member Choice: PrettyS<       'p     , Choice<'p, 'o>, 't>
               * PrettyS<           'o ,            'o , 't>
              -> PrettyS<Choice<'p, 'o>, Choice<'p, 'o>, 't>

  member Sum: AsChoices<'s, 't> * PrettyS<'s, 's, 't> -> PrettyO<'t>
