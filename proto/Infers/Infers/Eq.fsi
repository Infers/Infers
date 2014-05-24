module Infers.Eq

open System
open Infers.Rep

val eq: 'a -> 'a -> bool

type t<'a> = Func<'a, 'a, bool>
type u<'c, 'cs, 'u>
type p<'e, 'es, 'p>

type [<InferenceRules>] Eq =
  new: unit -> Eq

  // Rules ---------------------------------------------------------------------

  member recFn: RecFn
  member rep: Rep

  // Rec -----------------------------------------------------------------------

  member fix: RecFn -> Rec<t<'x>>

  // Base Types ----------------------------------------------------------------

  member unit: t<unit>

  member bool: t<bool>

  member int8: t<int8>
  member int16: t<int16>
  member int32: t<int32>
  member int64: t<int64>

  member uint8: t<uint8>
  member uint16: t<uint16>
  member uint32: t<uint32>
  member uint64: t<uint64>

  member float32: t<float32>
  member float64: t<float>

  member char: t<char>
  member string: t<string>

  // Refs and Arrays -----------------------------------------------------------

  member ref: unit -> t<ref<'a>>
  member array: unit -> t<array<'a>>

  // Discriminated Unions ------------------------------------------------------

  member case: Case<Empty, 'cs, 'u>                   -> u<Empty, 'cs, 'u>
  member case: Case<  'ls, 'cs, 'u> * p<'ls, 'ls, 'u> -> u<  'ls, 'cs, 'u>

  member plus: u<       'c      , Choice<'c, 'cs>, 'u>
             * u<           'cs ,            'cs , 'u>
            -> u<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u>

  member union: Rep * Union<'u> * AsChoice<'c, 'u> * u<'c, 'c, 'u> -> t<'u>

  // Tuples and Records --------------------------------------------------------

  member elem: Elem<'e, 'p, 't> * t<'e> -> p<'e, 'p, 't>

  member times: p<    'f      , And<'f, 'fs>, 'r>
              * p<        'fs ,         'fs , 'r>
             -> p<And<'f, 'fs>, And<'f, 'fs>, 'r>

  member product: Rep * Product<'t> * AsProduct<'p, 't> * p<'p, 'p, 't> -> t<'t>
   when 't: not struct
