module Infers.Eq

open System
open Infers.Rep

val eq: 'a -> 'a -> bool

type t<'a> = Func<'a, 'a, bool>
type u<'u, 'c, 'cs>
type p<'p, 'e, 'es>

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

  member case: Case<'u, Empty, 'cs>                   -> u<'u, Empty, 'cs>
  member case: Case<'u,   'ls, 'cs> * p<'u, 'ls, 'ls> -> u<'u,   'ls, 'cs>

  member choice: u<'u,        'c,       Choice<'c, 'cs>>
               * u<'u,            'cs,             'cs>
              -> u<'u, Choice<'c, 'cs>, Choice<'c, 'cs>>

  member union: Rep * Union<'u> * AsChoice<'u, 'c> * u<'u, 'c, 'c> -> t<'u>

  // Tuples and Records --------------------------------------------------------

  member product: p<'r,     'f,       And<'f, 'fs>>
                * p<'r,         'fs,          'fs>
               -> p<'r, And<'f, 'fs>, And<'f, 'fs>>

  member elem: Elem<'t, 'e, 'p> * t<'e> -> p<'t, 'e, 'p>

  member tuple: Rep * Tuple<'t> * AsProduct<'t, 'p> * p<'t, 'p, 'p> -> t<'t>

  member record: Rep * Record<'r> * AsProduct<'r, 'p> * p<'r, 'p, 'p> -> t<'r>
   when 'r: not struct
