module Infers.Pickle

open System
open System.IO
open Infers.Rep

val pickle: BinaryWriter -> 'a -> unit
val unpickle: BinaryReader -> 'a

type [<AbstractClass>] t<'a> =
  abstract Pickle: BinaryWriter * 'a -> unit
  abstract Unpickle: BinaryReader -> 'a

type [<AbstractClass>] p<'e, 'es, 'p> =
  abstract Pickle: BinaryWriter * byref<'e> -> unit
  abstract Unpickle: BinaryReader * byref<'e> -> unit

type u<'c, 'cs, 'u>

type [<InferenceRules>] Pickle =
  new: unit -> Pickle

  member rep: Rep

  member fix: unit -> Rec<t<'x>>

  // Base Types ----------------------------------------------------------------

  member unit: t<unit>

  member bool: t<bool>

  member int8:  t<int8>
  member int16: t<int16>
  member int32: t<int32>
  member int64: t<int64>

  member uint8:  t<uint8>
  member uint16: t<uint16>
  member uint32: t<uint32>
  member uint64: t<uint64>

  member float32: t<float32>
  member float64: t<float>

  member char: t<char>
  member string: t<string>

  // Special optimizations -----------------------------------------------------

  member list: t<'a> -> t<list<'a>>
  //member bytes: t<array<byte>>

  // Refs and Arrays -----------------------------------------------------------

  //member ref: t<'a> -> t<ref<'a>>
  member array: t<'a> -> t<array<'a>>

  // Discriminated Unions ------------------------------------------------------

  member case: Case<Empty, 'cs, 'u>                   -> u<Empty, 'cs, 'u>
  member case: Case<  'ls, 'cs, 'u> * p<'ls, 'ls, 'u> -> u<  'ls, 'cs, 'u>

  member choice: u<       'c      , Choice<'c, 'cs>, 'u>
               * u<           'cs ,            'cs , 'u>
              -> u<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u>

  member union: Rep * Union<'u> * AsChoice<'c, 'u> * u<'c, 'c, 'u> -> t<'u>

  // Tuples and Records --------------------------------------------------------

  member product: p<    'f      , And<'f, 'fs>, 'r>
                * p<        'fs ,         'fs , 'r>
               -> p<And<'f, 'fs>, And<'f, 'fs>, 'r>

  member elem: Elem<'e, 'p, 't> * t<'e> -> p<'e, 'p, 't>

  member tuple: Rep * Tuple<'t> * AsProduct<'p, 't> * p<'p, 'p, 't> -> t<'t>

  member record: Rep * Record<'r> * AsProduct<'p, 'r> * p<'p, 'p, 'r> -> t<'r>
