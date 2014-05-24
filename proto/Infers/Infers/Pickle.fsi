module Infers.Pickle

open System
open System.IO
open Infers.Rep

val pickle: BinaryWriter -> 'a -> unit
val unpickle: BinaryReader -> 'a

type [<AbstractClass>] t<'a> =
  abstract Pickle: BinaryWriter * 'a -> unit
  abstract Unpickle: BinaryReader -> 'a

type [<AbstractClass>] p<'p, 'e, 'es> =
  abstract Pickle: BinaryWriter * byref<'e> -> unit
  abstract Unpickle: BinaryReader * byref<'e> -> unit

type u<'u, 'c, 'cs>

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
