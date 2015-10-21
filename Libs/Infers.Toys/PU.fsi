// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module PU =
  open Infers
  open Infers.Rep

  //////////////////////////////////////////////////////////////////////////////

  /// Converts the given value to an array of bytes.
  val pickle: 'x -> array<byte>

  /// Converts an array of bytes produced by `pickle` into a value.  The type of
  /// the result must match the type that was given to `pickle`.
  val unpickle: array<byte> -> 'x

  //////////////////////////////////////////////////////////////////////////////

  type PU<'x>
  type PUP<'e, 'r, 'o, 't>
  type PUS<'p, 'o, 't>

  type [<InferenceRules>] PU =
    inherit Rep

    new: unit -> PU

    member Rec: unit -> Rec<PU<'t>>

    member Int: PU<int>
    member Float: PU<float>
    member String: PU<string>

    member Elem: Elem<'e, 'r, 'o, 't> * PU<'e> -> PUP<'e, 'r, 'o, 't>

    member Pair: PUP<     'e     , Pair<'e, 'r>, 'o, 't>
               * PUP<         'r ,          'r , 'o, 't>
              -> PUP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't>

    member Tuple: Tuple<'t> * AsPairs<'p,'o,'t> * PUP<'p,'p,'o,'t> -> PU<'t>

    member Record: Record<'t> * AsPairs<'p,'o,'t> * PUP<'p,'p,'o,'t> -> PU<'t>

    member Case: Case<Empty, 'o, 't> -> PUS<Empty, 'o, 't>
    member Case: Case<'p, 'o, 't> * PUP<'p, 'p, 'o, 't> -> PUS<'p, 'o, 't>

    member Choice: PUS<       'p     , Choice<'p, 'o>, 't>
                 * PUS<           'o ,            'o , 't>
                -> PUS<Choice<'p, 'o>, Choice<'p, 'o>, 't>

    member Sum: AsChoices<'s, 't> * PUS<'s, 's, 't> -> PU<'t>
