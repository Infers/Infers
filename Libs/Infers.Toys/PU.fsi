// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

/// This is a toy example of a binary pickler / unpickler.  This can handle
/// integral types, floats, strings, tuples, records, and union types.
/// Recursive types, such as lists, and recursive values, via records, are
/// supported.  Other types, including arbitrary classes or structs, are not
/// supported.
///
/// This could be improved in various ways.  Examples:
///
/// - Pickles do not contain any error checking information.  It would be
/// straighforward to add, for example, a hash of the type structure to the
/// beginning of the pickle and verify it when unpickling to help to detect type
/// errors.
///
/// - Support for various special types such as arrays and refs is not
/// implemented.  Such support could be added in a straightforward manner.
///
/// - Lists are pickled via naive recursive encoding.  Lists could be
/// implemented via (not yet implemented) array support.
///
/// Perhaps the main point here is that it doesn't really take all that much
/// code to implement a fairly powerful pickler.
[<AutoOpen>]
module PU =
  open Infers
  open Infers.Rep

  //////////////////////////////////////////////////////////////////////////////

  /// Converts the given value to an array of bytes.
  val pickle<'x> : 'x -> array<byte>

  /// Converts an array of bytes produced by `pickle` into a value.  The type of
  /// the result must match the type that was given to `pickle`.
  val unpickle<'x> : array<byte> -> 'x

  //////////////////////////////////////////////////////////////////////////////

  type PU<'x>
  type PUP<'e, 'r, 'o, 't>
  type PUS<'p, 'o, 't>

  type PU =
    inherit Rules
    new: unit -> PU

    static member Rec: unit -> Rec<PU<'t>>

    static member Unit: PU<unit>

    static member UInt8: PU<uint8>
    static member Int16: PU<int16>
    static member Int32: PU<int32>
    static member Int64: PU<int64>

    static member Bitwise: Bitwise<'b,'t> * PU<'b> -> PU<'t>

    static member String: PU<string>

    static member Elem: Elem<'e,'r,'o,'t> * PU<'e> -> PUP<'e,'r,'o,'t>

    static member Pair: PUP<     'e    , Pair<'e,'r>,'o,'t>
                      * PUP<        'r ,         'r ,'o,'t>
                     -> PUP<Pair<'e,'r>, Pair<'e,'r>,'o,'t>

    static member Tuple: Tuple<'t> * AsPairs<'p,'o,'t> * PUP<'p,'p,'o,'t>
                      -> PU<'t>

    static member Record: Record<'t> * AsPairs<'p,'o,'t> * PUP<'p,'p,'o,'t>
                       -> PU<'t>

    static member Case: Case<Empty,'o,'t> -> PUS<Empty,'o,'t>
    static member Case: Case<'p,'o,'t> * PUP<'p,'p,'o,'t> -> PUS<'p,'o,'t>

    static member Choice: PUS<       'p    , Choice<'p,'o>,'t>
                        * PUS<          'o ,           'o ,'t>
                       -> PUS<Choice<'p,'o>, Choice<'p,'o>,'t>

    static member Sum: AsChoices<'s,'t> * PUS<'s,'s,'t> -> PU<'t>
