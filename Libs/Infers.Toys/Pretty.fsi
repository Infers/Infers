// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

[<AutoOpen>]
module Pretty =
  open System
  open System.Numerics
  open Infers
  open Infers.Rep
  open PPrint

  //////////////////////////////////////////////////////////////////////////////

  val pretty: 'x -> Doc

  val show: 'x -> string

  //////////////////////////////////////////////////////////////////////////////

  type Pretty<'t> = 't -> Doc
  type PrettyO<'t>
  type PrettyP<'e,'r,'o,'t>
  type PrettyS<'p,'o,'t>

  type Pretty =
    inherit Rules
    new: unit -> Pretty

    // -------------------------------------------------------------------------

    static member Enter: PrettyO<'t> -> Pretty<'t>

    // Rec ---------------------------------------------------------------------

    static member Rec: unit -> Rec<PrettyO<'t>>

    // Base Types --------------------------------------------------------------

    static member Unit: PrettyO<unit>

    static member Bool: PrettyO<bool>

    static member Integral: Integral<'t> -> PrettyO<'t>

    static member Float32: PrettyO<float32>
    static member Float64: PrettyO<float>

    static member Char: PrettyO<char>
    static member String: PrettyO<string>

    // Special -----------------------------------------------------------------

    static member Option: PrettyO<'t> -> PrettyO<option<'t>>

    static member Ref: PrettyO<'t> -> PrettyO<ref<'t>>

    static member List: PrettyO<'t> -> PrettyO<list<'t>>

    static member Array: PrettyO<'t> -> PrettyO<array<'t>>

    // Tuples and Records ------------------------------------------------------

    static member Item: Item<'e,'r,'t> * PrettyO<'e> -> PrettyP<'e,'r,'t,'t>

    static member Labelled: Labelled<'e,'r,'o,'t> * PrettyO<'e>
                         -> PrettyP<'e,'r,'o,'t>

    static member Pair: PrettyP<     'e    , Pair<'e,'r>,'o,'t>
                      * PrettyP<        'r ,         'r ,'o,'t>
                     -> PrettyP<Pair<'e,'r>, Pair<'e,'r>,'o,'t>

    static member Product: AsPairs<'p,'t,'t> * PrettyP<'p,'p,'t,'t>
                        -> PrettyO<'t>

    // Discriminated Unions ----------------------------------------------------

    static member Case: Case<Empty,'o,'t> -> PrettyS<Empty,'o,'t>

    static member Case: Case<'p,'o,'t> * PrettyP<'p,'p,'o,'t>
                     -> PrettyS<'p,'o,'t>

    static member Choice: PrettyS<       'p    , Choice<'p,'o>,'t>
                        * PrettyS<          'o ,           'o ,'t>
                       -> PrettyS<Choice<'p,'o>, Choice<'p,'o>,'t>

    static member Sum: AsChoices<'s,'t> * PrettyS<'s,'s,'t> -> PrettyO<'t>
