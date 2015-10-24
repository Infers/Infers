// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module MayBeMutable =
  open Infers
  open Infers.Rep

  //////////////////////////////////////////////////////////////////////////////

  type MBM<'t> = {MBM: bool}
  type MBMP<'e, 'r, 'o, 't> = {MBMP: bool}
  type MBMS<'p, 'o, 't> = {MBMS: bool}

  let imm = {MBM = false}
  let mut = {MBM = true}
  let immp = {MBMP = false}
  let mutp = {MBMP = true}
  let imms = {MBMS = false}
  let muts = {MBMS = true}

  type [<Rep>] MayBeMutable () =
    inherit Rules ()

    static member Rec () =
      {new Rec<MBM<'t>> () with
        member t.Get () = imm
        member t.Set (_) = ()}

    static member Prim (_: Prim<'t>) : MBM<'t> = imm

    static member String : MBM<string> = imm

    static member Ref () : MBM<ref<_>> = mut
    static member Array () : MBM<array<_>> = mut

    static member Elem (_: Elem<'e,'r,'o,'t>, eM: MBM<'e>) : MBMP<'e,'r,'o,'t> =
      if eM.MBM then mutp else immp
    static member Pair (eM: MBMP<     'e     , Pair<'e, 'r>, 'o, 't>,
                        rM: MBMP<         'r ,          'r , 'o, 't>)
                          : MBMP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
      if eM.MBMP || rM.MBMP then mutp else immp
    static member Product (asP: AsPairs<'p, 'o, 't>, pM: MBMP<'p, 'p, 'o, 't>) =
      if asP.IsMutable || pM.MBMP then mut else imm : MBM<'t>

    static member Case (_: Case<Empty, 'o, 't>) : MBMS<Empty, 'o, 't> = imms
    static member Case (_: Case<'p, 'o, 't>, pM: MBMP<'p, 'p, 'o, 't>) =
      if pM.MBMP then muts else imms : MBMS<'p, 'o, 't>
    static member Choice (pM: MBMS<       'p     , Choice<'p, 'o>, 't>,
                          oM: MBMS<           'o ,            'o , 't>)
                            : MBMS<Choice<'p, 'o>, Choice<'p, 'o>, 't> =
      if pM.MBMS || oM.MBMS then muts else imms
    static member Sum (_: AsChoices<'s, 't>, sM: MBMS<'s, 's, 't>) : MBM<'t> =
      if sM.MBMS then mut else imm

  //////////////////////////////////////////////////////////////////////////////

  let mayBeMutable<'t> = Engine.generateDFS<MayBeMutable, MBM<'t>>.MBM
