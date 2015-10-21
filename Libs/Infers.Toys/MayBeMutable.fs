// Copyright (C) by Vesa Karvonen

module Infers.Toys.MayBeMutable

open Infers
open Infers.Rep

////////////////////////////////////////////////////////////////////////////////

type MBM<'t> = {MBM: bool}
type MBMP<'e, 'r, 'o, 't> = {MBMP: bool}
type MBMS<'p, 'o, 't> = {MBMS: bool}

let imm = {MBM = false}
let mut = {MBM = true}
let immp = {MBMP = false}
let mutp = {MBMP = true}
let imms = {MBMS = false}
let muts = {MBMS = true}

type [<InferenceRules>] MayBeMutable () =
  inherit Rep ()

  member t.Rec () =
    {new Rec<MBM<'t>> () with
      member t.Get () = imm
      member t.Set (_) = ()}

  member t.Prim (_: Prim<'t>) : MBM<'t> = imm

  member t.String : MBM<string> = imm

  member t.Ref () : MBM<ref<_>> = mut
  member t.Array () : MBM<array<_>> = mut

  member t.Elem (_: Elem<'e, 'r, 'o, 't>, eM: MBM<'e>) : MBMP<'e, 'r, 'o, 't> =
    if eM.MBM then mutp else immp
  member t.Pair (eM: MBMP<     'e     , Pair<'e, 'r>, 'o, 't>,
                 rM: MBMP<         'r ,          'r , 'o, 't>)
                   : MBMP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
    if eM.MBMP || rM.MBMP then mutp else immp
  member t.Product (asP: AsPairs<'p, 'o, 't>, pM: MBMP<'p, 'p, 'o, 't>) =
    if asP.IsMutable || pM.MBMP then mut else imm : MBM<'t>

  member t.Case (_: Case<Empty, 'o, 't>) : MBMS<Empty, 'o, 't> = imms
  member t.Case (_: Case<'p, 'o, 't>, pM: MBMP<'p, 'p, 'o, 't>) =
    if pM.MBMP then muts else imms : MBMS<'p, 'o, 't>
  member t.Choice (pM: MBMS<       'p     , Choice<'p, 'o>, 't>,
                   oM: MBMS<           'o ,            'o , 't>)
                     : MBMS<Choice<'p, 'o>, Choice<'p, 'o>, 't> =
    if pM.MBMS || oM.MBMS then muts else imms
  member t.Sum (_: AsChoices<'s, 't>, sM: MBMS<'s, 's, 't>) : MBM<'t> =
    if sM.MBMS then mut else imm

////////////////////////////////////////////////////////////////////////////////

let mayBeMutable<'t> =
  Engine.generate<MayBeMutable, MBM<'t>>.MBM
