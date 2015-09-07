// Copyright (C) by Vesa Karvonen

open Infers

type GM<'w, 'p> = ('p -> 'p) -> 'w -> 'w
type GMap<'w, 'p> = {gm: GM<'w, 'p>}

type [<InferenceRules>] GMap () =
  member g.same ()                  : GMap<'p, 'p> = {gm = id}
  member g.notSame (gm: GM<'w, 'p>) : GMap<'w, 'p> = {gm = gm}

  member g.int    () : GM<int,    'p> = fun _ -> id
  member g.string () : GM<string, 'p> = fun _ -> id
  member g.bool   () : GM<bool,   'p> = fun _ -> id

  member g.option (wG: GMap<'w, 'p>) : GM<option<'w>, 'p> = wG.gm >> Option.map
  member g.pair (vG: GMap<'v, 'p>, wG: GMap<'w, 'p>) : GM<'v * 'w, 'p> =
    fun p2p (v, w) -> (vG.gm p2p v, wG.gm p2p w)
  member g.list (wG: GMap<'w, 'p>) : GM<list<'w>, 'p> =
    wG.gm >> List.map

let gmap (p2p: 'p -> 'p) : 'w -> 'w = StaticRules<GMap>.Generate () p2p

[<EntryPoint>]
let main argv = 
  gmap ((+) 1) [("vesa", 1)] |> printfn "%A"
  gmap (fun (x: string) -> x.ToUpper ()) [("vesa", 1)] |> printfn "%A"
  0
