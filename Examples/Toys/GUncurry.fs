// Copyright (C) by Vesa Karvonen

module GUncurry

open Infers
open Infers.Rep

type [<AbstractClass>] GUncurry<'xs, 'xs2y, 'y> () =
  abstract Do: byref<'xs> * 'xs2y -> 'y

type [<InferenceRules (StaticMap = StaticMap.Results)>] GUncurry () =
  member g.Uncurry (_: Rep,
                    _: Tuple<'xs>,
                    p: AsProduct<'xsp, 'xs>,
                    u: GUncurry<'xsp, 'xs2y, 'y>) : 'xs2y -> 'xs -> 'y =
    fun xs2y xs ->
      let mutable xsp = p.ToProduct xs
      u.Do (&xsp, xs2y)
  member g.Uncurry (u: GUncurry<'xs, 'xs2y, 'y>) : GUncurry<And<'x, 'xs>, 'x -> 'xs2y, 'y> =
    {new GUncurry<_, _, _> () with
      override t.Do (xxs, xxs2y) =
        u.Do (&xxs.Rest, xxs2y xxs.Elem)}
  member g.Uncurry () : GUncurry<'x, 'x -> 'y, 'y> =
    {new GUncurry<_, _, _> () with
      override t.Do (x, x2y) = x2y x}

let guncurry f = StaticRules<GUncurry>.Generate () f

let test () : unit =
  guncurry (printfn "%d %s") (1, "2")
  guncurry (printfn "%s %d %f %b") ("1", 2, 3.0, true)
