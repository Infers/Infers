// Copyright (C) by Vesa Karvonen

module Toys.GUncurry

open Infers
open Infers.Rep

////////////////////////////////////////////////////////////////////////////////

module Naive =
  type [<InferenceRules>] GUncurry () =
    member g.Uncurry (_: Rep,
                      _: Tuple<'xst>,
                      p: AsPairs<'xsa, 'xst, 'xst>,
                      u: 'xs2y -> 'xsa -> 'y) : 'xs2y -> 'xst -> 'y =
      fun xs2y -> p.ToPairs >> u xs2y
    member g.Nested u = fun x2xs2y (Pair (x, xs)) -> u (x2xs2y x) xs
    member g.Finish () = id

  /// Derives a function that uncurries a given n-ary curried function.
  let guncurry f = StaticRules<GUncurry>.Generate() f

  let test () : unit =
    guncurry (printfn "%d %s") (1, "2")
    guncurry (printfn "%s %d %f %b") ("1", 2, 3.0, true)

////////////////////////////////////////////////////////////////////////////////

module Optimized =
  // Here is a version of generic uncurrying that is optimized in the sense that
  // unnecessary copying and heap allocations are avoided when manipulating the
  // tuple of arguments.  This is done by extracting the argument tuple to a
  // nested product on stack once.  Then only references to parts of the product
  // are passed from one step to another.

  type [<AbstractClass>] GUncurry<'xs2y, 'xsa, 'y> () =
    abstract Do: 'xs2y * byref<'xsa> -> 'y

  type [<InferenceRules>] GUncurry () =
    member g.Uncurry (_: Rep,
                      _: Tuple<'xst>,
                      p: AsPairs<'xsa, 'xst, 'xst>,
                      u: GUncurry<'xs2y, 'xsa, 'y>) : 'xs2y -> 'xst -> 'y =
      fun xs2y xst ->
        let mutable xsa = p.ToPairs xst
        u.Do (xs2y, &xsa)
    member g.Nested (u: GUncurry<_, _, _>) =
      {new GUncurry<_, Pair<_, _>, _> () with
        override g.Do (x2xs2y, xxs) =
          u.Do (x2xs2y xxs.Elem, &xxs.Rest)}
    member g.Finish () =
      {new GUncurry<_, _, _> () with
        override g.Do (x2y, x) = x2y x}

  /// Derives a function that uncurries a given n-ary curried function.
  let guncurry f = StaticRules<GUncurry>.Generate() f

  let test () : unit =
    guncurry (printfn "%d %s") (1, "2")
    guncurry (printfn "%s %d %f %b") ("1", 2, 3.0, true)
