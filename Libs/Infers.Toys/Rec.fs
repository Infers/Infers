// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Rec =
  open System
  open Infers
  open Infers.Rep

  type RecVal<'t> =
    inherit Rec<'t>
    [<DefaultValue>] val mutable Rec: 't
    override rv.Get () = rv.Rec
    override rv.Set (v) = rv.Rec <- v
    new (mk) as rv = {inherit Rec<_>()} then rv.Rec <- mk rv

  let inline recVal<'t> mk = RecVal<'t> mk :> Rec<'t>

  type [<AbstractClass>] RecI<'t> () =
    abstract Get: byref<'t> -> unit
    abstract Set: byref<'t> -> unit

  type RecP<'e, 'r, 'o, 't> = P of RecI<'e>

  type [<Rep>] Rec () =
    inherit Rules ()

    static member Fun () = recVal ^ fun r -> fun x -> r.Rec x
    static member Func0 () =
      recVal ^ fun r -> Func<'x>(fun () -> r.Rec.Invoke ())
    static member Func1 () =
      recVal ^ fun r -> Func<'x,'y>(fun x -> r.Rec.Invoke x)
    static member Func2 () =
      recVal ^ fun r -> Func<'x,'y,'z>(fun x y -> r.Rec.Invoke (x, y))

    static member Elem (_: Elem<'e,'r,'o,'t>, eR: Rec<'e>) =
      P {new RecI<_> () with
          override pR.Get e = e <- eR.Get ()
          override pR.Set e = eR.Set e} : RecP<'e,'r,'o,'t>

    static member Pair (P eR: RecP<     'e,     Pair<'e,'r>,'o,'t>,
                        P rR: RecP<        'r ,         'r ,'o,'t>)
                            : RecP<Pair<_ , _>, Pair<'e,'r>,'o,'t> =
      P {new RecI<_> () with
          override pR.Get er = eR.Get (&er.Elem) ; rR.Get (&er.Rest)
          override pR.Set er = eR.Set (&er.Elem) ; rR.Set (&er.Rest)}

    static member Product (m: AsPairs<'p,'o,'t>, P pR: RecP<'p,'p,'o,'t>) =
      {new Rec<_> () with
        override tR.Get () =
          let mutable p = Unchecked.defaultof<_>
          pR.Get (&p)
          m.Create (&p)
        override tR.Set t =
          let mutable p = m.ToPairs t
          pR.Set (&p)}
