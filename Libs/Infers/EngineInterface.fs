// Copyright (C) by Vesa Karvonen

namespace Infers

open System

type Members =
  | PublicOnly = 1
  | PublicAndPrivate = 3

type StaticMap =
  | Nothing = 0
  | Results = 1

type InferenceRules () =
  inherit Attribute ()
  let mutable members = Members.PublicOnly
  let mutable staticMap = StaticMap.Nothing
  member this.Members with get () = members and set v = members <- v
  member this.StaticMap with get () = staticMap and set v = staticMap <- v

type PureInferenceRules () =
  inherit InferenceRules (StaticMap = StaticMap.Results)

type [<AbstractClass>] Rec<'x> () =
  abstract Get: unit -> 'x
  abstract Set: 'x -> unit
  // XXX This generates warning FS0050 as the IRecObj interface does not appear
  // in the signature.  This is an internal implementation detail that users
  // should never need to care about and makes it possible to invoke the typed,
  // user defined Get and Set functions dynamically without having to use
  // reflection.  If there is a better way to achieve this in F#, then I'd love
  // to know about it, because being able to have this kind of internal
  // functionality is a fairly common need.
  interface IRecObj with
   override this.GetObj () = box (this.Get ())
   override this.SetObj x = this.Set (unbox<'x> x)

exception Backtrack
