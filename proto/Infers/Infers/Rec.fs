namespace Infers

open System

type MutableRec<'x> () =
  inherit Rec<'x> ()
  [<DefaultValue>] val mutable Value: 'x
  override this.Get () = this.Value
  override this.Set (value) = this.Value <- value

module MutableRec =
  let create (mk) =
    let r = MutableRec<'x> ()
    r.Value <- mk r
    r :> Rec<'x>

type [<InferenceRules>] RecFn () =
  member rr.func () =
    MutableRec.create (fun r -> fun x -> r.Value x)

  member rr.func0 () =
    let r = ref (Func<'x>(fun _ -> failwith "rec"))
    let f = Func<'x>(fun () -> (!r).Invoke ())
    {new Rec<Func<'x>> () with
      override p.Get () = f
      override p.Set (f) = r := f}

  member rr.func1 () =
    let r = ref (Func<'x, 'y>(fun _ -> failwith "rec"))
    let f = Func<'x, 'y>(fun x -> (!r).Invoke (x))
    {new Rec<Func<'x, 'y>> () with
      override p.Get () = f
      override p.Set (f) = r := f}

  member rr.func2 () =
    MutableRec.create (fun r -> Func<'x, 'y, 'z>(fun x y -> r.Value.Invoke (x, y)))
