#if FSHARP_NON_INTERACTIVE
namespace Infers
#endif

open System

module Rec =
  type [<InferenceRules>] Rules () =
    member rr.func () =
      let r = ref (fun _ -> failwith "rec")
      let f = fun x -> !r x
      {new Rec<'a -> 'b> () with
        override p.Get () = f
        override p.Set (f) = r := f}

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
      let r = ref (Func<'x, 'y, 'z>(fun _ -> failwith "rec"))
      let f = Func<'x, 'y, 'z>(fun x y -> (!r).Invoke (x, y))
      {new Rec<Func<'x, 'y, 'z>> () with
        override p.Get () = f
        override p.Set (f) = r := f}
