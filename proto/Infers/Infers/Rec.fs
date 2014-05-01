#if FSHARP_NON_INTERACTIVE
module Infers.Rec
#endif

open System

type [<RecursionRules>] RecFun () =
  member rf.Tier (_: 'a -> 'b) : ref<'a -> 'b> =
    ref (fun _ -> failwith "Tier")
  member rf.Untie (r: ref<'a -> 'b>) : ('a -> 'b) = fun x -> !r x
  member rf.Tie (r: ref<'a -> 'b>, f: 'a -> 'b) : unit = r := f

type [<RecursionRules>] RecFunc0 () =
  member rf.Tier (_: Func<'a>) : ref<Func<'a>> =
    ref (Func<'a>(fun _ -> failwith "Tier"))
  member rf.Untie (r: ref<Func<'a>>) : Func<'a> =
    Func<'a>(fun () -> (!r).Invoke ())
  member rf.Tie (r: ref<Func<'a>>, f: Func<'a>) : unit =
    r := f

type [<RecursionRules>] RecFunc1 () =
  member rf.Tier (_: Func<'a, 'b>) : ref<Func<'a, 'b>> =
    ref (Func<'a, 'b>(fun _ -> failwith "Tier"))
  member rf.Untie (r: ref<Func<'a, 'b>>) : Func<'a, 'b> =
    Func<'a, 'b>(fun x -> (!r).Invoke (x))
  member rf.Tie (r: ref<Func<'a, 'b>>, f: Func<'a, 'b>) : unit =
    r := f

type [<RecursionRules>] RecFunc2 () =
  member rf.Tier (_: Func<'a, 'b, 'c>) : ref<Func<'a, 'b, 'c>> =
    ref (Func<'a, 'b, 'c>(fun _ -> failwith "Tier"))
  member rf.Untie (r: ref<Func<'a, 'b, 'c>>) : Func<'a, 'b, 'c> =
    Func<'a, 'b, 'c>(fun x y -> (!r).Invoke (x, y))
  member rf.Tie (r: ref<Func<'a, 'b, 'c>>, f: Func<'a, 'b, 'c>) : unit =
    r := f
