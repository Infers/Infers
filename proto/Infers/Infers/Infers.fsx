#r "bin\\Debug\\Infers.dll" ;;

open System ;;
open Infers ;;

Eq.eq (1,1) (1,1) ;;

#if FALSE
let (>>=) xO x2yO = Option.bind x2yO xO

type [<InferenceRules>] Dummy () =
  member d.int = Some 0
  member d.option () = Some None
  member d.list () = Some []
  member d.pair (xO, yO) =
    xO >>= fun x ->
    yO >>= fun y ->
    Some (x, y)

(tryGenerate true [Dummy ()] : option<option<int * list<option<int>>>>)

type P<'T> = 'T -> unit

type [<InferenceRules>] Print () =
  member r.IntRule () : P<int> = fun i -> printf "%d" i
  member r.ListRule<'T> (s: P<'T>, self: P<list<'T>>) : P<list<'T>> =
    function
     | [] -> printf "\n"
     | x::xs ->
       s x ; self xs
  member r.DelayRule<'T> (f: unit -> P<'T>) : P<'T> = raise Backtrack

match (tryGenerate true [Print (); Rec.Rules ()] : option<P<list<int>>>) with
 | None -> printf "None\n"
 | Some p -> p [1;2;3]
#endif
