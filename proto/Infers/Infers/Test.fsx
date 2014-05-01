let (>>=) xO x2yO = Option.bind x2yO xO

type [<InferenceRules>] Dummy () =
  member d.int = Some 0
  member d.option () = Some None
  member d.list () = Some []
  member d.pair (xO, yO) =
    xO >>= fun x ->
    yO >>= fun y ->
    Some (x, y)

(tryGenerate [Dummy ()] (RecFun ()) : option<option<int * list<option<int>>>>)
