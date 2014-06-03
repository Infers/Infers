// Inspired by
//   https://gist.github.com/t0yv0/649a10818119636c0952
//   http://fssnip.net/n1

module Main

////////////////////////////////////////////////////////////////////////////////
 
type SimpleRecord = { Name: string; Age: option<int> }
type LotsOfRecords = { People: SimpleRecord [] }
 
[<EntryPoint>]
let Start args =
  let sr = { People = [| {Name = "Rick"; Age = Some 33 }; { Name = "Paul"; Age = None } |] }
  let func = Infers.Engine.TryGenerate<CloneSmarter.Clone<LotsOfRecords>> (CloneSmarter.Clone ())
  match func with
   | Some clone ->
     let src = clone sr
     if (sr = src) && not (System.Object.ReferenceEquals(sr, src))
     then printfn "OK"
     else printfn "FAIL"
   | _ ->
     printfn "NO MATCH"
  0
