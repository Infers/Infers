#r "bin\\Debug\\Infers.dll" ;;

open System.IO ;;
open System ;;
open Infers ;;

let testPU (x: 'x) : 'x =
  let s = new MemoryStream ()
  let w = new BinaryWriter (s)
  Pickle.pickle w x
  s.Position <- 0L
  let r = new BinaryReader (s)
  Pickle.unpickle r : 'x

type [<InferenceRules>] TestSpecificFirst () =
  member this.aba () : string * list<'a * 'b * 'a> = ("aba", [])
  member this.abc () : string * list<'a * 'b * 'c> = ("abc", [])
  member this.aaa () : string * list<'a * 'a * 'a> = ("aaa", [])
  member this.abb () : string * list<'a * 'b * 'b> = ("abb", [])
  member this.aab () : string * list<'a * 'a * 'b> = ("aab", [])

let testSpecific (x: 'x) =
  Engine.TryGenerate (true, 10, 10, [TestSpecificFirst ()]) : option<string * list<'x>>
