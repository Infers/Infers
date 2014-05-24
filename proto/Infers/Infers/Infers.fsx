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
