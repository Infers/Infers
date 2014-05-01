namespace Infers

open System
open Infers.Engine

/////////////////////////////////////////////////////////////////////////

type [<AbstractClass>] Inj<'d, 'c> () =
  abstract Map: 'd -> 'c

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module internal BaseRulesHelpers =
  let inline inj (x2y: 'x -> 'y) =
    {new Inj<'x, 'y> () with member i.Map (x) = x2y x}

type [<InferenceRules>] BaseRules () =
  member br.boolIn: Inj<bool, Choice<unit, unit>> =
    inj (fun x -> if x then Choice1Of2 () else Choice2Of2 ())
  member br.boolOut: Inj<Choice<unit, unit>, bool> = 
    inj (function Choice1Of2 () -> true | Choice2Of2 () -> false)

  member br.int8:  Inj<int8,  int64> = inj int64
  member br.int16: Inj<int16, int64> = inj int64
  member br.int32: Inj<int32, int64> = inj int64

  member br.uint8:  Inj<uint8,  uint64> = inj uint64
  member br.uint16: Inj<uint16, uint64> = inj uint64
  member br.uint32: Inj<uint32, uint64> = inj uint64

  member   br.signed8:  Inj<uint8,   int8>  = inj  int8
  member br.unsigned8:  Inj< int8,  uint8>  = inj uint8
  member   br.signed16: Inj<uint16,  int16> = inj  int16
  member br.unsigned16: Inj< int16, uint16> = inj uint16
  member   br.signed32: Inj<uint32,  int32> = inj  int32
  member br.unsigned32: Inj< int32, uint32> = inj uint32
  member   br.signed64: Inj<uint64,  int64> = inj  int64
  member br.unsigned64: Inj< int64, uint64> = inj uint64

  member br.float: Inj<float32, float> = inj float

  member br.curry () : Inj<'a * 'b -> 'c, 'a -> 'b -> 'c> =
    inj (fun f -> fun x y -> f (x, y))
  member br.uncurry () : Inj<'a -> 'b -> 'c, 'a * 'b -> 'c> =
    inj (fun f -> fun (x, y) -> f x y)

  member br.ofFunc0 () : Inj<Func<'a>, unit -> 'a> =
    inj (fun f -> f.Invoke)
  member br.toFunc0 () : Inj<unit -> 'a, Func<'a>> =
    inj (fun f -> Func<'a>(f))

  member br.ofFunc1 () : Inj<Func<'a, 'b>, 'a -> 'b> =
    inj (fun f -> f.Invoke)
  member br.toFunc1 () : Inj<'a -> 'b, Func<'a, 'b>> =
    inj (fun f -> Func<'a, 'b>(f))

  member br.stageFunc () : Inj<Func<'a, 'b, 'c>, Func<'a, Func<'b, 'c>>> =
    inj (fun f -> Func<_, _>(fun x -> Func<_, _>(fun y -> f.Invoke (x, y))))
  member br.unstageFunc () : Inj<Func<'a, Func<'b, 'c>>, Func<'a, 'b, 'c>> =
    inj (fun f -> Func<_, _, _>(fun x y -> (f.Invoke x).Invoke y))

  member br.ofAct () : Inj<Action<'a>, 'a -> unit> =
    inj (fun f -> f.Invoke)
  member br.toAct () : Inj<'a -> unit, Action<'a>> =
    inj (fun f -> Action<_>(f))
