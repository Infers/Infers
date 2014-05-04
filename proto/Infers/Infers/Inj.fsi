namespace Infers

open System

/////////////////////////////////////////////////////////////////////////

type [<AbstractClass>] Inj<'d, 'c> =
  abstract Map: 'd -> 'c

/////////////////////////////////////////////////////////////////////////

type [<InferenceRules>] BaseRules =
  new: unit -> BaseRules

  member boolIn: Inj<bool, Choice<unit, unit>>
  member boolOut: Inj<Choice<unit, unit>, bool>

  member int8:  Inj<int8,  int64>
  member int16: Inj<int16, int64>
  member int32: Inj<int32, int64>

  member uint8 : Inj<uint8,  uint64>
  member uint16: Inj<uint16, uint64>
  member uint32: Inj<uint32, uint64>

  member   signed8:  Inj<uint8,   int8>
  member unsigned8:  Inj< int8,  uint8>
  member   signed16: Inj<uint16,  int16>
  member unsigned16: Inj< int16, uint16>
  member   signed32: Inj<uint32,  int32>
  member unsigned32: Inj< int32, uint32>
  member   signed64: Inj<uint64,  int64>
  member unsigned64: Inj< int64, uint64>

  member float: Inj<float32, float>

  member   curry: unit -> Inj<'a * 'b -> 'c, 'a -> 'b -> 'c>
  member uncurry: unit -> Inj<'a -> 'b -> 'c, 'a * 'b -> 'c>

  member ofFunc0: unit -> Inj<Func<'a>, unit -> 'a>
  member toFunc0: unit -> Inj<unit -> 'a, Func<'a>>

  member ofFunc1:  unit -> Inj<Func<'a, 'b>, 'a -> 'b>
  member toFunc1: unit -> Inj<'a -> 'b, Func<'a, 'b>>

  member   stageFunc: unit -> Inj<Func<'a, 'b, 'c>, Func<'a, Func<'b, 'c>>>
  member unstageFunc: unit -> Inj<Func<'a, Func<'b, 'c>>, Func<'a, 'b, 'c>>

  member ofAct: unit -> Inj<Action<'a>, 'a -> unit>
  member toAct: unit -> Inj<'a -> unit, Action<'a>>  

//  member   stageAct: unit -> Inj<Action<'a, 'b>, Func<'a, Action<'b>>>
//  member unstageAct: unit -> Inj<Func<'a, Action<'b>>, Action<'a, 'b>>
