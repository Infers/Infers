module Infers.Eq

open Microsoft.FSharp.Core.LanguagePrimitives
open System
open Infers.Util
open Infers.Rep

type t<'a> = Func<'a, 'a, bool>
type u<'c, 'cs, 'u> = U of list<option<t<'u>>>
type p<'e, 'es, 'p> = P of t<'p>

let inline via map eq x y = eq (map x) (map y)

type [<InferenceRules>] Eq () =
  member e.fix (r: RecFn) : Rec<t<'x>> = r.func2 ()

  member e.unit: t<unit> = Fun.toFunc (fun () () -> true)

  member e.bool: t<bool> = Fun.toFunc (=)

  member e.int8: t<int8> = Fun.toFunc (=)
  member e.int16: t<int16> = Fun.toFunc (=)
  member e.int32: t<int32> = Fun.toFunc (=)
  member e.int64: t<int64> = Fun.toFunc (=)

  member e.uint8: t<uint8> = Fun.toFunc (=)
  member e.uint16: t<uint16> = Fun.toFunc (=)
  member e.uint32: t<uint32> = Fun.toFunc (=)
  member e.uint64: t<uint64> = Fun.toFunc (=)

  member e.float32: t<float32> =
    Fun.toFunc <| fun x y ->
    via (fun (x: float32) ->
            BitConverter.ToInt32 (BitConverter.GetBytes x, 0))
        (=) x y
  member e.float64: t<float> =
    Fun.toFunc (fun x y -> via BitConverter.DoubleToInt64Bits (=) x y)

  member e.char: t<char> = Fun.toFunc (=)
  member e.string: t<string> = Fun.toFunc (=)

  member e.ref () : t<ref<'a>> = Fun.toFunc PhysicalEquality
  member e.array () : t<array<'a>> = Fun.toFunc PhysicalEquality

  member e.case (_: Case<Empty, 'cs, 'u>) : u<Empty, 'cs, 'u> =
    U [None]
  member t.case (_: Case<'ls, 'cs, 'u>, P ls: p<'ls, 'ls, 'u>) : u<'ls, 'cs, 'u> =
    U [Some ls]

  member e.plus (U c: u<'c, Choice<'c, 'cs>, 'u>, U cs: u<'cs, 'cs, 'u>) : u<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u> =
    U (c @ cs)

  member e.union (_: Rep, m: Union<'u>, _: AsChoice<'c, 'u>, U u: u<'c, 'c, 'u>) : t<'u> =
    let u = Array.ofList u
    Fun.toFunc <| fun l r ->
      let i = m.Tag l
      let j = m.Tag r
      i = j &&
      match u.[i] with
       | None -> true
       | Some f -> Fun.ofFunc f l r

  member e.elem (m: Elem<'e, 'p, 't>, t: t<'e>) : p<'e, 'p, 't> =
    P (Fun.toFunc (fun x y -> via m.Get (Fun.ofFunc t) x y))

  member e.times (P f: p<'f, And<'f, 'fs>, 'r>, P fs: p<'fs, 'fs, 'r>) : p<And<'f, 'fs>, And<'f, 'fs>, 'r> =
    P (Fun.toFunc (fun x y -> Fun.ofFunc f x y && Fun.ofFunc fs x y))

  member e.product (_: Rep, m: Rep.Product<'t>, _: AsProduct<'p, 't>, P p: p<'p, 'p, 't>) : t<'t> =
    if m.IsMutable then Fun.toFunc PhysicalEquality else p

let inline mk () : t<'a> =
  match StaticMap<Eq, t<'a>>.Get () with
   | null ->
     match Engine.TryGenerate (Eq ()) with
       | None -> failwithf "Eq: Unsupported type: %A" typeof<'a>
       | Some eq ->
         StaticMap<Eq, t<'a>>.Set eq
         eq
   | eq ->
     eq

let eq x y = (mk ()).Invoke (x, y)
