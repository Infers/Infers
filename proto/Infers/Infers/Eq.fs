module Infers.Eq

open Microsoft.FSharp.Core.LanguagePrimitives
open System
open Infers.Util
open Infers.Rep

type t<'a> = Func<'a, 'a, bool>
type c<'u, 'cs, 'l, 'ls> = C of t<'u>
type u<'u, 'c, 'cs> = U of list<option<t<'u>>>
type p<'p, 'e, 'es> = P of t<'p>

let inline via map eq x y = eq (map x) (map y)

type [<InferenceRules>] Rules () =
  
  member e.unit: t<unit> = toFunc (fun () () -> true)

  member e.bool: t<bool> = toFunc (=)

  member e.int8: t<int8> = toFunc (=)
  member e.int16: t<int16> = toFunc (=)
  member e.int32: t<int32> = toFunc (=)
  member e.int64: t<int64> = toFunc (=)

  member e.uint8: t<uint8> = toFunc (=)
  member e.uint16: t<uint16> = toFunc (=)
  member e.uint32: t<uint32> = toFunc (=)
  member e.uint64: t<uint64> = toFunc (=)

  member e.float32: t<float32> =
    toFunc <| fun x y ->
    via (fun (x: float32) ->
            BitConverter.ToInt32 (BitConverter.GetBytes x, 0))
        (=) x y
  member e.float64: t<float> =
    toFunc (fun x y -> via BitConverter.DoubleToInt64Bits (=) x y)

  member e.char: t<char> = toFunc (=)
  member e.string: t<string> = toFunc (=)

  member e.ref () : t<ref<'a>> = toFunc PhysicalEquality
  member e.array () : t<array<'a>> = toFunc PhysicalEquality

  member e.labels (C ls: c<'u, 'cs, 'ls, 'ls>, C l: c<'u, 'cs, 'l, And<'l, 'ls>>) : c<'u, 'cs, And<'l, 'ls>, And<'l, 'ls>> =
    C (toFunc (fun x y -> toFun l x y && toFun ls x y))

  member e.label (m: Label<'u, 'cs, 'l, 'ls>, t: t<'l>) : c<'u, 'cs, 'l, 'ls> =
    C (toFunc (fun x y -> via m.Get (toFun t) x y))

  member e.case (_: Case<'u, Empty, 'cs>) : u<'u, Empty, 'cs> =
    U [None]
  member t.case (_: Case<'u, 'ls, 'cs>, C ls: c<'u, 'cs, 'ls, 'ls>) : u<'u, 'ls, 'cs> =
    U [Some ls]

  member e.choice (U cs: u<'u, 'cs, 'cs>, U c: u<'u, 'c, Choice<'c, 'cs>>) : u<'u, Choice<'c, 'cs>, Choice<'c, 'cs>> =
    U (c @ cs)

  member e.union (m: Union<'u>, _: AsChoice<'u, 'c>, U u: u<'u, 'c, 'c>) : t<'u> =
    let u = Array.ofList u
    toFunc <| fun l r ->
      let i = m.Tag l
      let j = m.Tag r
      i = j &&
      match u.[i] with
       | None -> true
       | Some f -> toFun f l r

  member e.product (P fs: p<'r, 'fs, 'fs>, P f: p<'r, 'f, And<'f, 'fs>>) : p<'r, And<'f, 'fs>, And<'f, 'fs>> =
    P (toFunc (fun x y -> toFun f x y && toFun fs x y))

  member e.elem (m: Elem<'t, 'e, 'p>, t: t<'e>) : p<'t, 'e, 'p> =
    P (toFunc (fun x y -> via m.Get (toFun t) x y))
  member e.tuple (_: Rep.Tuple<'t>, _: AsProduct<'t, 'p>, P p: p<'t, 'p, 'p>) : t<'t> =
    p

  member e.field (m: Field<'r, 'f, 'p>, t: t<'f>) : p<'r, 'f, 'p> =
    P (toFunc (via m.Get (toFun t)))
  member e.record (m: Record<'r>, _: AsProduct<'r, 'p>, P p: p<'r, 'p, 'p>) : t<'r> =
    if m.IsMutable then toFunc PhysicalEquality else p

let inline mk () : t<'a> =
  match StaticMap<Rules, t<'a>>.Get () with
   | null ->
     match Engine.TryGenerate
            [Rules () :> obj; Rep.Rules () :> obj; Rec.Rules () :> obj] with
       | None -> failwithf "Eq: Unsupported type: %A" typeof<'a>
       | Some eq ->
         StaticMap<Rules, t<'a>>.Set eq
         eq
   | eq ->
     eq

let eq x y = (mk ()).Invoke (x, y)
