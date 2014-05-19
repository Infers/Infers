module Infers.Pickle

open Microsoft.FSharp.Core.Operators.Unchecked
open System
open System.IO
open Infers.Rep
open Infers.Util

type [<AbstractClass>] t<'a> () =
  abstract Pickle: BinaryWriter * 'a -> unit
  abstract Unpickle: BinaryReader -> 'a

type r<'a> () =
  inherit t<'a> ()
  [<DefaultValue>] val mutable impl: t<'a>
  override t.Pickle (w, x) = t.impl.Pickle (w, x)
  override t.Unpickle r = t.impl.Unpickle r

type [<AbstractClass>] p<'p, 'e, 'es> () =
  abstract Pickle: BinaryWriter * byref<'e> -> unit
  abstract Unpickle: BinaryReader * byref<'e> -> unit

let inline mk (unpickle: BinaryReader -> 'a)
              (pickle: BinaryWriter -> 'a -> unit) : t<'a> =
  {new t<'a> () with
    override t.Pickle (w, x) = pickle w x
    override t.Unpickle (r) = unpickle r}

type c<'u, 'cs, 'l, 'ls> = C of p<'u, 'l, 'ls>
type u<'u, 'c, 'cs> = U of list<t<'u>>

let inline mkConst x = mk (fun _ -> x) (fun _ _ -> ())

let inline mkSeq ofArray (e: t<'a>) =
  mk (fun r ->
        let n = r.ReadInt32 ()
        let xs = Array.zeroCreate n
        for i=0 to n-1 do
          xs.[i] <- e.Unpickle r
        ofArray xs)
     (fun w xs ->
        w.Write (Seq.length xs)
        xs |> Seq.iter (fun x -> e.Pickle (w, x)))

let inline mkElemOrField (t: t<'e>) =
  {new p<'t, 'e, 'p> () with
    override p.Pickle (w, e) = t.Pickle (w, e)
    override p.Unpickle (r, e) = e <- t.Unpickle (r)}

let inline mkProduct (fs: p<'r, 'fs, 'fs>) (f: p<'r, 'f, And<'f, 'fs>>) : p<'r, And<'f, 'fs>, And<'f, 'fs>> =
  {new p<'r, And<'f, 'fs>, And<'f, 'fs>> () with
    override p.Pickle (w, ffs) =
     f.Pickle (w, &ffs.Elem)
     fs.Pickle (w, &ffs.Rest)
    override p.Unpickle (r, ffs) =
     f.Unpickle (r, &ffs.Elem)
     fs.Unpickle (r, &ffs.Rest)}

let inline mkTupleOrNonRecursiveRecord (m: AsProduct<'t, 'p>) (p: p<'t, 'p, 'p>) =
  mk (fun r ->
        let mutable px = defaultof<_>
        p.Unpickle (r, &px)
        m.Create (&px))
     (fun w x ->
        let mutable px = defaultof<_>
        m.Extract (x, &px)
        p.Pickle (w, &px))

let inline mkUnion (m: Union<'u>) (U u: u<'u, 'c, 'c>) =
  let cs = Array.ofList u
  let inline mk readTag writeTag =
    mk (fun r ->
          let i = readTag r
          cs.[i].Unpickle r)
       (fun w x ->
          let i = m.Tag x
          writeTag w i
          cs.[i].Pickle (w, x))
  if m.Arity <= 256 then
    mk (fun r -> int (r.ReadByte ())) (fun w i -> w.Write (byte i))
  else
    mk (fun r -> r.ReadInt32 ()) (fun w i -> w.Write i)

type [<InferenceRules>] Pickle () =
  member e.unit: t<unit> = mkConst ()

  member e.rep = Rep ()

  member e.fix () : Rec<t<'x>> =
    let r = r<'x>()
    {new Rec<t<'x>> () with
      override p.Get () = r :> t<'x>
      override p.Set (t) = r.impl <- t}

  member e.bool: t<bool> = mk (fun r -> r.ReadBoolean ()) (fun w x -> w.Write x)

  member e.int8:  t<int8>  = mk (fun r -> r.ReadSByte ()) (fun w x -> w.Write x)
  member e.int16: t<int16> = mk (fun r -> r.ReadInt16 ()) (fun w x -> w.Write x)
  member e.int32: t<int32> = mk (fun r -> r.ReadInt32 ()) (fun w x -> w.Write x)
  member e.int64: t<int64> = mk (fun r -> r.ReadInt64 ()) (fun w x -> w.Write x)

  member e.uint8:  t<uint8>  = mk (fun r -> r.ReadByte   ()) (fun w x -> w.Write x)
  member e.uint16: t<uint16> = mk (fun r -> r.ReadUInt16 ()) (fun w x -> w.Write x)
  member e.uint32: t<uint32> = mk (fun r -> r.ReadUInt32 ()) (fun w x -> w.Write x)
  member e.uint64: t<uint64> = mk (fun r -> r.ReadUInt64 ()) (fun w x -> w.Write x)

  member e.float32: t<float32> = mk (fun r -> r.ReadSingle ()) (fun w x -> w.Write x)
  member e.float64: t<float> = mk (fun r -> r.ReadDouble ()) (fun w x -> w.Write x)

  member e.char: t<char> = mk (fun r -> r.ReadChar ()) (fun w x -> w.Write x)
  member e.string: t<string> = mk (fun r -> r.ReadString ()) (fun w x -> w.Write x)

  member e.list (t: t<'a>) : t<list<'a>> = mkSeq List.ofArray t
  member e.array (t: t<'a>) : t<array<'a>> = mkSeq id t

  member e.label (_: Label<'u, 'cs, 'l, 'ls>, t: t<'l>) : c<'u, 'cs, 'l, 'ls> =
    C (mkElemOrField t)

  member e.labels (C ls: c<'u, 'cs, 'ls, 'ls>, C l: c<'u, 'cs, 'l, And<'l, 'ls>>) : c<'u, 'cs, And<'l, 'ls>, And<'l, 'ls>> =
    C (mkProduct ls l)

  member e.case (m: Case<'u, Empty, 'cs>) : u<'u, Empty, 'cs> =
    U [mkConst (let mutable n = defaultof<_> in m.Create (&n))]
  member e.case (m: Case<'u, 'ls, 'cs>, C c: c<'u, 'cs, 'ls, 'ls>) : u<'u, 'ls, 'cs> =
    U [mkTupleOrNonRecursiveRecord m c]

  member e.choice (U cs: u<'u, 'cs, 'cs>, U c: u<'u, 'c, Choice<'c, 'cs>>) : u<'u, Choice<'c, 'cs>, Choice<'c, 'cs>> =
    U (c @ cs)

  member e.union (_: Rep, m: Union<'u>, _: AsChoice<'u, 'c>, u: u<'u, 'c, 'c>) : t<'u> =
    mkUnion m u

  member e.product (fs: p<'r, 'fs, 'fs>, f:  p<'r, 'f, And<'f, 'fs>>) : p<'r, And<'f, 'fs>, And<'f, 'fs>> =
    mkProduct fs f
    
  member e.elem (_: Elem<'t, 'e, 'p>, t: t<'e>) : p<'t, 'e, 'p> =
    mkElemOrField t
  member e.tuple (_: Rep, _: Tuple<'t>, m: AsProduct<'t, 'p>, p: p<'t, 'p, 'p>) : t<'t> =
    mkTupleOrNonRecursiveRecord m p

  member e.field (_: Field<'r, 'f, 'p>, t: t<'f>) : p<'r, 'f, 'p> =
    mkElemOrField t
  member e.record (_: Rep, _: Record<'r>, m: AsProduct<'r, 'p>, p: p<'r, 'p, 'p>) : t<'r> =
    mkTupleOrNonRecursiveRecord m p

let inline pu () : t<'a> =
  match StaticMap<Pickle, option<t<'a>>>.Get () with
   | None ->
     match Engine.TryGenerate (Pickle ()) with
      | None -> failwithf "Pickle: Unsupported type %A" typeof<'a>
      | Some pu ->
        StaticMap<Pickle, option<t<'a>>>.Set (Some pu)
        pu
   | Some pu ->
     pu

let pickle (w: BinaryWriter) (x: 'a) : unit = (pu ()).Pickle (w, x)
let unpickle (r: BinaryReader) : 'a = (pu ()).Unpickle (r)
