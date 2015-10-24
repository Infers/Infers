// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

// This is a toy example of a binary pickler / unpickler.  This can handle ints,
// floats, strings, tuples, records, and union types.  Recursive types, such as
// lists, and recursive values, via records, are supported.  Other types,
// including arbitrary classes or structs, are not supported.
//
// This could be improved in various ways.  Examples:
//
// - Pickles do not contain any error checking information.  It would be
// straighforward to add, for example, a hash of the type structure to the
// beginning of the pickle and verify it when unpickling to help to detect type
// errors.
//
// - Inefficient, but concise, pattern matching forms are used to manipulate
// nested pairs.  Using byref arguments copying could be minimized.
//
// - Support for various special types such as arrays and refs is not
// implemented.  Such support could be added in a straightforward manner.
//
// - 32-bit tags are used for union types even when a byte would suffice.  It
// would be easy to optimize the representation.
//
// - Lists are pickled via naive recursive encoding.  Lists could be implemented
// via (not yet implemented) array support.
//
// Perhaps the main point here is that it doesn't really take all that much code
// to implement a fairly powerful pickler.

module PU =
  open System.Collections.Generic
  open System.IO
  open Infers
  open Infers.Rep

  type State = Writing | Cyclic | Acyclic
  type Info = {Pos: int64; mutable State: State}
  type PU<'x> = {P: Dictionary<obj, Info> -> BinaryWriter -> 'x -> unit
                 U: Dictionary<int64, obj> -> BinaryReader -> 'x}
  type PUP<'e, 'r, 'o, 't> = P of PU<'e>
  type PUS<'p, 'o, 't> = S of list<PU<'t>>

  type [<Rep>] PU () =
    inherit Rules ()

    static member Rec () =
      let p = ref (fun _ -> failwith "Rec")
      let u = ref (fun _ -> failwith "Rec")
      {new Rec<PU<'t>> () with
        override t.Get () = {P = fun d -> !p d
                             U = fun d -> !u d}
        override t.Set tPU = p := tPU.P ; u := tPU.U}

    static member Unit = {U = fun _ _ -> ()
                          P = fun _ _ () -> ()}

    static member Int = {U = fun _ r -> r.ReadInt32 ()
                         P = fun _ w -> w.Write}
    static member Float = {U = fun _ r -> r.ReadDouble ()
                           P = fun _ w -> w.Write}
    static member String = {U = fun _ r -> r.ReadString ()
                            P = fun _ w -> w.Write}

    static member Elem (_: Elem<'e, 'r, 'o, 't>, ePU: PU<'e>) =
      P ePU : PUP<'e, 'r, 'o, 't>

    static member Pair (P ePU: PUP<     'e     , Pair<'e, 'r>, 'o, 't>,
                        P rPU: PUP<         'r ,          'r , 'o, 't>)
                             : PUP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
      P {P = fun d w (Pair (e, r)) -> ePU.P d w e; rPU.P d w r
         U = fun d r -> Pair (ePU.U d r, rPU.U d r)}

    static member Tuple (_: Tuple<'t>,
                         asP: AsPairs<'p, 'o, 't>,
                         P pPU: PUP<'p, 'p, 'o, 't>) =
      {P = fun d w -> asP.ToPairs >> pPU.P d w
       U = fun d -> pPU.U d >> asP.OfPairs}

    static member Record (tR: Record<'t>,
                          asP: AsPairs<'p, 'o, 't>,
                          P pPU: PUP<'p, 'p, 'o, 't>) =
      {P = fun d w t ->
        let mutable info = Unchecked.defaultof<_>
        if d.TryGetValue (t, &info)
        then w.Write 0uy
             w.Write info.Pos
             match info.State with
              | Writing -> info.State <- Cyclic
              | Cyclic | Acyclic -> ()
        else w.Write 1uy
             info <- {Pos = w.BaseStream.Position; State = Writing}
             d.Add (t, info)
             asP.ToPairs t |> pPU.P d w
             match info.State with
              | Acyclic | Writing -> info.State <- Acyclic
              | Cyclic ->
                let pos = w.BaseStream.Position
                w.BaseStream.Seek (info.Pos-1L, SeekOrigin.Begin) |> ignore
                w.Write 2uy
                w.BaseStream.Seek (pos, SeekOrigin.Begin) |> ignore
       U = fun d r ->
        match r.ReadByte () with
         | 0uy -> unbox d.[r.ReadInt64 ()]
         | 1uy -> let pos = r.BaseStream.Position
                  let o = pPU.U d r |> asP.OfPairs
                  d.Add (pos, o)
                  o
         | _   -> let pos = r.BaseStream.Position
                  let o = asP.Default tR
                  d.Add (pos, o)
                  let mutable p = pPU.U d r
                  asP.Overwrite (tR, o, &p)
                  o}

    static member Case (c: Case<Empty, 'o, 't>) : PUS<Empty, 'o, 't> =
      S [{P = fun _ _ _ -> ()
          U = fun _ _ -> c.OfPairs Unchecked.defaultof<_>}]

    static member Case (c: Case<'p, 'o, 't>, P pPU: PUP<'p, 'p, 'o, 't>) =
      S [{P = fun d w -> c.ToPairs >> pPU.P d w
          U = fun d -> pPU.U d >> c.OfPairs}] : PUS<'p, 'o, 't>

    static member Choice (S pPU: PUS<       'p     , Choice<'p, 'o>, 't>,
                          S oPU: PUS<           'o ,            'o , 't>) =
      S <| pPU @ oPU           : PUS<Choice<'p, 'o>, Choice<'p, 'o>, 't>

    static member Sum (asC: AsChoices<'s,'t>, S sPU: PUS<'s,'s,'t>) : PU<'t> =
      let sPU = Array.ofList sPU
      {P = fun d w t -> let i = asC.Tag t in w.Write i ; sPU.[i].P d w t
       U = fun d r -> sPU.[r.ReadInt32 ()].U d r}

  let pickle x =
    use s = new MemoryStream ()
    use w = new BinaryWriter (s)
    Engine.generateDFS<PU, PU<_>>.P (Dictionary (physicalComparer)) w x
    s.ToArray ()

  let unpickle bytes =
    use r = new BinaryReader (new MemoryStream (bytes, false))
    Engine.generateDFS<PU, PU<_>>.U (Dictionary ()) r
