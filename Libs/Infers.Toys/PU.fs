// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module PU =
  open System.Collections.Generic
  open System.IO
  open Infers
  open Infers.Rep

  type State = Writing | Cyclic | Acyclic
  type Info = {Pos: int64; mutable State: State}
  type [<AbstractClass>] PUI<'t> () =
    abstract P: Dictionary<obj, Info> * BinaryWriter * byref<'t> -> unit
    abstract U: Dictionary<int64, obj> * BinaryReader * byref<'t> -> unit
  type PUR<'t> () =
    inherit PUI<'t> ()
    [<DefaultValue>] val mutable Rec: PUI<'t>
    override t.P (d, w, x) = t.Rec.P (d, w, &x)
    override t.U (d, r, x) = t.Rec.U (d, r, &x)
  type PU<'t> = O of PUI<'t>
  let inline outO (O x) = x
  type PUP<'e, 'r, 'o, 't> = P of PUI<'e>
  type PUS<'p, 'o, 't> = S of list<PUI<'t>>

  let [<Literal>] InDict = 0uy
  let [<Literal>] InHere = 1uy
  let [<Literal>] Cycle = 2uy

  let inline prim pR pW =
    {new PUI<_> () with
      member pu.P (_, w, t) = pW w t
      member pu.U (_, r, t) = t <- pR r}

  let inline asPairs (asP: AsPairs<'p, 'o, 't>) (P pPU: PUP<'p, 'p, 'o, 't>) =
    {new PUI<'t> () with
      member pu.P (d, w, t) =
        let mutable p = asP.ToPairs t
        pPU.P (d, w, &p)
      member pu.U (d, r, t) =
        let mutable p = Unchecked.defaultof<_>
        pPU.U (d, r, &p)
        t <- asP.Create (&p)}

  type [<Bitwise; Rep>] PU () =
    inherit Rules ()

    static member Rec () =
      let r = PUR<'t> ()
      {new Rec<PU<'t>> () with
        override pu.Get () = O r
        override pu.Set (O tPU) = r.Rec <- tPU}

    static member Unit = O <| prim (fun _ -> ()) (fun _ _ -> ())

    static member UInt8 = O <| prim (fun r -> r.ReadByte ()) (fun w -> w.Write)
    static member Int16 = O <| prim (fun r -> r.ReadInt16 ()) (fun w -> w.Write)
    static member Int32 = O <| prim (fun r -> r.ReadInt32 ()) (fun w -> w.Write)
    static member Int64 = O <| prim (fun r -> r.ReadInt64 ()) (fun w -> w.Write)

    static member Bitwise (bIt: Bitwise<'b, 't>, O bPU: PU<'b>) : PU<'t> =
      O {new PUI<'t> () with
          member pu.P (d, w, t) =
            let mutable b = bIt.ToBits t
            bPU.P (d, w, &b)
          member pu.U (d, r, t) =
            let mutable b = Unchecked.defaultof<_>
            bPU.U (d, r, &b)
            t <- bIt.OfBits b}

    static member String =
      O {new PUI<string> () with
          member pu.P (d, w, t) = w.Write t
          member pu.U (d, r, t) = t <- r.ReadString ()}

    static member Elem (_: Elem<'e, 'r, 'o, 't>, O ePU: PU<'e>) =
      P ePU : PUP<'e, 'r, 'o, 't>

    static member Pair (P ePU: PUP<     'e     , Pair<'e, 'r>, 'o, 't>,
                        P rPU: PUP<         'r ,          'r , 'o, 't>)
                             : PUP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
      P {new PUI<Pair<'e, 'r>> () with
          member pu.P (d, w, t) = ePU.P (d, w, &t.Elem); rPU.P (d, w, &t.Rest)
          member pu.U (d, r, t) = ePU.U (d, r, &t.Elem); rPU.U (d, r, &t.Rest)}

    static member Tuple (_: Tuple<'t>,
                         asP: AsPairs<'p, 'o, 't>,
                         pPU: PUP<'p, 'p, 'o, 't>) = O <| asPairs asP pPU

    static member Record (tR: Record<'t>,
                          asP: AsPairs<'p, 'o, 't>,
                          P pPU: PUP<'p, 'p, 'o, 't>) =
      O {new PUI<'t> () with
          member pu.P (d, w, t) =
            let mutable info = Unchecked.defaultof<_>
            if d.TryGetValue (t, &info)
            then w.Write InDict
                 w.Write info.Pos
                 match info.State with
                  | Writing -> info.State <- Cyclic
                  | Cyclic | Acyclic -> ()
            else w.Write InHere
                 info <- {Pos = w.BaseStream.Position; State = Writing}
                 d.Add (t, info)
                 let mutable p = asP.ToPairs t
                 pPU.P (d, w, &p)
                 match info.State with
                  | Acyclic | Writing -> info.State <- Acyclic
                  | Cyclic ->
                    let pos = w.BaseStream.Position
                    w.BaseStream.Seek (info.Pos-1L, SeekOrigin.Begin) |> ignore
                    w.Write Cycle
                    w.BaseStream.Seek (pos, SeekOrigin.Begin) |> ignore
          member pu.U (d, r, t) =
            match r.ReadByte () with
             | InDict -> t <- unbox d.[r.ReadInt64 ()]
             | InHere -> let pos = r.BaseStream.Position
                         let mutable p = Unchecked.defaultof<_>
                         pPU.U (d, r, &p)
                         t <- asP.Create (&p)
                         d.Add (pos, t)
             | _ -> let pos = r.BaseStream.Position
                    t <- asP.Default tR
                    d.Add (pos, t)
                    let mutable p = Unchecked.defaultof<_>
                    pPU.U (d, r, &p)
                    asP.Overwrite (tR, t, &p)}

    static member Case (c: Case<Empty, 'o, 't>) : PUS<Empty, 'o, 't> =
      S [prim (fun _ -> c.OfPairs Unchecked.defaultof<_>) (fun _ _ -> ())]

    static member Case (c: Case<'p, 'o, 't>, pPU: PUP<'p, 'p, 'o, 't>) =
      S [asPairs c pPU] : PUS<'p, 'o, 't>

    static member Choice (S pPU: PUS<       'p     , Choice<'p, 'o>, 't>,
                          S oPU: PUS<           'o ,            'o , 't>) =
      S <| pPU @ oPU           : PUS<Choice<'p, 'o>, Choice<'p, 'o>, 't>

    static member Sum (asC: AsChoices<'s,'t>, S sPU: PUS<'s,'s,'t>) : PU<'t> =
      let sPU = Array.ofList sPU
      let inline mkPU tW tR =
        O {new PUI<'t> () with
            member pu.P (d, w, t) =
              let i = asC.Tag t in tW w i ; sPU.[i].P (d, w, &t)
            member pu.U (d, r, t) = sPU.[tR r].U (d, r, &t)}
      if sPU.Length <= 256
      then mkPU (fun w -> uint8 >> w.Write) (fun r -> r.ReadByte () |> int)
      else mkPU (fun w t -> w.Write t) ( fun r -> r.ReadInt32 ())

  let inline pu<'t> = Engine.generateDFS<PU, PU<'t>> |> outO

  let pickle x =
    use s = new MemoryStream ()
    use w = new BinaryWriter (s)
    let mutable x = x
    pu<'t>.P (Dictionary (physicalComparer), w, &x)
    s.ToArray ()

  let unpickle bytes =
    use r = new BinaryReader (new MemoryStream (bytes, false))
    let mutable x = Unchecked.defaultof<_>
    pu<'t>.U (Dictionary (), r, &x)
    x
