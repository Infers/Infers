// Copyright (C) by Vesa Karvonen

module Toys.PU

open System.Collections.Generic
open System.IO
open Infers
open Infers.Rep
open Toys.Basic
open Toys.Rec

type State = Writing | Cyclic | Acyclic
type Info = {Pos: int64; mutable State: State}
type PU<'x> =
  {Pickle: Dictionary<obj, Info> -> BinaryWriter -> 'x -> unit
   Unpickle: Dictionary<int64, obj> -> BinaryReader -> 'x}
type PUE<'x> = {PU: PU<'x>}
type PUP<'e, 'r, 'o, 't> = P of PU<'e>
type PUS<'p, 'o, 't> = S of list<PU<'t>>

type [<InferenceRules>] PU () =
  member t.Entry (_: Rep, _: Basic, _: Rec, xP) = {PU = xP}
  member t.Int : PU<int> =
    {Pickle = fun _ w -> w.Write
     Unpickle = fun _ r -> r.ReadInt32 ()}
  member t.Float : PU<float> =
    {Pickle = fun _ w -> w.Write
     Unpickle = fun _ r -> r.ReadDouble ()}
  member t.String : PU<string> =
    {Pickle = fun _ w -> w.Write
     Unpickle = fun _ r -> r.ReadString ()}
  member t.Elem (_: Elem<'e, 'r, 'o, 't>, eP: PU<'e>) : PUP<'e, 'r, 'o, 't> =
    P eP
  member t.Pair (P eP: PUP<     'e     , Pair<'e, 'r>, 'o, 't>,
                 P rP: PUP<         'r ,          'r , 'o, 't>)
                     : PUP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
    P {Pickle = fun d w (Pair (e, r)) -> eP.Pickle d w e; rP.Pickle d w r
       Unpickle = fun d r -> Pair (eP.Unpickle d r, rP.Unpickle d r)}
  member t.Tuple (_: Tuple<'t>,
                  asPairs: AsPairs<'p, 'o, 't>,
                  P pP: PUP<'p, 'p, 'o, 't>) : PU<'t> =
    {Pickle = fun d w -> asPairs.ToPairs >> pP.Pickle d w
     Unpickle = fun d -> pP.Unpickle d >> asPairs.OfPairs}
  member t.Record (record: Record<'t>,
                   asPairs: AsPairs<'p, 'o, 't>,
                   P pP: PUP<'p, 'p, 'o, 't>) : PU<'t> =
    {Pickle = fun d w t ->
      match d.TryGetValue t with
       | (false, _) ->
         w.Write 1uy
         let info = {Pos = w.BaseStream.Position; State = Writing}
         d.Add (t, info)
         pP.Pickle d w (asPairs.ToPairs t)
         match info.State with
          | Acyclic -> failwith "Bug"
          | Writing -> info.State <- Acyclic
          | Cyclic ->
            let pos = w.BaseStream.Position
            w.BaseStream.Seek (info.Pos-1L, SeekOrigin.Begin) |> ignore
            w.Write 2uy
            w.BaseStream.Seek (pos, SeekOrigin.Begin) |> ignore
       | (true, info) ->
         w.Write 0uy
         w.Write info.Pos
         match info.State with
          | Writing -> info.State <- Cyclic
          | Cyclic | Acyclic -> ()
     Unpickle = fun d r ->
      match r.ReadByte () with
       | 0uy -> let pos = r.ReadInt64 () in unbox d.[pos]
       | 1uy -> let pos = r.BaseStream.Position 
                let o = pP.Unpickle d r |> asPairs.OfPairs
                d.Add (pos, o)
                o
       | _   -> let pos = r.BaseStream.Position
                let o = asPairs.Default record
                d.Add (pos, o)
                let mutable p = pP.Unpickle d r
                asPairs.Overwrite (record, o, &p)
                o}
  member t.Case (case: Case<Empty, 'o, 't>) : PUS<Empty, 'o, 't> =
    S [{Pickle = fun _ _ _ -> ()
        Unpickle = fun _ _ -> case.OfPairs Unchecked.defaultof<_>}]
  member t.Case (case: Case<'p, 'o, 't>, P pP: PUP<'p, 'p, 'o, 't>) : PUS<'p, 'o, 't> =
    S [{Pickle = fun d w -> case.ToPairs >> pP.Pickle d w
        Unpickle = fun d -> pP.Unpickle d >> case.OfPairs}]
  member t.Choice (S pP: PUS<       'p     , Choice<'p, 'o>, 't>,
                   S oP: PUS<           'o ,            'o , 't>) =
    S (pP @ oP)        : PUS<Choice<'p, 'o>, Choice<'p, 'o>, 't>
  member t.Sum (asChoices: AsChoices<'s, 't>, S sP: PUS<'s, 's, 't>) : PU<'t> =
    let sP = Array.ofList sP
    {Pickle = fun d w t ->
       let tag = asChoices.Tag t
       w.Write tag
       sP.[tag].Pickle d w t
     Unpickle = fun d r -> sP.[r.ReadInt32 ()].Unpickle d r}

let pickle (x: 'x) =
  let c = {new IEqualityComparer<obj> with
     member t.GetHashCode (x) = LanguagePrimitives.PhysicalHash x
     member t.Equals (l, r) = LanguagePrimitives.PhysicalEquality l r}
  let d = Dictionary<_, _> (c)
  use s = new MemoryStream ()
  use w = new BinaryWriter (s)
  StaticRules<PU>.Generate().PU.Pickle d w x
  s.ToArray ()

let unpickle (bs: array<byte>) : 'x =
  let d = Dictionary<_, _> ()
  use s = new MemoryStream (bs)
  s.Seek (0L, SeekOrigin.Begin) |> ignore
  use r = new BinaryReader (s)
  StaticRules<PU>.Generate().PU.Unpickle d r
