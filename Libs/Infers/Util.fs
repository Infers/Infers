// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module internal Infers.Util

open System

////////////////////////////////////////////////////////////////////////////////

type [<Sealed>] Fun =
  static member inline ofFunc (f: Func<'a, 'b, 'c>) x y = f.Invoke (x, y)
  static member inline toFunc (f: 'a -> 'b -> 'c) = Func<'a, 'b, 'c>(f)

////////////////////////////////////////////////////////////////////////////////

let inline (|>!) x effect = effect x ; x

let inline ( ^ ) x = x

////////////////////////////////////////////////////////////////////////////////

let inline force (x: Lazy<_>) = x.Force ()

let (|Force|) (xL: Lazy<_>) = xL.Force ()

////////////////////////////////////////////////////////////////////////////////

module List =
module Option =
  let inline collect x2ys = function
    | None -> Seq.empty
    | Some x -> x2ys x

////////////////////////////////////////////////////////////////////////////////

module Array =
  let chooseAll x2yO xs =
    let ys = Array.zeroCreate ^ Array.length xs
    let rec lp i =
      if xs.Length <= i then
        Some ys
      else
        match x2yO xs.[i] with
         | None -> None
         | Some y ->
           ys.[i] <- y
           lp (i+1)
    lp 0

////////////////////////////////////////////////////////////////////////////////

type HashEqSet<'k> when 'k: equality = HES of Map<int, list<'k>>

module HashEqSet =
  let empty = HES Map.empty

  let private drop k ks =
    if List.exists ((=) k) ks then
      List.filter ((<>) k) ks
    else
      ks

  let add k (HES s)  =
    let hk = hash k
    Map.add hk
     (k :: match Map.tryFind hk s with
            | None -> []
            | Some ks -> drop k ks)
     s |> HES

  let contains k (HES s) =
    match Map.tryFind (hash k) s with
     | None -> false
     | Some ks -> List.exists ((=) k) ks

  let toSeq (HES s) = Map.toSeq s |> Seq.collect snd

////////////////////////////////////////////////////////////////////////////////

type HashEqMap<'k, 'v> when 'k: equality = HEM of Map<int, list<'k * 'v>>

module HashEqMap =
  let private drop (k: 'k) (kvs: list<'k * 'v>) =
    if List.exists (fst >> (=) k) kvs then
      List.filter (fst >> (<>) k) kvs
    else
      kvs

  let empty = HEM Map.empty

  let add k v (HEM m) =
    let hk = hash k
    Map.add hk
     ((k, v)
       :: match Map.tryFind hk m with
           | None -> []
           | Some kvs -> drop k kvs)
     m |> HEM

  let tryFind k (HEM m) =
    Map.tryFind (hash k) m
    |> Option.bind (List.tryFind (fst >> (=) k)
                    >> Option.map ^ fun (_, v) -> v)

  let addOrUpdate key onAdd onUpdate map =
    add key
     (match tryFind key map with
       | None -> onAdd key
       | Some v -> onUpdate key v)
     map

  let map v2w (HEM m) =
    m
    |> Map.map ^ fun _ -> List.map ^ fun (k, v) -> (k, v2w v)
    |> HEM

  let isEmpty (HEM m) =
    Map.isEmpty m

  let count (HEM m) =
    Map.toSeq m |> Seq.map (snd >> List.length) |> Seq.sum

  let toSeq (HEM m) = Map.toSeq m |> Seq.collect snd

////////////////////////////////////////////////////////////////////////////////

module Dictionary =
  open System.Collections.Generic

  let inline getOr (d: Dictionary<_, _>) k u2v =
    let mutable v = Unchecked.defaultof<_>
    if not ^ d.TryGetValue (k, &v) then
      v <- u2v ()
      d.Add (k, v)
    v

////////////////////////////////////////////////////////////////////////////////

type IRecObj =
  abstract GetObj: unit -> obj
  abstract SetObj: obj -> unit

////////////////////////////////////////////////////////////////////////////////

module StaticMap =
  open Infers.Core
  open System.Threading
  let tryGetM = typeof<StaticMap>.GetMethod "TryGet"
  let getOrSetM = typeof<StaticMap>.GetMethod "GetOrSet"
  let boxM = typeof<StaticMap>.GetMethod "Box"
  let wait (box: Box<'v>) =
    lock box ^ fun () ->
      while not box.Ready do
        Monitor.Wait box |> ignore
    box.Value
  let inline getOrInvoke<'k, 'v> (mk: unit -> 'v) =
    match StaticMap<'k, 'v>.box with
     | null -> mk ()
     | box -> wait box
  let tryGetDyn kT vT =
    match tryGetM.MakeGenericMethod([|kT; vT|]).Invoke(null, null) with
     | :? Box as box -> box.Get () |> Some
     | _ -> None
  let getOrInvokeDyn kT vT mk =
    match tryGetM.MakeGenericMethod([|kT; vT|]).Invoke(null, null) with
     | :? Box as box -> box.Get ()
     | _ -> mk ()
  let getOrSetDyn kT vT v =
    let newB = boxM.MakeGenericMethod([|vT|]).Invoke(null, null)
    let box =
      lock newB <| fun () ->
      match getOrSetM.MakeGenericMethod([|kT; vT|]).Invoke(null, [|newB|]) with
       | null ->
         let newB = unbox<Box> newB
         newB.Set v
         newB
       | oldB -> unbox<Box> oldB
    box.Get ()
