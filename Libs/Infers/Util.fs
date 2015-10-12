// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module internal Infers.Util

open System

////////////////////////////////////////////////////////////////////////////////

/// Operations for converting between F# functions and .Net `Func<...>`
/// delegates.
type [<Sealed>] Fun =
  static member inline ofFunc (f: Func<'a, 'b, 'c>) x y = f.Invoke (x, y)
  static member inline toFunc (f: 'a -> 'b -> 'c) = Func<'a, 'b, 'c>(f)

////////////////////////////////////////////////////////////////////////////////

/// The constant function.
let inline constant x _ = x

////////////////////////////////////////////////////////////////////////////////

/// `x |>! ef` is equivalent to `x |> fun x -> ef x ; x` and basically allows
/// an imperative operation to be used in a pipe.
let inline (|>!) x effect = effect x ; x

////////////////////////////////////////////////////////////////////////////////

/// Forces a lazy computation object.
let inline force (x: Lazy<_>) = x.Force ()

let (|Force|) (xL: Lazy<_>) = xL.Force ()

////////////////////////////////////////////////////////////////////////////////

/// Additional operations on options.
module Option =
  let inline collect x2ys = function
    | None -> Seq.empty
    | Some x -> x2ys x

////////////////////////////////////////////////////////////////////////////////

module Array =
  let chooseAll x2yO xs =
    let ys = Array.zeroCreate <| Array.length xs
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

/// Represents a functional set of type `'k` requiring only that `'k` provides
/// equality and hash operations.
type HashEqSet<'k> when 'k: equality = HES of Map<int, list<'k>>

/// Operations on functional sets using hash and equality.
module HashEqSet =
  /// The empty set.
  let empty = HES Map.empty

  let private drop k ks =
    if List.exists ((=) k) ks then
      List.filter ((<>) k) ks
    else
      ks

  /// Creates a new set containing the keys of the given set and the given key.
  let add k (HES s)  =
    let hk = hash k
    Map.add hk
     (match Map.tryFind hk s with
       | None -> [k]
       | Some ks -> k :: drop k ks)
     s |> HES

  /// Tests whether the key is in the set.
  let contains k (HES s) =
    match Map.tryFind (hash k) s with
     | None -> false
     | Some ks -> List.exists ((=) k) ks

  /// Returns a sequence of the set.
  let toSeq (HES s) = Map.toSeq s |> Seq.collect snd

////////////////////////////////////////////////////////////////////////////////

/// Represents a functional map of type `'k -> 'v` requiring only that `'k`
/// provides equality and hash operations.
type HashEqMap<'k, 'v> when 'k: equality =
  | HEM of Map<int, list<'k * 'v>>
  override this.ToString () =
    match this with
     | HEM m ->
       m
       |> Map.toSeq
       |> Seq.map snd
       |> Seq.concat
       |> List.ofSeq
       |> sprintf "%A"

/// Operations on functional maps using hash and equality.
module HashEqMap =
  let private drop (k: 'k) (kvs: list<'k * 'v>) =
    if List.exists (fst >> (=) k) kvs then
      List.filter (fst >> (<>) k) kvs
    else
      kvs

  /// The empty map.
  let empty = HEM Map.empty

  /// Creates a new map containing the key value pairs in the given map and the
  /// given key value pair.
  let add k v (HEM m) =
    let hk = hash k
    Map.add hk
     ((k, v)
       :: match Map.tryFind hk m with
           | None -> []
           | Some kvs -> drop k kvs)
     m |> HEM

  /// Tries to find the value associated with the given key from the given map.
  let tryFind k (HEM m) =
    Map.tryFind (hash k) m
    |> Option.bind (fun kvs ->
       List.tryFind (fst >> (=) k) kvs
       |> Option.map (fun (_, v) ->
          v))

  /// Creates a new map containing the key value pairs in the given map and the
  /// key value pair produced either by `onAdd`, in case the given map doesn't
  /// contain the given key, or by `onUpdate`, in case the given key already
  /// existed in the given map.
  let addOrUpdate key onAdd onUpdate map =
    add key
     (match tryFind key map with
       | None -> onAdd key
       | Some v -> onUpdate key v)
     map

  /// Transforms all values in the map with the given function producing a new
  /// map.
  let map v2w (HEM m) =
    HEM (Map.map (fun _ kvs -> List.map (fun (k, v) -> (k, v2w v)) kvs) m)

  /// Determines whether the given map is empty.
  let isEmpty (HEM m) =
    Map.isEmpty m

  /// Counts the number of key value pairs in the map.
  let count (HEM m) =
    Map.toSeq m |> Seq.map (snd >> List.length) |> Seq.sum

  /// Returns an association sequence corresponding to the given map.
  let toSeq (HEM m) = Map.toSeq m |> Seq.collect snd

////////////////////////////////////////////////////////////////////////////////

/// Additional `BindingFlags` combinations.
module BindingFlags =
  open System.Reflection

  /// Both public and non-public members.
  let [<Literal>] Any = BindingFlags.Public ||| BindingFlags.NonPublic
  /// Only declared and instance members.
  let [<Literal>] DeclaredInstance = BindingFlags.DeclaredOnly ||| BindingFlags.Instance
  /// Only declared instance members that may be either public or non-public.
  let [<Literal>] AnyDeclaredInstance = Any ||| DeclaredInstance
  /// Only public declared instance members.
  let [<Literal>] PublicDeclaredInstance = BindingFlags.Public ||| DeclaredInstance

////////////////////////////////////////////////////////////////////////////////

/// An untyped helper interface for dealing with recursion proxies.
type IRecObj =
  abstract GetObj: unit -> obj
  abstract SetObj: obj -> unit
