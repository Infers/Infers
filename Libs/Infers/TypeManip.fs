// Copyright (C) by Vesa Karvonen

[<AutoOpen>]
module internal Infers.TypeManip

open System.Collections.Generic
open System.Reflection
open System.Threading
open System

////////////////////////////////////////////////////////////////////////////////

module Fresh =
  type t = int
  let mutable private counter: t = 0

  let newMapper () =
    let v2w = Dictionary<_, _> ()
    fun v ->
      Dictionary.getOr v2w v <| fun () ->
        Interlocked.Increment &counter

////////////////////////////////////////////////////////////////////////////////

type TyCon =
  | Arr of rank: int
  | Def of def: Type

type Ty =
  | Var of Fresh.t
  | App of TyCon * array<Ty>
  | Mono of Type
  override t.ToString () =
    match t with
     | Mono t ->
       sprintf "%A" t
     | Var v -> sprintf "%A" v
     | App (Def tc, [||]) ->
       sprintf "%A" tc
     | App (tc, tys) ->
       match tc with
        | Arr _ ->
          sprintf "array<%A>" tys.[0]
        | Def t ->
          if t = typedefof<_ -> _> then
            sprintf "(%A -> %A)" tys.[0] tys.[1]
          elif t = typedefof<list<_>> then
            sprintf "list<%A>" tys.[0]
          else
            sprintf "%A%A" t tys

let (|App'|Var'|) ty =
  match ty with
   | Var v -> Var' v
   | App (tc, tys) -> App' (tc, tys)
   | Mono t ->
     if t.IsArray then
       App' (Arr (t.GetArrayRank ()), [|t.GetElementType () |> Mono|])
     elif t.IsGenericType then
       App' (Def (t.GetGenericTypeDefinition ()),
             t.GetGenericArguments () |> Array.map Mono)
     else
       App' (Def t, [||])

let (|TyCon'|_|) ty =
  match ty with
   | Var _ -> None
   | App (tc, _) -> Some tc
   | Mono t ->
     if t.IsArray then
       Arr (t.GetArrayRank ()) |> Some
     elif t.IsGenericType then
       Def (t.GetGenericTypeDefinition ()) |> Some
     else
       Def t |> Some

let (|TyArgs'|) ty =
  match ty with
   | Var _ -> [||]
   | App (_, tys) -> tys
   | Mono t ->
     if t.IsArray then
       [|t.GetElementType () |> Mono|]
     elif t.IsGenericType then
       t.GetGenericArguments () |> Array.map Mono
     else
       [||]

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module TyCon =
  let apply tc ts =
    match (tc, ts) with
     | (Def t, ts) ->
       t.MakeGenericType ts
     | (Arr r, [|t|]) ->
       if r = 1
       then t.MakeArrayType ()
       else t.MakeArrayType r
     | _ ->
       failwith "Bug: apply %A %A" tc ts

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Ty =
  let inline containsVars ty =
    match ty with
     | Mono _ -> false
     | Var _ | App _ -> true

  let ofTypeIn env (t: Type) =
    let rec ofType (t: Type) =
      if t.IsArray then
        match t.GetElementType () |> ofType with
         | Mono _ ->
           Mono t
         | ty ->
           App (Arr (t.GetArrayRank ()), [|ty|])
      elif t.IsGenericParameter then
        Var (env t)
      elif t.IsGenericType then
        let tys = t.GetGenericArguments () |> Array.map ofType
        if Array.exists containsVars tys
        then App (Def (t.GetGenericTypeDefinition ()), tys)
        else Mono t
      else
        Mono t
    ofType t

  let inline toMonoType (ty: Ty) =
    match ty with
     | Mono t -> Some t
     | Var _ | App _ -> None

////////////////////////////////////////////////////////////////////////////////

  let rec resolveTop v2ty ty =
    match ty with
     | Var v ->
       match Map.tryFind v v2ty with
        | None -> ty
        | Some ty -> resolveTop v2ty ty
     | Mono _ | App _ ->
       ty

  let rec resolve v2ty ty =
    match resolveTop v2ty ty with
     | App (tc, tys) ->
       let tys = tys |> Array.map (resolve v2ty)
       if Array.exists containsVars tys
       then App (tc, tys)
       else tys
            |> Array.choose toMonoType
            |> TyCon.apply tc
            |> Mono
     | (Mono _ | Var _) as ty ->
       ty

  let rec private occurs v2ty v ty =
    match ty with
     | Mono _ -> false
     | Var v' -> v = v'
     | App (_, tys) ->
       tys
       |> Array.exists (occurs v2ty v << resolveTop v2ty)

  let rec mapVars v2w ty =
    match ty with
     | Mono t -> Mono t
     | Var v ->
       Var (v2w v)
     | App (tc, tys) ->
       App (tc, tys |> Array.map (mapVars v2w))

  let rec tryMatchIn formal actual v2ty =
    match (resolveTop v2ty formal, resolveTop v2ty actual) with
     | (Var fv, Var av) when fv = av ->
       Some v2ty
     | (Var v, ty) | (ty, Var v) when not (occurs v2ty v ty) ->
       Some (Map.add v ty v2ty)
     | (Mono f, Mono a) ->
       if f = a then Some v2ty else None
     | (App' (formal, pars), App' (actual, args)) when formal = actual ->
       assert (pars.Length = args.Length)
       let rec loop i v2ty =
         if i < pars.Length then
           tryMatchIn pars.[i] args.[i] v2ty
           |> Option.bind (loop (i+1))
         else
           Some v2ty
       loop 0 v2ty
     | _ ->
       None

  let tryMatch formal actual =
    tryMatchIn formal actual Map.empty

  let areEqual aTy bTy =
    let rec types aTy bTy v2ty =
      match (aTy, bTy) with
       | (Var a, Var b) ->
         match Map.tryFind a v2ty with
          | Some b' ->
            if b' = b then Some v2ty else None
          | None ->
            Some (Map.add a b v2ty)
       | (App' (aTc, aArgs), App' (bTc, bArgs)) when aTc = bTc ->
         assert (aArgs.Length = bArgs.Length)
         let rec args i v2ty =
           if aArgs.Length <= i then
             Some v2ty
           else
             match types aArgs.[i] bArgs.[i] v2ty with
              | None -> None
              | Some v2ty ->
                args (i+1) v2ty
         args 0 v2ty
       | _ ->
         None
    types aTy bTy Map.empty |> Option.isSome

  type MoreSpecific =
   | Lhs
   | Rhs
   | Equal
   | Unmatchable
   | Incomparable

  let moreSpecific lhs rhs =
    match tryMatch lhs rhs with
     | None -> Unmatchable
     | Some v2ty ->
       assert (areEqual (resolve v2ty lhs) (resolve v2ty rhs))
       match (areEqual (resolve v2ty lhs) lhs,
              areEqual (resolve v2ty rhs) rhs) with
        | ( true,  true) -> Equal
        | ( true, false) -> Lhs
        | (false,  true) -> Rhs
        | (false, false) -> Incomparable

  let inPlaceSelectSpecificFirst (tyrs: array<Ty * 'r>) =
    let swap i j =
      let iEl = tyrs.[i]
      tyrs.[i] <- tyrs.[j]
      tyrs.[j] <- iEl
    for i=0 to tyrs.Length-2 do
      let mutable j = i
      for k=i+1 to tyrs.Length-1 do
        if Rhs = moreSpecific (fst tyrs.[j]) (fst tyrs.[k]) then
          j <- k
      swap i j

////////////////////////////////////////////////////////////////////////////////

/// Represents a position of a type in the structure of a (constructed) type.
/// For example, the type `T<A, U<B>>` has the following positions identified by
/// the brackets: `[T<A, U<B>>]`,  `T<[A], U<B>>`, `T<A, [U<B>]>`, and
/// `T<A, U<[B]>>`.
type TyCursor = list<int>

type TyTree<'r> = Lazy<TyTreeFun<'r>>
and TyTreeFun<'r> =
  | Empty
  | One of 'r
  | Many of array<'r>
  | Branch of at: TyCursor *
              apps: HashEqMap<TyCon, TyTree<'r>> *
              vars: TyTree<'r>

module TyTree =
  let rec internal getAt (is: TyCursor) ty =
    match is with
     | [] ->
       Some ty
     | i::is ->
       match getAt is ty with
        | Some (TyArgs' args) when i < args.Length ->
          Some args.[i]
        | _ ->
          None

  let rec private build' (ats: list<TyCursor>)
                         (tyrs: list<Ty * 'r>) : TyTree<'r> =
    lazy match tyrs with
          | [] -> Empty
          | [(_, r)] -> One r
          | tyrs ->
            match ats with
             | [] ->
               tyrs
               |> Array.ofList
               |>!Ty.inPlaceSelectSpecificFirst
               |> Array.map snd
               |> Many
             | at::ats ->
               tyrs
               |> List.fold
                   (fun (apps, vars) (ty, r) ->
                     match getAt at ty with
                      | None ->
                        failwith "Bug: Only existing positions are produced?"
                      | Some (Var' _) ->
                        (apps, (ty, r)::vars)
                      | Some (App' (tc, args)) ->
                        (HashEqMap.addOrUpdate tc
                          (fun _ -> (args.Length, [(ty, r)]))
                          (fun _ (n, tyrs) -> (n, (ty, r)::tyrs))
                          apps,
                         vars))
                   (HashEqMap.empty, [])
               |> fun (apps, vars) ->
                  let vars = build' ats vars
                  if HashEqMap.isEmpty apps then
                    force vars
                  else
                    (at,
                     apps
                     |> HashEqMap.map
                         (fun (n, tyds) ->
                            let ats = List.init n (fun i -> i::at) @ ats
                            build' ats tyds),
                     vars) |> Branch

  let build tyrs = build' [[]] (List.ofSeq tyrs)

  let rec filter (formal: TyTree<'r>) (actual: Ty) : seq<'r> = seq {
    match force formal with
     | Empty -> ()
     | One r -> yield r
     | Many rs -> yield! rs
     | Branch (at, apps, vars) ->
       match getAt at actual with
        | Some (TyCon' tc) ->
          match HashEqMap.tryFind tc apps with
           | None -> ()
           | Some formal ->
             yield! filter formal actual
        | _ ->
          yield! HashEqMap.toSeq apps
                 |> Seq.collect (fun (_, formal) ->
                    filter formal actual)
       yield! filter vars actual
  }
