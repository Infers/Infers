module Infers.Engine

open System
open System.Reflection
open System.Collections.Generic

/////////////////////////////////////////////////////////////////////////

let (|Var|App|Array|) (t: Type) =
  if t.IsArray then
    Array (t.GetArrayRank (), t.GetElementType ())
  elif t.IsGenericParameter then
    Var t
  elif t.IsGenericType then
    App (t.GetGenericTypeDefinition (), t.GetGenericArguments ())
  else
    App (t, [||])

/////////////////////////////////////////////////////////////////////////

let rec containsVars t =
  match t with
   | Var v -> true
   | App (_, ts) -> Array.exists containsVars ts
   | Array (_, t) -> containsVars t

let rec resolveTop t v2t =
  match t with
   | Var v ->
     match HashEqMap.tryFind v v2t with
      | None -> t
      | Some t -> resolveTop t v2t
   | _ ->
     t

let rec resolve t v2t =
  match resolveTop t v2t with
   | Var v -> v
   | Array (r, t) ->
     let t = resolveTop t v2t
     if r = 1
     then t.MakeArrayType ()
     else t.MakeArrayType r
   | App (c, [||]) -> c
   | App (c, ps) ->
     c.MakeGenericType (Array.map (fun p -> resolve p v2t) ps)

let rec occurs v t v2t =
  match t with
   | Var v' -> v = v'
   | Array (_, t) -> occurs v t v2t
   | App (_, ts) ->
     ts
     |> Array.exists (fun t -> occurs v (resolveTop t v2t) v2t)

let rec tryMatch (formal: Type)
                 (actual: Type)
                 (v2t: HashEqMap<Type, Type>) : option<HashEqMap<Type, Type>> =
  match (resolveTop formal v2t, resolveTop actual v2t) with
   | (Var v, t) | (t, Var v) when not (occurs v t v2t) ->
     Some (HashEqMap.add v t v2t)
   | (Array (r1, t1), Array (r2, t2)) when r1 = r2 ->
     tryMatch t1 t2 v2t
   | (App (formal, pars), App (actual, args)) when formal = actual ->
     assert (pars.Length = args.Length)
     let rec loop i v2t =
       if i < pars.Length then
         tryMatch pars.[i] args.[i] v2t
         |> Option.bind (fun v2t ->
            loop (i+1) v2t)
       else
         Some v2t
     loop 0 v2t
   | _ ->
     None

/////////////////////////////////////////////////////////////////////////

let maybeAddRulesObject (o: obj) cs =
  match (o.GetType ()).GetCustomAttributes (typeof<InferenceRules>, true) with
   | [||] -> cs
   | _ -> HashEqSet.add o cs

/////////////////////////////////////////////////////////////////////////

let prepare (m: MethodInfo) (v2t: HashEqMap<Type, Type>) : MethodInfo =
  if m.ContainsGenericParameters then
    m.MakeGenericMethod
      (m.GetGenericArguments ()
       |> Array.map (fun t -> resolve t v2t))
  else
    m

type TyCon (tyConObj: obj) =
  let tyConType = tyConObj.GetType ()
  let getMethod name =
    match tyConType.GetMethod
           (name,
            BindingFlags.Public
            ||| BindingFlags.NonPublic
            ||| BindingFlags.Instance) with
     | null ->
       failwithf "Infers.Engine: %A does not have static method named %s"
        tyConType name
     | meth -> meth
  let tier = getMethod "Tier"
  let tierParam =
    match tier.GetParameters () with
     | [|p|] -> p.ParameterType
     | _ -> failwithf "Infers.Engine: Tier method should have one dummy parameter"

  let untie = getMethod "Untie"
  let untieResult = untie.ReturnType

  let tie = getMethod "Tie"
  let tieParam =
    match tie.GetParameters () with
     | [|_;p|] -> p.ParameterType
     | _ -> failwithf "Infers.Engine: Tie method should have two parameters"

  let call (m: MethodInfo) (formalType: Type) (actualType: Type) (actuals: array<obj>) =
    tryMatch formalType actualType HashEqMap.empty
    |> Option.bind (fun v2t ->
       (prepare m v2t).Invoke (tyConObj, actuals) |> Some)

  member x.Tier (t: Type) : option<obj> =
    call tier tierParam t [|null|]
  member x.Untie (t: Type, tier: obj) : obj =
    match call untie untieResult t [|tier|] with
     | None -> failwith "Infers.Engine: Failed to call Untie"
     | Some x -> x
  member x.Tie (t: Type, tier: obj, result: obj) : unit  =
    match call tie tieParam t [|tier; result|] with
     | None -> failwith "Infers.Engine: Failed to call Tie"
     | Some _ -> ()

/////////////////////////////////////////////////////////////////////////

let rec generate' (tyCon: TyCon)
                  (gs: HashEqSet<obj>)
                  (t2o: HashEqMap<Type, unit -> obj>)
                  (t: Type) : seq<_ * _ * _> =
  Seq.concat
   [HashEqMap.tryFind t t2o
    |> Option.toSeq
    |> Seq.collect (fun u2o ->
       Seq.singleton (u2o (), HashEqMap.empty, t2o))
    gs
    |> HashEqSet.toSeq
    |> Seq.collect (fun ro ->
       let t = ro.GetType ()
       t.GetFields (BindingFlags.Public
                    ||| BindingFlags.NonPublic
                    ||| BindingFlags.Instance
                    ||| BindingFlags.Static)
       |> Seq.choose (fun f ->
          tryMatch f.FieldType t HashEqMap.empty
          |> Option.map (fun v2t ->
             let o = f.GetValue ro
             let t2o = HashEqMap.add t (fun () -> o) t2o
             (o, v2t, t2o))))
    Seq.delay <| fun () ->
    if containsVars t then
      failwithf "Infers.Engine: Polymorphic type %A not constrainted by existing object." t
    gs
    |> HashEqSet.toSeq
    |> Seq.collect (fun c ->
       let t = c.GetType ()
       t.GetMethods (BindingFlags.Public
                     ||| BindingFlags.NonPublic
                     ||| BindingFlags.Instance
                     ||| BindingFlags.Static)
       |> Seq.collect (fun m ->
          tryMatch m.ReturnType t HashEqMap.empty
          |> Option.toSeq
          |> Seq.collect (fun v2t ->
             let (t2o, tie) =
               match tyCon.Tier t with
                | None -> (t2o, fun _ -> ())
                | Some tier ->
                  (HashEqMap.add t (fun () -> tyCon.Untie (t, tier)) t2o,
                   fun o -> tyCon.Tie (t, tier, o))
             let rec loop gs gas t2o os ps =
               match ps with
                | [] -> Seq.singleton (Array.ofList (List.rev os), gas, t2o)
                | p::ps ->
                  generate' tyCon gs t2o p
                  |> Seq.collect (fun (o, v2t, t2o) ->
                     loop (maybeAddRulesObject o gs)
                          (gas |> Array.map (fun t -> resolve t v2t))
                          t2o
                          (o::os)
                          (ps |> List.map (fun p -> resolve p v2t)))
             loop
              gs
              (if m.ContainsGenericParameters
               then m.GetGenericArguments () |> Array.map (fun t -> resolve t v2t)
               else [||])
              t2o []
              (m.GetParameters ()
               |> Array.map (fun p -> resolve p.ParameterType v2t) |> List.ofArray)
             |> Seq.choose (fun (os, gas, t2o) ->
                match (if m.ContainsGenericParameters
                       then m.MakeGenericMethod gas
                       else m).Invoke (null, os) with
                 | null -> None
                 | x ->
                   tie x
                   let t2o = HashEqMap.add t (fun () -> x) t2o
                   Some (x, v2t, t2o)))))]

let generate (inferenceRules: seq<obj>) (recursionRules: obj) : option<'a> =
  generate'
   (TyCon recursionRules)
   (inferenceRules
    |> Seq.fold (fun rulesSet rules ->
         HashEqSet.add rules rulesSet)
       HashEqSet.empty)
   HashEqMap.empty
   typeof<'a>
  |> Seq.tryPick (fun (x, _, _) ->
     Some (unbox<'a> x))
