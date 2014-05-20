namespace Infers

open System
open System.Reflection

/////////////////////////////////////////////////////////////////////////

type InfRule (infRule: MethodInfo, infRules: obj) =
  let returnType = toTy infRule.ReturnType
  let parTypes =
    lazy (infRule.GetParameters ()
          |> Array.map (fun p -> toTy p.ParameterType))
  let genArgs =
    lazy (if infRule.ContainsGenericParameters
          then infRule.GetGenericArguments () |> Array.map toTy
          else [||])
  member ir.ReturnType = returnType
  member ir.ParTypes = parTypes.Force ()
  member ir.GenericArgTypes = genArgs.Force ()
  member ir.Invoke (genArgTys, argObjs) =
   try (if infRule.ContainsGenericParameters
        then infRule.MakeGenericMethod (Array.map ofTy genArgTys)
        else infRule).Invoke (infRules, argObjs) |> Some
   with :? TargetInvocationException as e
          when (match e.InnerException with
                 | Backtrack -> true
                 | _ -> false) -> None

module InfRuleSet =
  let ofSeq infRules =
   infRules
   |> Seq.fold (fun rulesSet rules -> HashEqSet.add rules rulesSet)
      HashEqSet.empty

  let getInferenceRules (ty: Type) =
    ty.GetCustomAttributes<InferenceRules> true
    |> Seq.tryPick Some

  type PartialOrder<'T> =
    | PartialOrder of ('T -> 'T -> bool)

  let specificFirst =
    let cmp a b =
      let h = tryMatch a b HashEqMap.empty
      h.IsSome
    PartialOrder cmp

  let orderRulesBySpecificFirst (methods: seq<InfRule>) =
    let (PartialOrder sp) = specificFirst
    methods
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.toArray
    |> Array.sortWith (fun (i, a) (j, b) ->
        if sp a.ReturnType b.ReturnType then -1 else compare i j)
    |> Seq.map snd

  let rules infRuleSet =
    HashEqSet.toSeq infRuleSet
    |> Seq.collect (fun infRules ->
       let rec loop ty =
         match getInferenceRules ty with
          | None -> Seq.empty
          | Some attr ->
            Seq.concat
             [ty.GetMethods (if attr.NonPublic
                             then BindingFlags.AnyDeclaredInstance
                             else BindingFlags.PublicDeclaredInstance)
              |> Seq.map (fun infRule -> InfRule (infRule, infRules))
              |> orderRulesBySpecificFirst
              loop ty.BaseType]
       infRules.GetType () |> loop)

  let maybeAddRules (o: obj) infRuleSet =
    if o.GetType () |> getInferenceRules |> Option.isSome
    then HashEqSet.add o infRuleSet
    else infRuleSet

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module IDDFS =
  let result x = Seq.singleton x
  let (>>=) xO x2yO = Seq.collect x2yO xO
  let (|>>) xO x2y = Seq.map x2y xO
  let guard b = if b then result () else Seq.empty

  let rec dfsGenerate (explain: bool)
                      (nesting: int)
                      (limit: int)
                      (infRuleSet: HashEqSet<obj>)
                      (knownObjs: HashEqMap<Ty, unit -> obj>)
                      (desiredTy: Ty) : seq<_ * _ * _> =
    let inline tell u2msg =
      if explain then
        printfn "%s%s" (String.replicate nesting " ") (u2msg ())
    tell <| fun () -> sprintf "desired: %A" desiredTy
    match HashEqMap.tryFind desiredTy knownObjs
          |> Option.bind (fun u2o ->
             try Some (u2o ()) with Backtrack -> None) with
     | Some o ->
       tell <| fun () -> sprintf "known"
       result (o, HashEqMap.empty, knownObjs)
     | None ->
       guard (nesting < limit) >>= fun () ->
       let nesting = nesting + 1
       InfRuleSet.rules infRuleSet >>= fun infRule ->
       tell <| fun () -> sprintf "trying: %A :- %A" infRule.ReturnType infRule.ParTypes
       tryMatch infRule.ReturnType desiredTy HashEqMap.empty |> Option.toSeq >>= fun v2t ->
       let desiredTy = resolve v2t desiredTy
       guard (not (containsVars desiredTy)) >>= fun () ->
       tell <| fun () -> sprintf "match as: %A" desiredTy
       let (knownObjs, tie) =
         let noRec () =
           (HashEqMap.add desiredTy
             (fun () -> raise Backtrack)
             knownObjs,
            K >> HashEqMap.add desiredTy)
         match desiredTy with
          | App (Def t, _) when t = typedefof<Rec<_>> -> noRec ()
          | _ ->
            let recTy = App (Def typedefof<Rec<_>>, [|desiredTy|])
            match dfsGenerate explain nesting limit infRuleSet knownObjs recTy
                  |> Seq.tryPick Some with
             | Some (:? IRecObj as recObj, _, knownObjs) ->
               (HashEqMap.add desiredTy
                 (fun () -> recObj.GetObj ()) knownObjs,
                fun complete knownObjs ->
                  recObj.SetObj complete
                  HashEqMap.add desiredTy (K complete) knownObjs)
             | _ ->
               noRec ()
       let rec lp infRuleSet genArgTypes knownObjs argObjs parTypes =
         match parTypes with
          | [] ->
            let argObjs = Array.ofList (List.rev argObjs)
            infRule.Invoke (genArgTypes, argObjs)
            |> Option.toSeq |>> fun desiredObj ->
               tell <| fun () -> sprintf "built: %A" desiredTy
               (desiredObj, v2t, tie desiredObj knownObjs)
          | parType::parTypes ->
            dfsGenerate explain nesting limit infRuleSet knownObjs parType >>=
             fun (argObj, v2t, knownObjs) ->
               lp (InfRuleSet.maybeAddRules argObj infRuleSet)
                  (genArgTypes |> Array.map (resolve v2t))
                  knownObjs
                  (argObj::argObjs)
                  (parTypes |> List.map (resolve v2t))
       lp infRuleSet
          (infRule.GenericArgTypes |> Array.map (resolve v2t))
          knownObjs
          []
          (infRule.ParTypes |> Array.map (resolve v2t) |> List.ofArray)

  let iddfsGenerate explain initialDepth maxDepth infRuleSet desiredTy =
    seq {initialDepth .. maxDepth}
    |> Seq.collect (fun limit ->
       dfsGenerate explain 0 limit infRuleSet HashEqMap.empty desiredTy)

type [<Sealed>] Engine =
  static member TryGenerate (explain: bool,
                             initialDepth: int, 
                             maxDepth: int,
                             rules: seq<obj>) : option<'a> =
    assert (0 <= initialDepth && initialDepth <= maxDepth)
    let desiredTy = toTy typeof<'a>
    if containsVars desiredTy then
      failwith "Infers can only generate monomorphic values"
    iddfsGenerate
     explain
     initialDepth
     maxDepth
     (InfRuleSet.ofSeq rules)
     desiredTy
    |> Seq.tryPick (fun (x, _, _) ->
       Some (unbox<'a> x))

  static member TryGenerate (rules: obj) =
    Engine.TryGenerate (false, Int32.MaxValue, Int32.MaxValue, [rules])

  static member TryGenerate (rules: seq<obj>) =
    Engine.TryGenerate (false, Int32.MaxValue, Int32.MaxValue, rules)
