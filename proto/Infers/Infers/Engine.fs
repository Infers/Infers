namespace Infers

open System
open System.Reflection

/////////////////////////////////////////////////////////////////////////

type InfRule (infRule: MethodInfo, infRules: obj) =
  member ir.ReturnType = infRule.ReturnType
  member ir.ParTypes =
   infRule.GetParameters () |> Array.map (fun p -> p.ParameterType)
  member ir.GenericArgTypes =
   if infRule.ContainsGenericParameters
   then infRule.GetGenericArguments ()
   else [||]
  member ir.Invoke (genArgTypes, argObjs) =
   try (if infRule.ContainsGenericParameters
        then infRule.MakeGenericMethod genArgTypes
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
              |> orderMethodsBySpecificFirst
              |> Seq.map (fun infRule -> InfRule (infRule, infRules))
              loop ty.BaseType]
       infRules.GetType () |> loop)

  let maybeAddRules (o: obj) infRuleSet =
    if o.GetType () |> getInferenceRules |> Option.isSome
    then HashEqSet.add o infRuleSet
    else infRuleSet

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module IDDFS =
  let (>>=) xO x2yO = Option.bind x2yO xO
  let (|>>) xO x2y = Option.map x2y xO
  let guard b = if b then Some () else None

  let rec dfsGenerate (explain: bool)
                      (nesting: int)
                      (limit: int)
                      (infRuleSet: HashEqSet<obj>)
                      (knownObjs: HashEqMap<Type, unit -> obj>)
                      (desiredTy: Type) : option<_ * _ * _> =
    let inline tell u2msg =
      if explain then
        printfn "%s%s" (String.replicate nesting " ") (u2msg ())
    tell <| fun () -> sprintf "desired: %A" desiredTy
    match HashEqMap.tryFind desiredTy knownObjs with
     | Some u2o ->
       tell <| fun () -> sprintf "known"
       Some (u2o (), HashEqMap.empty, knownObjs)
     | None ->
       let nesting = nesting + 1
       guard (nesting <= limit) >>= fun () ->
       InfRuleSet.rules infRuleSet
       |> Seq.tryPick (fun infRule ->
          tell <| fun () -> sprintf "trying: %A :- %A" infRule.ReturnType infRule.ParTypes
          tryMatch infRule.ReturnType desiredTy HashEqMap.empty >>= fun v2t ->
          let desiredTy = resolve v2t desiredTy
          guard (not (containsVars desiredTy)) >>= fun () ->
          tell <| fun () -> sprintf "match as: %A" desiredTy
          let recTy = typedefof<Rec<_>>.MakeGenericType [|desiredTy|]
          let (knownObjs, tie) =
            match dfsGenerate explain nesting limit infRuleSet knownObjs recTy with
             | None ->
               (HashEqMap.add desiredTy
                 (fun () -> failwithf "Error: No rec rule for %A" desiredTy)
                 knownObjs,
                K >> HashEqMap.add desiredTy)
             | Some (recObj, _, knownObjs) ->
               let recObj = recObj :?> RecObj
               (HashEqMap.add desiredTy
                 (fun () -> recObj.GetObj ()) knownObjs,
                fun complete knownObjs ->
                  recObj.SetObj complete
                  HashEqMap.add desiredTy (K complete) knownObjs)
          let rec lp infRuleSet genArgTypes knownObjs argObjs parTypes =
            match parTypes with
             | [] ->
               let argObjs = Array.ofList (List.rev argObjs)
               infRule.Invoke (genArgTypes, argObjs) |>> fun desiredObj ->
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
             (infRule.ParTypes |> Array.map (resolve v2t) |> List.ofArray))

  let iddfsGenerate explain initialDepth maxDepth infRuleSet desiredTy =
    seq {initialDepth .. maxDepth}
    |> Seq.tryPick (fun limit ->
       dfsGenerate explain 0 limit infRuleSet HashEqMap.empty desiredTy)

type [<Sealed>] Engine =
  static member TryGenerate (explain: bool,
                             initialDepth: int, 
                             maxDepth: int,
                             rules: seq<obj>) : option<'a> =
    assert (0 <= initialDepth && initialDepth <= maxDepth)
    if containsVars typeof<'a> then
      failwith "Infers can only generate monomorphic values"
    iddfsGenerate
     explain
     initialDepth
     maxDepth
     (InfRuleSet.ofSeq rules)
     typeof<'a> |>> fun (x, _, _) ->
      unbox<'a> x

  static member TryGenerate (rules: obj) =
    Engine.TryGenerate (false, Int32.MaxValue, Int32.MaxValue, [rules])

  static member TryGenerate (rules: seq<obj>) =
    Engine.TryGenerate (false, Int32.MaxValue, Int32.MaxValue, rules)
