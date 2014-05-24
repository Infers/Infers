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
    lazy if infRule.ContainsGenericParameters
         then infRule.GetGenericArguments () |> Array.map toTy
         else [||]
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
  type InfRuleSet = HashEqMap<obj, TyTree<InfRule>>

  let getInferenceRules (ty: Type) =
    ty.GetCustomAttributes<InferenceRules> true
    |> Seq.tryPick Some

  let preprocess infRules =
    let rec collectRules ty =
      match getInferenceRules ty with
       | None -> Seq.empty
       | Some attr ->
         let bindingFlags =
           if attr.NonPublic
           then BindingFlags.AnyDeclaredInstance
           else BindingFlags.PublicDeclaredInstance
         [ty.GetMethods bindingFlags
          |> Seq.map (fun infRule -> InfRule (infRule, infRules))
          collectRules ty.BaseType]
         |> Seq.concat
    infRules.GetType ()
    |> collectRules
    |> Seq.map (fun rule -> (rule.ReturnType, rule))
    |> TyTree.build

  let ofSeq infRules : InfRuleSet =
   infRules
   |> Seq.fold
       (fun rulesMap rules ->
         HashEqMap.add
          rules
          (preprocess rules)
          rulesMap)
       HashEqMap.empty

  let rulesFor (infRuleSet: InfRuleSet) (desiredTy: Ty) =
    HashEqMap.toSeq infRuleSet
    |> Seq.collect (fun (_, infRulesTree) ->
       TyTree.filter infRulesTree desiredTy)

  let maybeAddRules (o: obj) (infRuleSet: InfRuleSet) =
    if o.GetType () |> getInferenceRules |> Option.isSome
    then HashEqMap.add o (preprocess o) infRuleSet
    else infRuleSet

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module IDDFS =
  let result x = Seq.singleton x
  let (>>=) xO x2yO = Seq.collect x2yO xO
  let (|>>) xO x2y = Seq.map x2y xO
  let guard b = if b then result () else Seq.empty

  let isRec ty =
    match ty with
     | App (Def t, _) when t = typedefof<Rec<_>> -> true
     | _ -> false

  let rec dfsGenerate (explain: bool)
                      (nesting: int)
                      (limit: int)
                      (infRuleSet: InfRuleSet.InfRuleSet)
                      (knownObjs: HashEqMap<Ty, unit -> obj>)
                      (desiredTy: Ty) : seq<_ * _ * _> =
    let inline tell u2msg =
      if explain then
        printfn "%s%s" (String.replicate nesting " ") (u2msg ())
    tell <| fun () -> sprintf "desired: %A" (ofTy desiredTy)
    match HashEqMap.tryFind desiredTy knownObjs
          |> Option.bind (fun u2o ->
             try Some (u2o ()) with Backtrack -> None) with
     | Some o ->
       tell <| fun () -> sprintf "known"
       result (o, HashEqMap.empty, knownObjs)
     | None ->
       guard (nesting < limit) >>= fun () ->
       let nesting = nesting + 1
       InfRuleSet.rulesFor infRuleSet desiredTy >>= fun infRule ->
       tell <| fun () ->
         sprintf "trying: %A :- %A"
          (ofTy infRule.ReturnType) (Array.map ofTy infRule.ParTypes)
       tryMatch infRule.ReturnType desiredTy HashEqMap.empty |> Option.toSeq >>= fun v2t ->
       let desiredTy = resolve v2t desiredTy
       guard (not (containsVars desiredTy)) >>= fun () ->
       tell <| fun () -> sprintf "match as: %A" (ofTy desiredTy)
       let parTypes = infRule.ParTypes
       let (knownObjs, tie) =
         let noRec () =
           (HashEqMap.add desiredTy (fun () -> raise Backtrack) knownObjs,
            K >> HashEqMap.add desiredTy)
         if parTypes.Length = 0 || isRec desiredTy then
           noRec ()
         else
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
               tell <| fun () -> sprintf "built: %A" (ofTy desiredTy)
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
          (parTypes |> Array.map (resolve v2t) |> List.ofArray)

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
