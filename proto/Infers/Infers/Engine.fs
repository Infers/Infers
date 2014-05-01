#if FSHARP_NON_INTERACTIVE
module Infers.Engine
#endif

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
   with Backtrack -> None

module InfRuleSet =
  let ofSeq infRules =
   infRules
   |> Seq.fold (fun rulesSet rules -> HashEqSet.add rules rulesSet)
      HashEqSet.empty

  let hasInferenceRules (ty: Type) =
    match ty.GetCustomAttributes (typeof<InferenceRules>, true) with
     | [||] -> false
     | _ -> true

  let rules infRuleSet =
    HashEqSet.toSeq infRuleSet
    |> Seq.collect (fun infRules ->
       let rec loop ty =
         if hasInferenceRules ty then
           Seq.concat
            [ty.GetMethods BindingFlags.AnyDeclaredInstance
             |> Seq.map (fun infRule -> InfRule (infRule, infRules))
             loop ty.BaseType]
         else
           Seq.empty
       infRules.GetType () |> loop)

  let maybeAddRules (o: obj) infRuleSet =
    if hasInferenceRules (o.GetType ())
    then HashEqSet.add o infRuleSet
    else infRuleSet

/////////////////////////////////////////////////////////////////////////

let (>>=) xO x2yO = Option.bind x2yO xO
let (|>>) xO x2y = Option.map x2y xO
let guard b = if b then Some () else None

/////////////////////////////////////////////////////////////////////////

let rec generate (explain: bool)
                 (nesting: int)
                 (recRules: RecRulesInvoker)
                 (infRuleSet: HashEqSet<obj>)
                 (knownObjs: HashEqMap<Type, unit -> obj>)
                 (desiredTy: Type) : option<_ * _ * _> =
  let inline tell u2msg =
    if explain then
      printfn "%s%s" (String.replicate nesting " ") (u2msg ())
  let nesting = nesting + 2
  tell <| fun () -> sprintf "desired: %A" desiredTy
  match HashEqMap.tryFind desiredTy knownObjs with
   | Some u2o ->
     tell <| fun () -> sprintf "known"
     Some (u2o (), HashEqMap.empty, knownObjs)
   | None ->
     InfRuleSet.rules infRuleSet
     |> Seq.tryPick (fun infRule ->
        tell <| fun () -> sprintf "trying: %A :- %A" infRule.ReturnType infRule.ParTypes
        tryMatch infRule.ReturnType desiredTy HashEqMap.empty >>= fun v2t ->
        let desiredTy = resolve v2t desiredTy
        guard (not (containsVars desiredTy)) >>= fun () ->
        tell <| fun () -> sprintf "match as: %A" desiredTy
        let (knownObjs, tie) =
          match recRules.Tier desiredTy with
           | None -> (knownObjs, ignore)
           | Some tier ->
             (HashEqMap.add desiredTy
               (fun () -> recRules.Untie (desiredTy, tier)) knownObjs,
              fun complete -> recRules.Tie (desiredTy, tier, complete))
        let rec lp infRuleSet genArgTypes knownObjs argObjs parTypes =
          match parTypes with
           | [] ->
             let argObjs = Array.ofList (List.rev argObjs)
             infRule.Invoke (genArgTypes, argObjs) |>> fun desiredObj ->
             tell <| fun () -> sprintf "built: %A" desiredTy
             tie desiredObj
             (desiredObj, v2t, HashEqMap.add desiredTy (K desiredObj) knownObjs)
           | parType::parTypes ->
             generate explain nesting recRules infRuleSet knownObjs parType >>=
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

let tryGenerate (explain: bool) (infRules: seq<obj>) (recRules: obj) : option<'a> =
  if containsVars typeof<'a> then
    failwith "Infers can only generate monomorphic values"
  generate
   explain
   0
   (RecRulesInvoker recRules)
   (InfRuleSet.ofSeq infRules)
   HashEqMap.empty
   typeof<'a> |>> fun (x, _, _) ->
  unbox<'a> x
