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
   (if infRule.ContainsGenericParameters
    then infRule.MakeGenericMethod genArgTypes
    else infRule).Invoke (infRules, argObjs) |> Option.ofNull

module InfRuleSet =
  let ofSeq infRules =
   infRules
   |> Seq.fold (fun rulesSet rules -> HashEqSet.add rules rulesSet)
      HashEqSet.empty

  let rules infRuleSet =
    HashEqSet.toSeq infRuleSet
    |> Seq.collect (fun infRules ->
       (infRules.GetType ()).GetMethods BindingFlags.AnyInstance
       |> Seq.map (fun infRule -> InfRule (infRule, infRules)))

  let maybeAddRules (o: obj) infRuleSet =
    match (o.GetType ()).GetCustomAttributes (typeof<InferenceRules>, true) with
     | [||] -> infRuleSet
     | _ -> HashEqSet.add o infRuleSet

/////////////////////////////////////////////////////////////////////////

let (>>=) xO x2yO = Option.bind x2yO xO
let (|>>) xO x2y = Option.map x2y xO
let guard b = if b then Some () else None

/////////////////////////////////////////////////////////////////////////

let rec generate (recRules: RecRulesInvoker)
                 (infRuleSet: HashEqSet<obj>)
                 (knownObjs: HashEqMap<Type, unit -> obj>)
                 (desiredTy: Type) : option<_ * _ * _> =
  match HashEqMap.tryFind desiredTy knownObjs with
   | Some u2o ->
     Some (u2o (), HashEqMap.empty, knownObjs)
   | None ->
     InfRuleSet.rules infRuleSet
     |> Seq.tryPick (fun infRule ->
        tryMatch infRule.ReturnType desiredTy HashEqMap.empty >>= fun v2t ->
        let desiredTy = resolve v2t desiredTy
        guard (not (containsVars desiredTy)) >>= fun () ->
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
             tie desiredObj
             (desiredObj, v2t, HashEqMap.add desiredTy (K desiredObj) knownObjs)
           | parType::parTypes ->
             generate recRules infRuleSet knownObjs parType >>=
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

let tryGenerate (infRules: seq<obj>) (recRules: obj) : option<'a> =
  if containsVars typeof<'a> then
    failwith "Infers can only generate monomorphic values"
  generate
   (RecRulesInvoker recRules)
   (InfRuleSet.ofSeq infRules)
   HashEqMap.empty
   typeof<'a> |>> fun (x, _, _) ->
  unbox<'a> x
