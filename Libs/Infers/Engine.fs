// Copyright (C) by Vesa Karvonen

namespace Infers

open System.Threading
open System.Collections.Generic
open System.Reflection
open System

////////////////////////////////////////////////////////////////////////////////

type Rule =
  {ReturnType: Ty
   ParTypes: array<Ty>
   GenericArgTypes: array<Ty>
   Invoke: array<Type> -> array<Type> -> array<obj> -> obj}

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Rule =
  let mapVars v2w (rule: Rule) =
    {ReturnType = Ty.mapVars v2w rule.ReturnType
     ParTypes = Array.map (Ty.mapVars v2w) rule.ParTypes
     GenericArgTypes = Array.map (Ty.mapVars v2w) rule.GenericArgTypes
     Invoke = rule.Invoke}

  let freshVars (rule: Rule) : Rule =
    mapVars (Fresh.newMapper ()) rule

module RuleSet =
  type Rules<'x> = Rules'1
  let rulesTy t = typedefof<Rules<_>>.MakeGenericType [|t|]
  let tyOfRulesTy (t: Type) =
    if not t.IsGenericType ||
       t.GetGenericTypeDefinition () <> typedefof<Rules<_>>
    then failwith "Bug"
    else t.GetGenericArguments().[0]

  type Cache =
    {rules: Dictionary<Type, array<Rule>>
     trees: Dictionary<HashEqSet<Type>, TyTree<Rule>>}
  type RuleSet =
    {cache: Cache
     rules: HashEqSet<Type>
     tree: TyTree<Rule>}

  let hasRules (t: Type) =
    t.GetCustomAttributes<InferenceRules> true
    |> Seq.isEmpty
    |> not

  let rec ruleClassesOf t =
    if hasRules t
    then rulesTy t :: ruleClassesOf t.BaseType
    else []

  type B = BindingFlags

  let rulesOf o (t: Type) =
    let t = tyOfRulesTy t
    t.GetMethods (B.DeclaredOnly ||| B.Instance ||| B.Public ||| B.NonPublic)
    |> Array.filter (fun m -> not m.IsAbstract)
    |> Array.map (fun m ->
       let env = Fresh.newMapper ()
       {ReturnType = Ty.ofTypeIn env m.ReturnType
        ParTypes =
          m.GetParameters ()
          |> Array.map (fun p -> Ty.ofTypeIn env p.ParameterType)
        GenericArgTypes =
          if m.ContainsGenericParameters
          then m.GetGenericArguments ()
               |> Array.map (Ty.ofTypeIn env)
          else [||]
        Invoke = fun genArgTys _ argObjs ->
          let m = if m.ContainsGenericParameters
                  then m.MakeGenericMethod genArgTys
                  else m
          m.Invoke (o, argObjs)})

  let newEmpty () =
    {cache = {rules = Dictionary<_, _> ()
              trees = Dictionary<_, _> ()}
     rules = HashEqSet.empty
     tree = TyTree.build []}

  let addRules tys ruleSet =
    let rules =
      tys
      |> List.fold (fun r t -> HashEqSet.add t r) ruleSet.rules
    let tree =
      Dictionary.getOr ruleSet.cache.trees rules <| fun () ->
        lazy (rules
              |> HashEqSet.toSeq
              |> Seq.collect (fun t ->
                 ruleSet.cache.rules.[t]
                 |> Seq.map (fun rule -> (rule.ReturnType, rule)))
              |> TyTree.build
              |> force)
    {ruleSet with rules = rules; tree = tree}

  let maybeAddRulesTy ty ruleSet =
    match ty with
     | App' (Def tc, _) when not tc.IsAbstract
                          && not <| HashEqSet.contains tc ruleSet.rules ->
       if ruleSet.cache.rules.ContainsKey tc |> not then
         let rules =
           tc.GetConstructors ()
           |> Array.map (fun c ->
              let env = Fresh.newMapper ()
              {ReturnType = Ty.ofTypeIn env tc
               ParTypes =
                 c.GetParameters ()
                 |> Array.map (fun p -> Ty.ofTypeIn env p.ParameterType)
               GenericArgTypes =
                 if tc.ContainsGenericParameters
                 then tc.GetGenericArguments () |> Array.map (Ty.ofTypeIn env)
                 else [||]
               Invoke = fun genArgTys argTys argObjs ->
                 let tc = if tc.ContainsGenericParameters
                          then tc.MakeGenericType genArgTys
                          else tc
                 match tc.GetConstructor argTys with
                  | null -> failwith "Bug"
                  | c ->
                    c.Invoke argObjs})
         ruleSet.cache.rules.Add (tc, rules)
       addRules [tc] ruleSet
     | _ ->
       ruleSet

  let maybeAddRulesObj (o: obj) ruleSet =
    match o with
     | null ->
       ruleSet
     | o ->
       let ty = o.GetType ()
       match ruleClassesOf ty with
        | [] -> ruleSet
        | tys ->
          match tys
                |> List.takeWhile (fun ty ->
                   HashEqSet.contains ty ruleSet.rules |> not) with
           | [] -> ruleSet
           | tys ->
             tys
             |> List.iter (fun t ->
                if ruleSet.cache.rules.ContainsKey t |> not then
                  ruleSet.cache.rules.Add (t, rulesOf o t))
             addRules tys ruleSet

  let rulesFor (ruleSet: RuleSet) (desiredTy: Ty) =
    TyTree.filter ruleSet.tree desiredTy

////////////////////////////////////////////////////////////////////////////////

module Engine =
  type Result =
    | Value of monoTy: Type * value: obj
    | Ruled of ty: Ty
             * args: array<Result>
             * genArgTys: array<Ty>
             * invoke: (array<Type> -> array<Type> -> array<obj> -> obj)

  let addObj monoTy o objEnv =
    match o with
     | null -> HashEqMap.add monoTy o objEnv
     | o ->
       let rec lp t objEnv =
         if t = typeof<Object>
            || t = typeof<ValueType>
            || HashEqMap.tryFind t objEnv |> Option.isSome then
           objEnv
         else
           lp t.BaseType (HashEqMap.add t o objEnv)
       lp (o.GetType ()) objEnv

  let rec resolveResult objEnv tyEnv result =
    match result with
     | Value (_, _) -> (objEnv, result)
     | Ruled (ty, args', genArgTys, invoke) ->
       let args = Array.zeroCreate <| Array.length args'
       let rec lp objEnv i =
         if args.Length <= i then
           (objEnv, args)
         else
           let (objEnv, arg) = resolveResult objEnv tyEnv args'.[i]
           args.[i] <- arg
           lp objEnv (i+1)
       let (objEnv, args) = lp objEnv 0
       let ty = Ty.resolve tyEnv ty
       match Ty.toMonoType ty with
        | None ->
          (objEnv, Ruled (ty, args, genArgTys, invoke))
        | Some monoTy ->
          match Array.chooseAll
                  <| function Value (t, v) -> Some (t, v) | Ruled _ -> None
                  <| args
                |> Option.map Array.unzip with
           | None ->
             (objEnv, Ruled (ty, args, genArgTys, invoke))
           | Some (argTys, argVals) ->
             match genArgTys
                   |> Array.chooseAll (Ty.resolve tyEnv >> Ty.toMonoType) with
              | None ->
                failwith "Bug"
              | Some genArgTys ->
                let o = invoke genArgTys argTys argVals
                (addObj monoTy o objEnv, Value (monoTy, o))

  let inline isRec ty =
    match ty with
     | App' (Def tc, _) -> tc = typedefof<Rec<_>>
     | _ -> false

  let rec tryGen limit reached rules objEnv tyEnv stack ty =
    if limit <= 0 then
      reached := true
      Seq.empty
    else
      let search () =
         let limit = limit - 1
         let rules = RuleSet.maybeAddRulesTy ty rules
         RuleSet.rulesFor rules ty
         |> Seq.collect (fun rule ->
            let rule = Rule.freshVars rule
            Ty.tryMatchIn rule.ReturnType ty tyEnv
            |> Option.collect (fun tyEnv ->
               let rec outer args rules objEnv tyEnv ty parTys =
                 let ty = Ty.resolve tyEnv ty
                 let stack = stack |> List.map (Ty.resolve tyEnv)

                 if rule.ParTypes.Length <> 0
                    && stack |> List.exists ((=) ty) then
                  if isRec ty then
                    Seq.empty
                  else
                    match Ty.toMonoType ty with
                     | None -> Seq.empty
                     | Some monoTy ->
                       match HashEqMap.tryFind monoTy objEnv with
                        | Some o ->
                          Seq.singleton (Value (monoTy, o), objEnv, tyEnv)
                        | None ->
                          App (Def typedefof<Rec<_>>, [|ty|])
                          |> tryGen limit reached rules objEnv tyEnv (ty::stack)
                          |> Seq.choose (fun (result, objEnv, tyEnv) ->
                             match result with
                              | Ruled _ -> None
                              | Value (monoRecTy, recO) ->
                                match recO with
                                 | :? IRecObj as recO' ->
                                   let o = recO'.GetObj ()
                                   Some (Value (monoTy, o),
                                         objEnv
                                         |> HashEqMap.add monoRecTy recO
                                         |> addObj monoTy o,
                                         tyEnv)
                                 | _ ->
                                   failwith "Bug")
                 else
                   match parTys with
                    | [] ->
                      let (objEnv, result) =
                        Ruled (ty,
                               List.rev args |> Array.ofList,
                               rule.GenericArgTypes,
                               rule.Invoke)
                        |> resolveResult objEnv tyEnv
                      let objEnv =
                        match result with
                         | Ruled _ -> objEnv
                         | Value (monoTy, o) ->
                           let monoRecTy =
                             typedefof<Rec<_>>.MakeGenericType [|monoTy|]
                           match HashEqMap.tryFind monoRecTy objEnv with
                            | None -> ()
                            | Some recO ->
                              match recO with
                               | :? IRecObj as recO' -> recO'.SetObj o
                               | _ -> failwith "Bug"
                           objEnv
                           |> HashEqMap.add monoTy o
                      Seq.singleton (result, objEnv, tyEnv)
                    | parTy::parTys ->
                      Ty.resolve tyEnv parTy
                      |> tryGen limit reached rules objEnv tyEnv (ty::stack)
                      |> Seq.collect (fun (result, objEnv, tyEnv) ->
                         let rec inner resolvedArgs rules objEnv = function
                           | [] ->
                             outer resolvedArgs rules objEnv tyEnv ty parTys
                           | arg::args ->
                             let (objEnv, resolvedArg) =
                               resolveResult objEnv tyEnv arg
                             inner <| resolvedArg::resolvedArgs
                                   <| match resolvedArg with
                                       | Ruled _ -> rules
                                       | Value (_, value) ->
                                         RuleSet.maybeAddRulesObj value rules
                                   <| objEnv
                                   <| args
                         result::args |> List.rev |> inner [] rules objEnv)
               rule.ParTypes |> Array.toList |> outer [] rules objEnv tyEnv ty))
      match Ty.toMonoType ty with
       | None -> search ()
       | Some monoTy ->
         match HashEqMap.tryFind monoTy objEnv with
          | Some o -> Seq.singleton (Value (monoTy, o), objEnv, tyEnv)
          | None -> search () |> Seq.truncate 1

  let tryGenerate (rules: obj) : option<'a> =
    let desTy = Ty.ofTypeIn <| Fresh.newMapper () <| typeof<'a>
    let rules =
      RuleSet.newEmpty ()
      |> RuleSet.maybeAddRulesObj rules
    let rec gen limit =
      let reached = ref false
      match tryGen limit reached rules HashEqMap.empty Map.empty [] desTy
            |> Seq.tryPick (fun (result, _, _) ->
               match result with
                | Ruled _ -> None
                | Value (_, value) -> Some (unbox<'a> value)) with
       | None ->
         if !reached
         then gen (limit + 1)
         else None
       | some -> some
    gen 1
