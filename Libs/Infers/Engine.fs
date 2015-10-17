// Copyright (C) by Vesa Karvonen

namespace Infers

open System.Collections.Generic
open System.Reflection
open System

////////////////////////////////////////////////////////////////////////////////

module Fresh =
  let mutable private counter = 0L

  let newMapper () =
    let v2w = Dictionary<_, _> ()
    fun v ->
      match v2w.TryGetValue v with
       | (true, w) -> w
       | (false, _) ->
         let w = System.Threading.Interlocked.Increment &counter
         v2w.Add (v, w)
         w

////////////////////////////////////////////////////////////////////////////////

type Rule<'v> =
  abstract ReturnType: Ty<'v>
  abstract ParTypes: array<Ty<'v>>
  abstract GenericArgTypes: array<Ty<'v>>
  abstract Invoke: array<Type> * array<Type> * array<obj> -> option<obj>

module Rule =
  let mapVars v2w (rule: Rule<_>) =
    let returnType = Ty.mapVars v2w rule.ReturnType
    let parTypes = lazy Array.map (Ty.mapVars v2w) rule.ParTypes
    let genericArgTypes = lazy Array.map (Ty.mapVars v2w) rule.GenericArgTypes
    {new Rule<_> with
      member ir.ReturnType = returnType
      member ir.ParTypes = parTypes.Force ()
      member ir.GenericArgTypes = genericArgTypes.Force ()
      member ir.Invoke (genArgTys, argTys, argObjs) =
       rule.Invoke (genArgTys, argTys, argObjs)}

  let freshVars (rule: Rule<Type>) : Rule<Int64> =
    mapVars (Fresh.newMapper ()) rule

type RuleMethod (m: MethodInfo, o: obj) =
  let returnType = Ty.ofType m.ReturnType
  let parTypes =
    lazy (m.GetParameters ()
          |> Array.map (fun p -> Ty.ofType p.ParameterType))
  let genArgs =
    lazy if m.ContainsGenericParameters
         then m.GetGenericArguments () |> Array.map Ty.ofType
         else [||]
  interface Rule<Type> with
   member ir.ReturnType = returnType
   member ir.ParTypes = parTypes.Force ()
   member ir.GenericArgTypes = genArgs.Force ()
   member ir.Invoke (genArgTys, _, argObjs) =
    let m = if m.ContainsGenericParameters
            then m.MakeGenericMethod genArgTys
            else m
    try m.Invoke (o, argObjs) |> Some
    with :? TargetInvocationException as e ->
      match e.InnerException with
       | Backtrack -> None
       | e -> raise <| Exception ("Rule raised an exception.", e)

module RuleSet =
  type Rules<'x> = Rules'1
  let rulesTy t = typedefof<Rules<_>>.MakeGenericType [|t|]
  let tyOfRulesTy (t: Type) =
    if not t.IsGenericType ||
       t.GetGenericTypeDefinition () <> typedefof<Rules<_>>
    then failwith "Bug"
    else t.GetGenericArguments().[0]

  type Cache =
    {rules: Dictionary<Type, array<Rule<Type>>>
     trees: Dictionary<HashEqSet<Type>, TyTree<Rule<Type>>>}
  type RuleSet =
    {cache: Cache
     rules: HashEqSet<Type>
     tree: TyTree<Rule<Type>>}

  let hasRules (t: Type) =
    t.GetCustomAttributes<InferenceRules> true
    |> Seq.isEmpty
    |> not

  let rec ruleClassesOf t =
    if hasRules t
    then rulesTy t :: ruleClassesOf t.BaseType
    else []

  let rulesOf o (t: Type) =
    let t = tyOfRulesTy t
    t.GetMethods BindingFlags.AnyDeclaredInstance
    |> Array.filter (fun r -> not r.IsAbstract)
    |> Array.map (fun r ->
       RuleMethod (r, o) :> Rule<_>)

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
      match ruleSet.cache.trees.TryGetValue rules with
       | (true, tree) -> tree
       | (false, _) ->
         let tree =
           lazy (rules
                 |> HashEqSet.toSeq
                 |> Seq.collect (fun t ->
                    ruleSet.cache.rules.[t]
                    |> Seq.map (fun rule -> (rule.ReturnType, rule)))
                 |> TyTree.build
                 |> force)
         ruleSet.cache.trees.Add (rules, tree)
         tree
    {ruleSet with rules = rules; tree = tree}

  let maybeAddRulesTy ty ruleSet =
    match ty with
     | App' (Def tc, _) when not tc.IsAbstract
                          && not <| HashEqSet.contains tc ruleSet.rules ->
       if ruleSet.cache.rules.ContainsKey tc |> not then
         let rules =
           tc.GetConstructors ()
           |> Array.map (fun c ->
              let returnType = Ty.ofType tc
              let parTypes =
                lazy (c.GetParameters ()
                      |> Array.map (fun p -> Ty.ofType p.ParameterType))
              let genArgs =
                lazy if tc.ContainsGenericParameters
                     then tc.GetGenericArguments () |> Array.map Ty.ofType
                     else [||]
              {new Rule<Type> with
                member t.ReturnType = returnType
                member t.ParTypes = parTypes.Force ()
                member t.GenericArgTypes = genArgs.Force ()
                member t.Invoke (genArgTys, argTys, argObjs) =
                 let tc = if tc.ContainsGenericParameters
                          then tc.MakeGenericType genArgTys
                          else tc
                 match tc.GetConstructor argTys with
                  | null -> failwith "Bug"
                  | c ->
                    match c.Invoke argObjs with
                     | null -> None
                     | o -> Some o})
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

  let rulesFor (ruleSet: RuleSet) (desiredTy: Ty<_>) =
    TyTree.filter ruleSet.tree desiredTy

////////////////////////////////////////////////////////////////////////////////

module Engine =
  type TyEnv<'v when 'v : equality> = HashEqMap<'v, Ty<'v>>
  type ObjEnv = HashEqMap<Type, obj>

  type Result<'v> =
    | Value of monoTy: Type * value: obj
    | Ruled of ty: Ty<'v> * args: array<Result<'v>> * rule: Rule<'v>

  let rec tryResolveResult objEnv tyEnv result =
    match result with
     | Value (_, _) -> Some (objEnv, result)
     | Ruled (ty, args', rule) ->
       let args = Array.zeroCreate <| Array.length args'
       let rec lp objEnv i =
         if args.Length <= i then
           Some (objEnv, args)
         else
           tryResolveResult objEnv tyEnv args'.[i]
           |> Option.bind (fun (objEnv, arg) ->
              args.[i] <- arg
              lp objEnv (i+1))
       lp objEnv 0
       |> Option.bind (fun (objEnv, args) ->
          let ty = Ty.resolve tyEnv ty
          match Ty.toMonoType ty with
           | None ->
             Some (objEnv, Ruled (ty, args, rule))
           | Some monoTy ->
             match Array.chooseAll
                     <| function Value (t, v) -> Some (t, v) | Ruled _ -> None
                     <| args
                   |> Option.map Array.unzip with
              | None ->
                Some (objEnv, Ruled (ty, args, rule))
              | Some (argTys, argVals) ->
                match rule.GenericArgTypes
                      |> Array.chooseAll (Ty.resolve tyEnv >> Ty.toMonoType) with
                 | None ->
                   failwith "Bug"
                 | Some genArgTys ->
                   rule.Invoke (genArgTys, argTys, argVals)
                   |> Option.map (fun o ->
                      (HashEqMap.add monoTy o objEnv, Value (monoTy, o))))

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
                                         |> HashEqMap.add monoTy o,
                                         tyEnv)
                                 | _ ->
                                   failwith "Bug")
                 else
                   match parTys with
                    | [] ->
                      match Ruled (ty, List.rev args |> Array.ofList, rule)
                            |> tryResolveResult objEnv tyEnv with
                       | None -> Seq.empty
                       | Some (objEnv, result) ->
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
                             match tryResolveResult objEnv tyEnv arg with
                              | None -> Seq.empty
                              | Some (objEnv, resolvedArg) ->
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
    let desTy = typeof<'a> |> Ty.ofType |> Ty.mapVars (Fresh.newMapper ())
    let rules =
      RuleSet.newEmpty ()
      |> RuleSet.maybeAddRulesObj rules
    let rec gen limit =
      let reached = ref false
      match tryGen limit reached rules HashEqMap.empty HashEqMap.empty [] desTy
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
