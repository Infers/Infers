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
  abstract Invoke: array<Type> * array<obj> -> option<obj>

module Rule =
  let mapVars v2w (rule: Rule<_>) =
    let returnType = Ty.mapVars v2w rule.ReturnType
    let parTypes = lazy Array.map (Ty.mapVars v2w) rule.ParTypes
    let genericArgTypes = lazy Array.map (Ty.mapVars v2w) rule.GenericArgTypes
    {new Rule<_> with
      member ir.ReturnType = returnType
      member ir.ParTypes = parTypes.Force ()
      member ir.GenericArgTypes = genericArgTypes.Force ()
      member ir.Invoke (genArgTys, argObjs) = rule.Invoke (genArgTys, argObjs)}

  let freshVars (rule: Rule<Type>) : Rule<Int64> =
    mapVars (Fresh.newMapper ()) rule

type RuleMethod (infRule: MethodInfo, infRules: obj) =
  let returnType = Ty.ofType infRule.ReturnType
  let parTypes =
    lazy (infRule.GetParameters ()
          |> Array.map (fun p -> Ty.ofType p.ParameterType))
  let genArgs =
    lazy if infRule.ContainsGenericParameters
         then infRule.GetGenericArguments () |> Array.map Ty.ofType
         else [||]
  interface Rule<Type> with
   member ir.ReturnType = returnType
   member ir.ParTypes = parTypes.Force ()
   member ir.GenericArgTypes = genArgs.Force ()
   member ir.Invoke (genArgTys, argObjs) =
    try (if infRule.ContainsGenericParameters
         then infRule.MakeGenericMethod genArgTys
         else infRule).Invoke (infRules, argObjs) |> Some
    with :? TargetInvocationException as e
           when (match e.InnerException with
                  | Backtrack -> true
                  | _ -> false) -> None

module RuleSet =
  type Cache =
    {rules: Dictionary<Type, array<Rule<Type>>>
     trees: Dictionary<HashEqSet<Type>, TyTree<Rule<Type>>>}
  type RuleSet =
    {cache: Cache
     rules: HashEqSet<Type>
     tree: TyTree<Rule<Type>>}

  let hasInferenceRules (ty: Type) =
    ty.GetCustomAttributes<InferenceRules> true
    |> Seq.isEmpty
    |> not

  let collectRules o =
    let rec collectRules ty =
      if hasInferenceRules ty then
        [ty.GetMethods BindingFlags.AnyDeclaredInstance
         |> Seq.map (fun infRule ->
            RuleMethod (infRule, o) :> Rule<_>)
         collectRules ty.BaseType]
        |> Seq.concat
      else
        Seq.empty
    o.GetType ()
    |> collectRules

  let newEmpty () =
    {cache = {rules = Dictionary<_, _> ()
              trees = Dictionary<_, _> ()}
     rules = HashEqSet.empty
     tree = TyTree.build []}

  let maybeAddRules (o: obj) ruleSet =
    match o with
     | null ->
       ruleSet
     | o ->
       let t = o.GetType ()
       if hasInferenceRules t
          && HashEqSet.contains t ruleSet.rules |> not then
         let rules = HashEqSet.add t ruleSet.rules
         if ruleSet.cache.rules.ContainsKey t |> not then
           ruleSet.cache.rules.Add (t, collectRules o |> Seq.toArray)
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
       else
         ruleSet

  let rulesFor (ruleSet: RuleSet) (desiredTy: Ty<_>) = seq {
    yield! TyTree.filter ruleSet.tree desiredTy
    match Ty.toMonoType desiredTy with
     | None -> ()
     | Some t ->
       if not t.IsAbstract then
         match t.GetConstructor [||] with
          | null -> ()
          | _ ->
            let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
            let returnType = Ty.ofType t
            let genArgs =
              lazy if t.IsGenericType
                   then t.GetGenericArguments () |> Array.map Ty.ofType
                   else [||]
            yield {new Rule<_> with
                    member this.ReturnType = returnType
                    member this.ParTypes = [||]
                    member this.GenericArgTypes = genArgs.Force ()
                    member this.Invoke (genArgTys: array<Type>, _: array<obj>) =
                     (if t.IsGenericType
                      then t.GetGenericTypeDefinition().MakeGenericType(genArgTys)
                      else t).GetConstructor([||]).Invoke [||] |> Some}
  }

////////////////////////////////////////////////////////////////////////////////

module Engine =
  type TyEnv<'v when 'v : equality> = HashEqMap<'v, Ty<'v>>
  type ObjEnv<'v when 'v : equality> = HashEqMap<Ty<'v>, obj>

  type Result<'v> =
    | Value of obj
    | Ruled of ty: Ty<'v> * args: array<Result<'v>> * rule: Rule<'v>

  let rec tryResolveResult objEnv tyEnv result =
    match result with
     | Value _ -> Some (objEnv, result)
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
          if Ty.containsVars ty then
            Some (objEnv, Ruled (ty, args, rule))
          else
            match Array.chooseAll
                   <| function Value v -> Some v
                             | Ruled _ -> None
                   <| args with
             | None ->
               Some (objEnv, Ruled (ty, args, rule))
             | Some argVals ->
               match rule.GenericArgTypes
                     |> Array.chooseAll (Ty.resolve tyEnv >> Ty.toMonoType) with
                | None ->
                  failwith "Bug"
                | Some genArgTys ->
                  rule.Invoke (genArgTys, argVals)
                  |> Option.map (fun o ->
                     (HashEqMap.add ty o objEnv, Value o)))

  let inline isRec ty =
    match ty with
     | App (Def tc, _) -> tc = typedefof<Rec<_>>
     | _ -> false

  let rec tryGenerate' limit rules objEnv tyEnv stack ty =
    if limit <= 0 then
      Seq.empty
    else
      match HashEqMap.tryFind ty objEnv with
       | Some o ->
         Seq.singleton (Value o, objEnv, tyEnv)
       | None ->
         let limit = limit - 1
         RuleSet.rulesFor rules ty
         |> Seq.collect (fun rule ->
            let rule = Rule.freshVars rule
            Ty.tryMatchIn rule.ReturnType ty tyEnv
            |> Option.collect (fun tyEnv ->
               let rec outer args rules objEnv tyEnv ty parTys =
                 let ty = Ty.resolve tyEnv ty
                 let stack = stack |> List.map (Ty.resolve tyEnv)

                 if rule.ParTypes.Length <> 0 &&
                    isRec ty |> not &&
                    Ty.containsVars ty |> not &&
                    stack |> List.exists ((=) ty) then

                   match HashEqMap.tryFind ty objEnv with
                    | Some o ->
                      Seq.singleton (Value o, objEnv, tyEnv)
                    | None ->
                      let recTy = App (Def typedefof<Rec<_>>, [|ty|])
                      tryGenerate' limit rules objEnv tyEnv (ty::stack) recTy
                      |> Seq.choose (fun (result, objEnv, tyEnv) ->
                         match result with
                          | Ruled _ -> None
                          | Value recO ->
                            match recO with
                             | :? IRecObj as recO' ->
                               let o = recO'.GetObj ()
                               Some (Value o,
                                     objEnv
                                     |> HashEqMap.add recTy recO
                                     |> HashEqMap.add ty o,
                                     tyEnv)
                             | _ ->
                               failwith "Bug")
                 else
                   match parTys with
                    | [] ->
                      match Ruled (ty, List.rev args |> Array.ofList, rule)
                            |> tryResolveResult objEnv tyEnv with
                       | None ->
                         Seq.empty
                       | Some (objEnv, result) ->

                         let objEnv =
                           match result with
                            | Value o ->
                              let recTy = App (Def typedefof<Rec<_>>, [|ty|])
                              match HashEqMap.tryFind recTy objEnv with
                               | None ->
                                 ()
                               | Some recO ->
                                 match recO with
                                  | :? IRecObj as recO' ->
                                    recO'.SetObj o
                                  | _ ->
                                    failwith "Bug"
                              objEnv
                              |> HashEqMap.add ty o
                            | Ruled _ ->
                              objEnv

                         Seq.singleton (result, objEnv, tyEnv)
                    | parTy::parTys ->
                      Ty.resolve tyEnv parTy
                      |> tryGenerate' limit rules objEnv tyEnv (ty::stack)
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
                                          | Value value ->
                                            RuleSet.maybeAddRules value rules
                                      <| objEnv
                                      <| args
                         result::args |> List.rev |> inner [] rules objEnv)
               rule.ParTypes |> Array.toList |> outer [] rules objEnv tyEnv ty))
         |> if Ty.containsVars ty then id else Seq.truncate 1

  let tryGenerate (rules: obj) : option<'a> =
    let desTy = typeof<'a> |> Ty.ofType |> Ty.mapVars (Fresh.newMapper ())
    let rules =
      RuleSet.newEmpty ()
      |> RuleSet.maybeAddRules rules
    seq {1 .. Int32.MaxValue}
    |> Seq.tryPick (fun limit ->
       tryGenerate' limit rules HashEqMap.empty HashEqMap.empty [] desTy
       |> Seq.tryPick (fun (result, _, _) ->
          match result with
           | Ruled _ -> None
           | Value value -> Some (unbox<'a> value)))
