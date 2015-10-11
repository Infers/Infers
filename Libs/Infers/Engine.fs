// Copyright (C) by Vesa Karvonen

namespace Infers

open System
open System.Reflection

/////////////////////////////////////////////////////////////////////////

module Fresh =
  let mutable private counter = 0L

  let newMapper () =
    let v2w = System.Collections.Generic.Dictionary<_, _> ()
    fun v ->
      match v2w.TryGetValue v with
       | (true, w) -> w
       | (false, _) ->
         let w = System.Threading.Interlocked.Increment &counter
         v2w.Add (v, w)
         w

/////////////////////////////////////////////////////////////////////////

type Rule<'v> =
  abstract ReturnType: Ty<'v>
  abstract ParTypes: array<Ty<'v>>
  abstract GenericArgTypes: array<Ty<'v>>
  abstract Invoke: array<Type> * array<obj> -> option<obj>

module Rule =
  let mapVars v2w (rule: Rule<_>) =
    let returnType = mapVars v2w rule.ReturnType
    let parTypes = lazy Array.map (mapVars v2w) rule.ParTypes
    let genericArgTypes = lazy Array.map (mapVars v2w) rule.GenericArgTypes
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
  type RuleSet = HashEqMap<obj, TyTree<Rule<Type>>>

  let getInferenceRules (ty: Type) =
    ty.GetCustomAttributes<InferenceRules> true
    |> Seq.tryPick Some

  let preprocess infRules =
    let rec collectRules ty =
      match getInferenceRules ty with
       | None -> Seq.empty
       | Some _ ->
         [ty.GetMethods BindingFlags.PublicDeclaredInstance
          |> Seq.map (fun infRule ->
             RuleMethod (infRule, infRules) :> Rule<_>)
          collectRules ty.BaseType]
         |> Seq.concat
    infRules.GetType ()
    |> collectRules
    |> Seq.map (fun rule -> (rule.ReturnType, rule))
    |> TyTree.build

  let ofSeq infRules : RuleSet =
    infRules
    |> Seq.fold
        (fun rulesMap rules ->
          HashEqMap.add
           rules
           (preprocess rules)
           rulesMap)
        HashEqMap.empty

  let rulesFor (infRuleSet: RuleSet) (desiredTy: Ty<_>) = seq {
    yield! HashEqMap.toSeq infRuleSet
           |> Seq.collect (fun (_, infRulesTree) ->
              TyTree.filter infRulesTree desiredTy)
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

  let maybeAddRules (o: obj) (infRuleSet: RuleSet) =
    match o with
     | null ->
       infRuleSet
     | o ->
       if o.GetType () |> getInferenceRules |> Option.isSome
          && HashEqMap.tryFind o infRuleSet |> Option.isNone
       then HashEqMap.add o (preprocess o) infRuleSet
       else infRuleSet

/////////////////////////////////////////////////////////////////////////

module Engine =
  type TyEnv<'v when 'v : equality> = HashEqMap<'v, Ty<'v>>
  type ObjEnv<'v when 'v : equality> = HashEqMap<Ty<'v>, obj>

  type Result<'v> =
    | Value of ty: Ty<'v> * value: obj
    | Ruled of ty: Ty<'v> * args: array<Result<'v>> * rule: Rule<'v>

  let rec tryResolveResult objEnv tyEnv result =
    match result with
     | Value _ -> (objEnv, Some result)
     | Ruled (ty, args', rule) ->
       let args = Array.zeroCreate <| Array.length args'
       let rec lp objEnv i =
         if args.Length <= i then
           (objEnv, Some args)
         else
           match tryResolveResult objEnv tyEnv args'.[i] with
            | (objEnv, Some arg) ->
              args.[i] <- arg
              lp objEnv (i+1)
            | (objEnv, None) ->
              (objEnv, None)
       match lp objEnv 0 with
        | (objEnv, None) ->
          (objEnv, None)
        | (objEnv, Some args) ->
          let ty = resolve tyEnv ty
          match (lazy containsVars ty,
                 lazy (Array.chooseAll
                         <| function Value (_, v) -> Some v
                                   | Ruled _ -> None
                         <| args)) with
           | (Force true, _) | (_, Force None) ->
             (objEnv, Ruled (ty, args, rule) |> Some)
           | (Force false, Force (Some argVals)) ->
             let genArgTys =
               rule.GenericArgTypes
               |> Array.map (resolve tyEnv >> Ty.toMonoType >> Option.get) // XXX
             match rule.Invoke (genArgTys, argVals) with
              | None -> (objEnv, None)
              | Some o ->
                (HashEqMap.add ty o objEnv, Value (ty, o) |> Some)

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
         Seq.singleton (Value (ty, o), objEnv, tyEnv)
       | None ->
         let limit = limit - 1
         RuleSet.rulesFor rules ty
         |> Seq.map Rule.freshVars
         |> Seq.collect (fun rule ->
            tryMatchIn rule.ReturnType ty tyEnv
            |> Option.toSeq |> Seq.collect (fun tyEnv ->
               let rec outer args rules objEnv tyEnv ty parTys =
                 let ty = resolve tyEnv ty
                 let stack = stack |> List.map (resolve tyEnv)

                 if rule.ParTypes.Length <> 0 &&
                    isRec ty |> not &&
                    containsVars ty |> not &&
                    stack |> List.exists ((=) ty) then

                   match HashEqMap.tryFind ty objEnv with
                    | Some o ->
                      Seq.singleton (Value (ty, o), objEnv, tyEnv)
                    | None ->
                      let recTy = App (Def typedefof<Rec<_>>, [|ty|])
                      tryGenerate' limit rules objEnv tyEnv (ty::stack) recTy
                      |> Seq.tryPick (fun (result, objEnv, tyEnv) ->
                         match result with
                          | Ruled _ -> None
                          | Value (_, recO) ->
                            match recO with
                             | :? IRecObj as recO' ->
                               let o = recO'.GetObj ()
                               Some (Value (ty, o),
                                     objEnv
                                     |> HashEqMap.add recTy recO
                                     |> HashEqMap.add ty o,
                                     tyEnv)
                             | _ ->
                               failwith "Bug")
                      |> Option.toSeq
                 else
                   match parTys with
                    | [] ->
                      match Ruled (ty, List.rev args |> Array.ofList, rule)
                            |> tryResolveResult objEnv tyEnv with
                       | (_, None) ->
                         Seq.empty
                       | (objEnv, Some result) ->

                         let objEnv =
                           match result with
                            | Value (ty, o) ->
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
                      resolve tyEnv parTy
                      |> tryGenerate' limit rules objEnv tyEnv (ty::stack)
                      |> Seq.collect (fun (result, objEnv, tyEnv) ->
                         let rec inner resolvedArgs rules objEnv = function
                           | [] ->
                             outer resolvedArgs rules objEnv tyEnv ty parTys
                           | arg::args ->
                             match tryResolveResult objEnv tyEnv arg with
                              | (_, None) -> Seq.empty
                              | (objEnv, Some resolvedArg) ->
                                inner <| resolvedArg::resolvedArgs
                                      <| match resolvedArg with
                                          | Ruled _ -> rules
                                          | Value (_, value) ->
                                            RuleSet.maybeAddRules value rules
                                      <| objEnv
                                      <| args
                         result::args |> List.rev |> inner [] rules objEnv)
               rule.ParTypes |> Array.toList |> outer [] rules objEnv tyEnv ty))
         |> if containsVars ty then id else Seq.truncate 1

  let tryGenerate (rules: obj) : option<'a> =
    let desTy = typeof<'a> |> Ty.ofType |> mapVars (Fresh.newMapper ())
    let rules = RuleSet.ofSeq [rules]
    seq {1 .. Int32.MaxValue}
    |> Seq.tryPick (fun limit ->
       tryGenerate' limit rules HashEqMap.empty HashEqMap.empty [] desTy
       |> Seq.tryPick (fun (result, _, _) ->
          match result with
           | Ruled _ -> None
           | Value (_, value) -> Some (unbox<'a> value)))
