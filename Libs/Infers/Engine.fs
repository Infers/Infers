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
         let w = (System.Threading.Interlocked.Increment &counter, v)
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

  let freshVars (rule: Rule<Type>) : Rule<Int64 * Type> =
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
       | Some attr ->
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
    let t = desiredTy |> Ty.toType
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

module NextGen =
  type Env<'v when 'v : equality> = HashEqMap<'v, Ty<'v>>

  type Result<'v> =
    | Value of ty: Ty<'v> * value: obj
    | Ruled of ty: Ty<'v> * args: list<Result<'v>> * rule: Rule<'v>

  let rec resolveResult env result =
    match result with
     | Value _ -> result
     | Ruled (ty, args, rule) ->
       let ty = resolve env ty
       let args = args |> List.map (resolveResult env)
       if containsVars ty
          || args |> List.exists (function Value _ -> false | Ruled _ -> true)
       then Ruled (ty, args, rule)
       else let genArgTys =
              rule.GenericArgTypes
              |> Array.map (resolve env >> Ty.toMonoType)
            let argVals =
              args
              |> Array.ofList
              |> Array.map (function Value (_, value) -> value
                                   | _ -> failwith "Bug")
            Value (ty, rule.Invoke (genArgTys, argVals) |> Option.get)

  let rec tryGenerate' nesting
                       limit
                       (rules: RuleSet.RuleSet)
                       env
                       (desTy: Ty<_>) : seq<Result<_> * Env<_>> =
    if limit <= nesting then
      Seq.empty
    else
      let nesting = nesting + 1
      RuleSet.rulesFor rules (mapVars snd desTy)
      |> Seq.map Rule.freshVars
      |> Seq.collect (fun rule ->
         tryMatchIn rule.ReturnType desTy env
         |> Option.toSeq |> Seq.collect (fun env ->
            let rec outer args rules env = function
              | [] ->
                Seq.singleton (Ruled (resolve env desTy, List.rev args, rule), env)
              | parTy::parTys ->
                resolve env parTy
                |> tryGenerate' nesting limit rules env
                |> Seq.collect (fun (result, env) ->
                   let rec inner resolvedArgs rules = function
                     | [] ->
                       outer resolvedArgs rules env parTys
                     | arg::args ->
                       let resolvedArg = resolveResult env arg
                       inner (resolvedArg::resolvedArgs)
                             (match resolvedArg with
                               | Ruled _ -> rules
                               | Value (_, value) ->
                                 RuleSet.maybeAddRules value rules)
                             args
                   inner [] rules (List.rev (result::args)))
            outer [] rules env (rule.ParTypes |> Array.toList)))

  let tryGenerate (rules: obj) : option<'a> =
    let desTy = typeof<'a> |> Ty.ofType |> mapVars (Fresh.newMapper ())
    let rules = RuleSet.ofSeq [rules]
    seq {1 .. Int32.MaxValue}
    |> Seq.collect (fun limit ->
       tryGenerate' 0 limit rules HashEqMap.empty desTy)
    |> Seq.tryPick (fun (result, env) ->
       match resolveResult env result with
        | Ruled _ -> None
        | Value (_, value) -> Some (unbox<'a> value))

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
                      (infRuleSet: RuleSet.RuleSet)
                      (knownObjs: HashEqMap<Ty<_>, unit -> obj>)
                      (desiredTy: Ty<_>) : seq<_ * _ * _> =
    let inline tell u2msg =
      if explain then
        printfn "%s%s" (String.replicate nesting " ") (u2msg ())
    tell <| fun () -> sprintf "desired: %A" (Ty.toType desiredTy)
    match HashEqMap.tryFind desiredTy knownObjs
          |> Option.bind (fun u2o ->
             try Some (u2o ()) with Backtrack -> None) with
     | Some o ->
       tell <| fun () -> sprintf "known"
       result (o, HashEqMap.empty, knownObjs)
     | None ->
       guard (nesting < limit) >>= fun () ->
       let nesting = nesting + 1
       RuleSet.rulesFor infRuleSet desiredTy >>= fun infRule ->
       tell <| fun () ->
         sprintf "trying: %A :- %A"
          (Ty.toType infRule.ReturnType) (Array.map Ty.toType infRule.ParTypes)
       tryMatch infRule.ReturnType desiredTy |> Option.toSeq >>= fun v2t ->
       let desiredTy = resolve v2t desiredTy
       guard (not (containsVars desiredTy)) >>= fun () ->
       tell <| fun () -> sprintf "match as: %A" (Ty.toType desiredTy)
       let parTypes = infRule.ParTypes
       let (knownObjs, tie) =
         let noRec () =
           (HashEqMap.add desiredTy (fun () -> raise Backtrack) knownObjs,
            constant >> HashEqMap.add desiredTy)
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
                 HashEqMap.add desiredTy (constant complete) knownObjs)
            | _ ->
              noRec ()
       let rec lp infRuleSet genArgTypes knownObjs argObjs parTypes =
         match parTypes with
          | [] ->
            let argObjs = Array.ofList (List.rev argObjs)
            infRule.Invoke (Array.map Ty.toType genArgTypes, argObjs)
            |> Option.toSeq |>> fun desiredObj ->
               tell <| fun () -> sprintf "built: %A" (Ty.toType desiredTy)
               (desiredObj, v2t, tie desiredObj knownObjs)
          | parType::parTypes ->
            let results = seq {
              yield! dfsGenerate explain nesting limit infRuleSet knownObjs parType
              do tell <| fun () -> sprintf "FAILED: %A" (Ty.toType parType)
            }
            results >>= fun (argObj, v2t, knownObjs) ->
            lp (RuleSet.maybeAddRules argObj infRuleSet)
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
    let desiredTy = Ty.ofType typeof<'a>
    if containsVars desiredTy then
      failwith "Infers can only generate monomorphic values"
    iddfsGenerate
     explain
     initialDepth
     maxDepth
     (RuleSet.ofSeq rules)
     desiredTy
    |> Seq.tryPick (fun (x, _, _) ->
       Some (unbox<'a> x))

  static member TryGenerate (rules: obj) =
    Engine.TryGenerate (false, Int32.MaxValue, Int32.MaxValue, [rules])

  static member TryGenerate (rules: seq<obj>) =
    Engine.TryGenerate (false, Int32.MaxValue, Int32.MaxValue, rules)
