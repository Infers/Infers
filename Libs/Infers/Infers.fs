// Copyright (C) by Vesa Karvonen

namespace Infers

open System.Threading.Tasks
open System.Threading
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection
open System

////////////////////////////////////////////////////////////////////////////////

type [<AbstractClass>] Rules () =
  inherit Attribute ()

type [<AbstractClass>] Rec<'x> () =
  abstract Get: unit -> 'x
  abstract Set: 'x -> unit
  // XXX This generates warning FS0050 as the IRecObj interface does not appear
  // in the signature.  This is an internal implementation detail that users
  // should never need to care about and makes it possible to invoke the typed,
  // user defined Get and Set functions dynamically without having to use
  // reflection.  If there is a better way to achieve this in F#, then I'd love
  // to know about it, because being able to have this kind of internal
  // functionality is a fairly common need.
  interface IRecObj with
   override this.GetObj () = box (this.Get ())
   override this.SetObj x = this.Set (unbox<'x> x)

////////////////////////////////////////////////////////////////////////////////

type Rule =
  {ReturnType: Ty
   ParTypes: array<Ty>
   GenericArgTypes: array<Ty>
   Invoke: array<Type> -> array<Type> -> array<obj> -> obj}
  override t.ToString () =
    sprintf "{ReturnType = %A; ParTypes = %A; GenericArgTypes = %A}"
     t.ReturnType t.ParTypes t.GenericArgTypes

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
  type Cache =
    {key: Type
     rules: ConcurrentDictionary<Type, array<Rule>>
     trees: ConcurrentDictionary<HashEqSet<Type>, TyTree<Rule>>
     lastFailures: Queue<string * list<Ty>>
     mutable limitReached: bool}
  type RuleSet =
    {cache: Cache
     rules: HashEqSet<Type>
     tree: TyTree<Rule>}

  let newEmpty key =
    {cache = {key = key
              rules = ConcurrentDictionary<_, _> ()
              trees = ConcurrentDictionary<_, _> ()
              lastFailures = Queue ()
              limitReached = false}
     rules = HashEqSet.empty
     tree = TyTree.build []}

  type B = BindingFlags

  let fieldsOf (t: Type) =
    t.GetFields (B.DeclaredOnly ||| B.Static ||| B.Public ||| B.NonPublic)
    |> Array.map (fun f ->
       {ReturnType = Ty.Mono f.FieldType
        ParTypes = [||]
        GenericArgTypes = [||]
        Invoke = fun _ _ _ -> f.GetValue null})

  let methodsOf (t: Type) =
    t.GetMethods (B.DeclaredOnly ||| B.Static ||| B.Public ||| B.NonPublic)
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
          m.Invoke (null, argObjs)})

  let rulesOf (t: Type) =
    Array.append (fieldsOf t) (methodsOf t)

  let addRules t ruleSet =
    let rules = HashEqSet.add t ruleSet.rules
    let tree =
      ruleSet.cache.trees.GetOrAdd (rules, fun _ ->
        lazy (rules
              |> HashEqSet.toSeq
              |> Seq.collect (fun t ->
                 ruleSet.cache.rules.[t]
                 |> Seq.map (fun rule -> (rule.ReturnType, rule)))
              |> TyTree.build
              |> force))
    {ruleSet with rules = rules; tree = tree}

  let requiresRules (t: Type) =
    t.GetCustomAttributes<Rules> true

  let rec maybeAddRules (t: Type) ruleSet =
    if HashEqSet.contains t ruleSet.rules
    then ruleSet
    else ruleSet.cache.rules.GetOrAdd (t, rulesOf) |> ignore
         requiresRules t
         |> Seq.fold
             (fun ruleSet o ->
                maybeAddRulesObj o ruleSet)
             (addRules t ruleSet)

  and maybeAddRulesObj (o: obj) ruleSet =
    match o with
     | :? Rules -> maybeAddRules (o.GetType ()) ruleSet
     | _ -> ruleSet

  let rulesFor (ruleSet: RuleSet) (desiredTy: Ty) =
    TyTree.filter ruleSet.tree desiredTy

////////////////////////////////////////////////////////////////////////////////

module Infers =
  type Result =
    | Value of monoTy: Type
             * value: obj
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

  let rec resolveResult key objEnv tyEnv result =
    match result with
     | Value (_, _) -> (objEnv, result)
     | Ruled (ty, args', genArgTys, invoke) ->
       let args = Array.zeroCreate <| Array.length args'
       let rec lp objEnv i =
         if args.Length <= i then
           (objEnv, args)
         else
           let (objEnv, arg) = resolveResult key objEnv tyEnv args'.[i]
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
                let o = StaticMap.getOrInvokeDyn key monoTy <| fun () ->
                        invoke genArgTys argTys argVals
                (addObj monoTy o objEnv, Value (monoTy, o))

  let inline isRec ty =
    match ty with
     | App' (Def tc, _) -> tc = typedefof<Rec<_>>
     | _ -> false

  let addFailurePath (rules: RuleSet.RuleSet) stack ty msg =
    lock rules.cache.lastFailures <| fun () ->
    rules.cache.lastFailures.Enqueue (msg, ty::stack)
    if rules.cache.lastFailures.Count > 10 then
      rules.cache.lastFailures.Dequeue () |> ignore

  let rec tryGen limit (rules: RuleSet.RuleSet) objEnv tyEnv stack ty =
    if limit <= 0 then
      rules.cache.limitReached <- true
      Seq.empty
    else
      let search limit =
        RuleSet.rulesFor rules ty
        |> Seq.collect (fun rule ->
           let rule = Rule.freshVars rule
           match Ty.tryMatchIn rule.ReturnType ty tyEnv with
            | None ->
              addFailurePath rules stack ty "No rule for Tycon"
              Seq.empty
            | Some tyEnv ->
               let rec outer args rules objEnv tyEnv ty parTys =
                 let ty = Ty.resolve tyEnv ty
                 let stack = stack |> List.map (Ty.resolve tyEnv)

                 if rule.ParTypes.Length <> 0
                    && stack |> List.exists ((=) ty) then
                  if isRec ty then
                    Seq.empty
                  else
                    match Ty.toMonoType ty with
                     | None ->
                       addFailurePath rules stack ty "Infinite derivation"
                       Seq.empty
                     | Some monoTy ->
                       match HashEqMap.tryFind monoTy objEnv with
                        | Some o ->
                          Seq.singleton (Value (monoTy, o), objEnv, tyEnv)
                        | None ->
                          let recTy = App (Def typedefof<Rec<_>>, [|ty|])
                          match
                            recTy
                            |> tryGen limit rules objEnv tyEnv (ty::stack)
                            |> Seq.tryPick (fun (result, objEnv, tyEnv) ->
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
                                     failwith "Bug") with
                           | None ->
                             addFailurePath rules stack recTy "No Rec rule"
                             Seq.empty
                           | Some x ->
                             Seq.singleton x
                 else
                   match parTys with
                    | [] ->
                      let (objEnv, result) =
                        Ruled (ty,
                               List.rev args |> Array.ofList,
                               rule.GenericArgTypes,
                               rule.Invoke)
                        |> resolveResult rules.cache.key objEnv tyEnv
                      let objEnv =
                        match result with
                         | Ruled _ -> objEnv
                         | Value (monoTy, o') ->
                           let o =
                             StaticMap.getOrSetDyn rules.cache.key monoTy o'
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
                      |> tryGen limit rules objEnv tyEnv (ty::stack)
                      |> Seq.collect (fun (result, objEnv, tyEnv) ->
                         let rec inner resolvedArgs rules objEnv = function
                           | [] ->
                             outer resolvedArgs rules objEnv tyEnv ty parTys
                           | arg::args ->
                             let (objEnv, resolvedArg) =
                               resolveResult rules.cache.key objEnv tyEnv arg
                             inner <| resolvedArg::resolvedArgs
                                   <| match resolvedArg with
                                       | Ruled _ -> rules
                                       | Value (_, value) ->
                                         RuleSet.maybeAddRulesObj value rules
                                   <| objEnv
                                   <| args
                         result::args |> List.rev |> inner [] rules objEnv)
               rule.ParTypes |> Array.toList |> outer [] rules objEnv tyEnv ty)
      match Ty.toMonoType ty with
       | None -> search (limit - 1)
       | Some monoTy ->
         match HashEqMap.tryFind monoTy objEnv with
          | Some o -> Seq.singleton (Value (monoTy, o), objEnv, tyEnv)
          | None -> search limit |> Seq.truncate 1

  let generateWithLimits minDepth maxDepth (rulesObj: obj) : 't =
    let desTy = Ty.ofTypeIn <| Fresh.newMapper () <| typeof<'t>
    let rules =
      RuleSet.newEmpty (rulesObj.GetType ())
      |> RuleSet.maybeAddRulesObj rulesObj
    let rec gen limit =
      rules.cache.limitReached <- false
      rules.cache.lastFailures.Clear ()
      match tryGen limit rules HashEqMap.empty Map.empty [] desTy
            |> Seq.tryPick (fun (result, _, _) ->
               match result with
                | Ruled _ -> None
                | Value (_, value) -> Some (unbox<'t> value)) with
       | None ->
         if rules.cache.limitReached && limit < maxDepth
         then gen (limit + 1)
         else match rules.cache.lastFailures.ToArray () with
               | [||] ->
                 failwithf "%A cannot derive %A."
                  <| rulesObj.GetType ()
                  <| typeof<'t>
               | paths ->
                 failwithf "%A cannot derive %A.  Last failure paths:\n\n%s\n"
                  <| rulesObj.GetType ()
                  <| typeof<'t>
                  <| (paths
                      |> Array.map (fun (msg, path) ->
                         sprintf "%s:\n%s" msg
                          (path
                          |> List.map (sprintf "  %A")
                          |> String.concat "\n"))
                      |> String.concat "\n\n")
       | Some t -> t
    gen minDepth

  let generate' (rules: 'r when 'r :> Rules) : 't =
    generateWithLimits 1 Int32.MaxValue rules

  let generate<'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't =
    StaticMap.getOrInvoke<'r, 't> <| fun () ->
    generate' (new 'r ())

  let generateDFS' (rules: 'r when 'r :> Rules) : 't =
    generateWithLimits Int32.MaxValue Int32.MaxValue rules

  let generateDFS<'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't =
    StaticMap.getOrInvoke<'r, 't> <| fun () ->
    generateDFS' (new 'r ())
