[<AutoOpen>]
module internal Infers.TypeManip

open System
open System.Reflection

////////////////////////////////////////////////////////////////////////////////

/// Represents a type constructor.  This eliminates the distinction between
/// array types and other generic type definitions in .Net.
type TyCon =
  | Arr of rank: int
  | Def of def: Type

/// Represents a type.  This makes dealing with generic types more convenient.
type Ty =
  | Var of Type
  | App of TyCon * array<Ty>

  /// Converts a .Net type to a `Ty`.
  static member ofType (t: Type) =
    if t.IsArray then
      App (Arr (t.GetArrayRank ()), [|Ty.ofType (t.GetElementType ())|])
    elif t.IsGenericParameter then
      Var t
    elif t.IsGenericType then
      App (Def (t.GetGenericTypeDefinition ()),
           t.GetGenericArguments () |> Array.map Ty.ofType)
    else
      App (Def t, [||])

  /// Converts a `Ty` to the corresponding .Net `Type`.
  static member toType ty =
    match ty with
     | Var t -> t
     | App (Arr r, [|ty|]) -> 
       let t = Ty.toType ty
       if r = 1
       then t.MakeArrayType ()
       else t.MakeArrayType r
     | App (Arr _, _) -> failwithf "Bug: %A" ty
     | App (Def t, [||]) -> t
     | App (Def t, tys) -> t.MakeGenericType (tys |> Array.map Ty.toType)

////////////////////////////////////////////////////////////////////////////////

/// Determines whether the give type contains type variables.  Note that this
/// is not given a substitution.
let rec containsVars ty =
  match ty with
    | Var v -> true
    | App (_, tys) -> Array.exists containsVars tys

/// Resolves the root of the type with respect to the given substition of type
/// variables to types.
let rec resolveTop v2ty ty =
  match ty with
   | Var v ->
     match HashEqMap.tryFind v v2ty with
      | None -> ty
      | Some ty -> resolveTop v2ty ty
   | _ ->
     ty

/// Fully recursively resolves the type with respect to the given substition of
/// type variables to types.
let rec resolve v2ty ty =
  match resolveTop v2ty ty with
   | Var v -> Var v
   | App (tc, tys) ->
     App (tc, tys |> Array.map (resolve v2ty))

/// Tests whether the type variable `v` occurs in the type `ty` with respect to
/// the given substitution of type variables to types.
let rec private occurs v2ty v ty =
  match ty with
   | Var v' -> v = v'
   | App (_, tys) ->
     tys
     |> Array.exists (occurs v2ty v << resolveTop v2ty)

/// Given two types, determines whether they can be unified and, if so, returns
/// substition of type variables to types.
let tryMatch formal actual =
  let rec tryMatch formal actual v2ty =
    match (resolveTop v2ty formal, resolveTop v2ty actual) with
     | (Var fv, Var av) when fv = av ->
       Some v2ty
     | (Var v, ty) | (ty, Var v) when not (occurs v2ty v ty) ->
       Some (HashEqMap.add v ty v2ty)
     | (App (formal, pars), App (actual, args)) when formal = actual ->
       assert (pars.Length = args.Length)
       let rec loop i v2ty =
         if i < pars.Length then
           tryMatch pars.[i] args.[i] v2ty
           |> Option.bind (loop (i+1))
         else
           Some v2ty
       loop 0 v2ty
     | _ ->
       None
  tryMatch formal actual HashEqMap.empty

/// Given two types, determines whether they are equal, taking into
/// consideration the implicit universal quantification of type variables.
let areEqual aTy bTy =
  let rec types aTy bTy v2ty =
    match (aTy, bTy) with
     | (Var a, Var b) ->
       match HashEqMap.tryFind a v2ty with
        | Some b' ->
          if b' = b then Some v2ty else None
        | None ->
          Some (HashEqMap.add a b v2ty)
     | (App (aTc, aArgs), App (bTc, bArgs)) when aTc = bTc ->
       assert (aArgs.Length = bArgs.Length)
       let rec args i v2ty =
         if aArgs.Length <= i then
           Some v2ty
         else
           match types aArgs.[i] bArgs.[i] v2ty with
            | None -> None
            | Some v2ty ->
              args (i+1) v2ty
       args 0 v2ty
     | _ ->
       None
  types aTy bTy HashEqMap.empty |> Option.isSome

/// Result of testing two types by `moreSpecific`.
type MoreSpecific =
 /// Left hand side type is more specific.
 | Lhs
 /// Right hand side type is more specific.
 | Rhs
 /// The two types are equivalent.
 | Equal
 /// The types cannot be unified.
 | Unmatchable
 /// The types unify, but neither is more specific than the other.
 | Incomparable

/// Given two types, determines whether one of the two types is clearly a more
/// specific type with respect to unification and returns that type if so.
let moreSpecific lhs rhs =
  match tryMatch lhs rhs with
  | None -> Unmatchable
  | Some v2ty ->
    assert (areEqual (resolve v2ty lhs) (resolve v2ty rhs))
    match (areEqual (resolve v2ty lhs) lhs, areEqual (resolve v2ty rhs) rhs) with
     | ( true,  true) -> Equal
     | ( true, false) -> Lhs
     | (false,  true) -> Rhs
     | (false, false) -> Incomparable

/// Reorder the type associations in the given array so that types that unify
/// with a smaller set of types are before types that unify with larger sets of
/// types.
let inPlaceSelectSpecificFirst (tyrs: array<Ty * 'r>) =
  let swap i j =
    let iEl = tyrs.[i]
    tyrs.[i] <- tyrs.[j]
    tyrs.[j] <- iEl
  for i=0 to tyrs.Length-2 do
    let mutable j = i
    for k=i+1 to tyrs.Length-1 do
      if Rhs = moreSpecific (fst tyrs.[j]) (fst tyrs.[k]) then
        j <- k
    swap i j

////////////////////////////////////////////////////////////////////////////////

/// Represents a position of a type in the structure of a (constructed) type.
/// For example, the type `T<A, U<B>>` has the following positions identified by
/// the brackets: `[T<A, U<B>>]`,  `T<[A], U<B>>`, `T<A, [U<B>]>`, and
/// `T<A, U<[B]>>`.
type TyCursor = list<int>

/// Examines the structure of the given type to determine whether the type has
/// that position and, if so, returns the type at that position.
let rec getAt is ty =
  match is with
   | [] ->
     Some ty
   | i::is ->
     match getAt is ty with
      | Some (App (_, args)) when i < args.Length ->
        Some args.[i]
      | _ ->
        None

/// Represents a mapping of types to values of type `'r` such that given a type,
/// the type value pairs of the mapping can be filtered based on the given type
/// and produced in an order from more specific to less specific types with
/// respect to unification.
type TyTree<'r> = Lazy<TyTreeFun<'r>>
and TyTreeFun<'r> =
  | Empty
  | One of 'r
  | Many of array<'r>
  | Branch of at: TyCursor *
              apps: HashEqMap<TyCon, TyTree<'r>> *
              vars: TyTree<'r>

/// Operations on type tress.
module TyTree =
  let rec private build' (ats: list<TyCursor>) (tyrs: list<Ty * 'r>) : TyTree<'r> =
    lazy match tyrs with
          | [] -> Empty
          | [(_, r)] -> One r
          | tyrs ->
            match ats with
             | [] ->
               tyrs
               |> Array.ofList
               &> inPlaceSelectSpecificFirst
               |> Array.map snd
               |> Many
             | at::ats ->
               tyrs
               |> List.fold
                   (fun (apps, vars) (ty, r) ->
                     match getAt at ty with
                      | None -> failwith "Bug"
                      | Some (Var _) ->
                        (apps, (ty, r) :: vars)
                      | Some (App (tc, args)) ->
                        (HashEqMap.addOrUpdate tc
                          (fun _ -> (args.Length, [(ty, r)]))
                          (fun _ (n, tyrs) -> (n, (ty, r)::tyrs))
                          apps,
                         vars))
                   (HashEqMap.empty, [])
               |> fun (apps, vars) ->
                  let vars = build' ats vars
                  if HashEqMap.isEmpty apps then
                    force vars
                  else
                    (at,
                     apps
                     |> HashEqMap.map
                         (fun (n, tyds) ->
                            build'
                             (List.init n (fun i -> i::at) @ ats)
                             tyds),
                     vars) |> Branch

  /// Builds a type tree corresponding to the given association sequence.
  let build tyrs = build' [[]] (List.ofSeq tyrs)

  /// Returns a sequence values from the type tree whose associated types may
  /// unify with the given type.
  let rec filter (formal: TyTree<'r>) (actual: Ty) : seq<'r> =
    match force formal with
     | Empty -> Seq.empty
     | One r -> Seq.singleton r
     | Many rs -> rs :> seq<_>
     | Branch (at, apps, vars) ->
       match getAt at actual with
        | None | Some (Var _) -> seq {
            yield! HashEqMap.toSeq apps
                   |> Seq.collect (fun (_, formal) ->
                      filter formal actual)
            yield! filter vars actual
          }
        | Some (App (tc, args)) ->
          match HashEqMap.tryFind tc apps with
           | None ->
             filter vars actual
           | Some formal ->
             filter formal actual

////////////////////////////////////////////////////////////////////////////////

let prepare (m: MethodInfo) (v2ty: HashEqMap<Type, Ty>) : MethodInfo =
  if m.ContainsGenericParameters then
    m.MakeGenericMethod
     (m.GetGenericArguments ()
      |> Array.map (Ty.ofType >> resolve v2ty >> Ty.toType))
  else
    m

let tryInvoke meth formalType actualType this actuals =
  tryMatch formalType actualType
  |> Option.map (fun v2ty ->
     (prepare meth v2ty).Invoke (this, actuals))
