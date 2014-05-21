[<AutoOpen>]
module internal Infers.TypeManip

open System
open System.Reflection

/////////////////////////////////////////////////////////////////////////

type TyCon =
  | Arr of rank: int
  | Def of def: Type

type Ty =
  | Var of Type
  | App of TyCon * array<Ty>

let rec toTy (t: Type) =
  if t.IsArray then
    App (Arr (t.GetArrayRank ()), [|toTy (t.GetElementType ())|])
  elif t.IsGenericParameter then
    Var t
  elif t.IsGenericType then
    App (Def (t.GetGenericTypeDefinition ()),
         t.GetGenericArguments () |> Array.map toTy)
  else
    App (Def t, [||])

let rec ofTy ty =
  match ty with
   | Var t -> t
   | App (Arr r, [|ty|]) -> 
     let t = ofTy ty
     if r = 1
     then t.MakeArrayType ()
     else t.MakeArrayType r
   | App (Arr _, _) -> failwithf "Bug: %A" ty
   | App (Def t, [||]) -> t
   | App (Def t, tys) -> t.MakeGenericType (tys |> Array.map ofTy)

/////////////////////////////////////////////////////////////////////////

let rec containsVars (ty: Ty) =
  match ty with
   | Var v -> true
   | App (_, tys) -> Array.exists containsVars tys

let rec resolveTop v2ty ty =
  match ty with
   | Var v ->
     match HashEqMap.tryFind v v2ty with
      | None -> ty
      | Some ty -> resolveTop v2ty ty
   | _ ->
     ty

let rec resolve v2ty ty =
  match resolveTop v2ty ty with
   | Var v -> Var v
   | App (tc, tys) ->
     App (tc, tys |> Array.map (resolve v2ty))

let rec occurs v2ty v ty =
  match ty with
   | Var v' -> v = v'
   | App (_, tys) ->
     tys
     |> Array.exists (occurs v2ty v << resolveTop v2ty)

let rec tryMatch formal actual v2ty =
  match (resolveTop v2ty formal, resolveTop v2ty actual) with
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

/////////////////////////////////////////////////////////////////////////

type TyCursor = list<int>

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

type TyTree<'r> =
  | Lf of list<'r>
  | Br of at: TyCursor *
          apps: HashEqMap<TyCon, TyTree<'r>> *
          vars: TyTree<'r>

let rec filter (formal: TyTree<'r>) (actual: Ty) : seq<'r> =
  match formal with
   | Lf results ->
     Seq.ofList results
   | Br (at, apps, vars) ->
     let all () =
       Seq.concat
        [HashEqMap.toSeq apps
         |> Seq.collect (fun (_, formal) ->
            filter formal actual)
         filter vars actual]
     match getAt at actual with
      | None ->
        all ()
      | Some (App (tc, args)) ->
        match HashEqMap.tryFind tc apps with
         | None ->
           Seq.empty
         | Some formal ->
           filter formal actual
      | Some (Var _) ->
        all ()

let rec build (ats: list<TyCursor>) (tyrs: list<Ty * 'r>) : TyTree<'r> =
  match tyrs with
   | [] -> Lf []
   | [(_, r)] -> Lf [r]
   | tyrs ->
     match ats with
      | [] -> Lf (List.map snd tyrs)
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
           if 0 = HashEqMap.count apps then
             build ats vars
           else
             Br (at,
                 apps
                 |> HashEqMap.map
                     (fun (n, tyds) ->
                        build
                         (List.init n (fun i -> i::at) @ ats)
                         tyds),
                 build ats vars)

/////////////////////////////////////////////////////////////////////////

let prepare (m: MethodInfo) (v2ty: HashEqMap<Type, Ty>) : MethodInfo =
  if m.ContainsGenericParameters then
    m.MakeGenericMethod
     (m.GetGenericArguments ()
      |> Array.map (toTy >> resolve v2ty >> ofTy))
  else
    m

let tryInvoke meth formalType actualType this actuals =
  tryMatch formalType actualType HashEqMap.empty
  |> Option.map (fun v2ty ->
     (prepare meth v2ty).Invoke (this, actuals))
