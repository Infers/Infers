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
