#if FSHARP_NON_INTERACTIVE
[<AutoOpen>]
module Infers.TypeManip
#endif

open System
open System.Reflection

/////////////////////////////////////////////////////////////////////////

let (|Var|App|Array|) (t: Type) =
  if t.IsArray then
    Array (t.GetArrayRank (), t.GetElementType ())
  elif t.IsGenericParameter then
    Var t
  elif t.IsGenericType then
    App (t.GetGenericTypeDefinition (), t.GetGenericArguments ())
  else
    App (t, [||])

/////////////////////////////////////////////////////////////////////////

let rec containsVars t =
  match t with
   | Var v -> true
   | App (_, ts) -> Array.exists containsVars ts
   | Array (_, t) -> containsVars t

let rec resolveTop v2t t =
  match t with
   | Var v ->
     match HashEqMap.tryFind v v2t with
      | None -> t
      | Some t -> resolveTop v2t t
   | _ ->
     t

let rec resolve v2t t =
  match resolveTop v2t t with
   | Var v -> v
   | Array (r, t) ->
     let t = resolveTop v2t t
     if r = 1
     then t.MakeArrayType ()
     else t.MakeArrayType r
   | App (c, [||]) -> c
   | App (c, ps) -> c.MakeGenericType (Array.map (resolve v2t) ps)

let rec occurs v2t v t =
  match t with
   | Var v' -> v = v'
   | Array (_, t) -> occurs v2t v t
   | App (_, ts) ->
     ts
     |> Array.exists (occurs v2t v << resolveTop v2t)

let rec tryMatch formal actual v2t =
  match (resolveTop v2t formal, resolveTop v2t actual) with
   | (Var v, t) | (t, Var v) when not (occurs v2t v t) ->
     Some (HashEqMap.add v t v2t)
   | (Array (r1, t1), Array (r2, t2)) when r1 = r2 ->
     tryMatch t1 t2 v2t
   | (App (formal, pars), App (actual, args)) when formal = actual ->
     assert (pars.Length = args.Length)
     let rec loop i v2t =
       if i < pars.Length then
         tryMatch pars.[i] args.[i] v2t
         |> Option.bind (loop (i+1))
       else
         Some v2t
     loop 0 v2t
   | _ ->
     None

/////////////////////////////////////////////////////////////////////////

let prepare (m: MethodInfo) (v2t: HashEqMap<Type, Type>) : MethodInfo =
  if m.ContainsGenericParameters then
    m.MakeGenericMethod (m.GetGenericArguments () |> Array.map (resolve v2t))
  else
    m

let tryInvoke meth formalType actualType this actuals =
  tryMatch formalType actualType HashEqMap.empty
  |> Option.map (fun v2t ->
     (prepare meth v2t).Invoke (this, actuals))
