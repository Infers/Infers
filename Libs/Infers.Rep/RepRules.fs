// Copyright (C) by Vesa Karvonen

namespace Infers.Rep

open Microsoft.FSharp.Reflection
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit
open System.Threading
open Infers

////////////////////////////////////////////////////////////////////////////////

type B = BindingFlags
type C = CallingConventions
type F = FieldAttributes
type M = MethodAttributes
type T = TypeAttributes

////////////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module Util =
  let memoDict = ConcurrentDictionary<Type, Lazy<obj>>()
  let inline memo t (oL: Lazy<_>) = memoDict.GetOrAdd(t, oL).Force()

  let repModule =
    let appDomain = AppDomain.CurrentDomain;
    let assemblyName = AssemblyName "RepAssembly"
    let assemblyBuilder =
      appDomain.DefineDynamicAssembly
       (assemblyName, AssemblyBuilderAccess.RunAndCollect)
    assemblyBuilder.DefineDynamicModule assemblyName.Name

  let mutable private uniqueId = 0
  let inline uniqueName () =
    let i = Interlocked.Increment &uniqueId
    sprintf "Generated%d" i

  type Builder<'a> =
    TypeBuilder * list<string * FieldBuilder * Type * obj>
     -> list<string * FieldBuilder * Type * obj> * 'a

  let inline (>>=) xB x2yB = fun (tB, vs) ->
    let (vs, x) = xB (tB, vs)
    x2yB x (tB, vs)

  let build (tycon: Type) (elems: array<Type>) =
    if 0 < elems.Length then
      let results = Array.zeroCreate elems.Length
      results.[elems.Length-1] <- elems.[elems.Length-1]
      for i=elems.Length-2 downto 0 do
        results.[i] <- tycon.MakeGenericType [|elems.[i]; results.[i+1]|]
      results
    else
      [|typeof<Empty>|] // Special case

type Builder =
  static member inline result x : Builder<_> = fun (_, vs) -> (vs, x)

  static member inline getTypeBuilder : Builder<TypeBuilder> =
    fun (typeBuilder, values) ->
      (values, typeBuilder)

  static member metaType (baseType: Type)
                         (basePars: array<string * obj>)
                         (builder: Builder<unit>) =
    let (values, metaType) =
      lock repModule <| fun () ->
      let typeBuilder = repModule.DefineType (uniqueName (), T.Public, baseType)
      let (values, ()) = builder (typeBuilder, [])
      (values, typeBuilder.CreateType ())
    values
    |> List.iter (fun (name, _, _, value) ->
       match metaType.GetField (name, B.Public ||| B.Static) with
        | null ->
          failwithf "Type %A :> %A has no static field named %s"
           metaType baseType name
        | field ->
          field.SetValue (null, value))
    let ctor = metaType.GetConstructor [||]
    let o = ctor.Invoke null
    let fields = metaType.GetFields (B.Instance ||| B.NonPublic)
    for (name, value) in basePars do
      match fields |> Array.tryFind (fun f -> f.Name.StartsWith name) with
       | None ->
         failwithf "Expected field named '%s' in %A" name baseType
       | Some field ->
         field.SetValue (o, value)
    o

  static member metaValue name fb baseType (value: Object) =
    fun (_, values) ->
      ((name, fb, baseType, value)::values, ())

  static member metaField baseType basePars (definition: Builder<unit>) =
    Builder.getTypeBuilder >>= fun typeBuilder ->
    let name = uniqueName ()
    let fb = typeBuilder.DefineField (name, baseType, F.Static ||| F.Public)
    Builder.metaValue
     name fb baseType
     (Builder.metaType baseType basePars definition)

  static member inline emit op = fun (ilgen: ILGenerator) ->
    ilgen.Emit op ; ilgen

  static member inline emit (op, m: MethodInfo) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, m) ; ilgen

  static member inline emit (op, c: ConstructorInfo) = fun (ilg: ILGenerator) ->
    ilg.Emit (op, c) ; ilg

  static member inline emit (op, f: FieldInfo) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, f) ; ilgen

  static member inline emit (op, t: Type) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, t) ; ilgen

  static member inline emit (op, v: LocalBuilder) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, v) ; ilgen

  static member overrideMethod name resultType paramTypes code =
    Builder.getTypeBuilder >>= fun typeBuilder ->
    let methodBuilder =
      typeBuilder.DefineMethod
       (name, M.Public ||| M.HideBySig ||| M.Virtual, resultType, paramTypes)
    let ilgen = methodBuilder.GetILGenerator ()
    code ilgen |> ignore
    Builder.result ()

  static member overrideGetMethod name paramType (property: PropertyInfo) =
    match property.GetGetMethod () with
     | null ->
       failwithf "Failed to get Get-method for property \"%s\" of \"%s\""
        property.Name property.DeclaringType.Name
     | getMethod ->
       Builder.overrideMethod name
        property.PropertyType
        [|paramType (*property.DeclaringType*)|]
        (Builder.emit (OpCodes.Ldarg_1) >>
         Builder.emit (OpCodes.Call, getMethod) >>
         Builder.emit (OpCodes.Ret))

  static member overrideSetMethodWhenCanWrite name (property: PropertyInfo) =
    if property.CanWrite then
      match property.GetSetMethod () with
       | null ->
         failwithf "Failed to get Set-method for property \"%s\" of \"%s\""
          property.Name property.DeclaringType.Name
       | setMethod ->
         Builder.overrideMethod name
          typeof<Void>
          [|property.DeclaringType; property.PropertyType|]
          (Builder.emit (OpCodes.Ldarg_1) >>
           Builder.emit (OpCodes.Ldarg_2) >>
           Builder.emit (OpCodes.Call, setMethod) >>
           Builder.emit (OpCodes.Ret))
    else
      Builder.result ()

  static member forTo i0 i1 (on: int -> Builder<unit>) : Builder<unit> =
    if i0 <= i1 then
      on i0 >>= fun () ->
      Builder.forTo (i0+1) i1 on
    else
      Builder.result ()

////////////////////////////////////////////////////////////////////////////////

module Products =
  let products ts = build typedefof<Pair<_, _>> ts

  let defineExtractAndCreate t
                             emitCtor
                             (props: array<PropertyInfo>)
                             (products: array<Type>) =
    let getField (t: Type) name =
      match t.GetField name with
       | null -> failwithf "The %A type has no field named \"%s\"" t name
       | field -> field
    let inline emitGet i =
      match props.[i].GetGetMethod () with
       | null ->
         failwithf "Failed to get Get-method for property \"%s\" of \"%s\""
          props.[i].Name props.[i].DeclaringType.Name
       | getMethod ->
         Builder.emit (OpCodes.Ldarg_1) >>
         Builder.emit (OpCodes.Call, getMethod)
    let rec emitCopies i =
      if i+1 = props.Length
      then emitGet i
           >> Builder.emit (OpCodes.Stobj, products.[i])
      else Builder.emit OpCodes.Dup
           >> emitGet i
           >> Builder.emit (OpCodes.Stfld, getField products.[i] "Elem")
           >> Builder.emit (OpCodes.Ldflda, getField products.[i] "Rest")
           >> emitCopies (i+1)
    let rec emitLoads ldaddr i =
      ldaddr
      >> if i+1 = props.Length
         then Builder.emit (OpCodes.Ldobj, products.[i])
         else Builder.emit (OpCodes.Ldfld, getField products.[i] "Elem")
              >> ldaddr
              >> let r = getField products.[i] "Rest" in
                 if i+1 < props.Length
                 then fun (ilg: ILGenerator) ->
                        let v = ilg.DeclareLocal (r.FieldType.MakeByRefType ())
                        ilg
                        |> Builder.emit (OpCodes.Ldflda, r)
                        |> Builder.emit (OpCodes.Stloc, v)
                        |> emitLoads (Builder.emit (OpCodes.Ldloc, v)) (i+1)
                 else Builder.emit (OpCodes.Ldfld, r)
    Builder.overrideMethod "Extract"
     typeof<Void>
     [|t; products.[0].MakeByRefType ()|]
     (if props.Length > 0
      then Builder.emit OpCodes.Ldarg_2
           >> emitCopies 0
      else id
      >> Builder.emit OpCodes.Ret) >>= fun () ->
    Builder.overrideMethod "Create"
     t
     [|products.[0].MakeByRefType ()|]
     (if props.Length > 0
      then emitLoads (Builder.emit OpCodes.Ldarg_1) 0
      else id
      >> emitCtor
      >> Builder.emit (OpCodes.Ret))

  let asPairsField (t: Type)
                   (emitCtor: ILGenerator -> ILGenerator)
                   (props: array<PropertyInfo>)
                   (products: array<Type>)
                   (defineRest: Builder<unit>) =
    Builder.metaField
     (typedefof<AsPairs<_, _, _>>.MakeGenericType [|products.[0]; t; t|])
     [|("Arity", box props.Length)
       ("IsMutable", box (props |> Array.exists (fun p -> p.CanWrite)))|]
     (defineExtractAndCreate t emitCtor props products >>= fun () ->
      defineRest)

////////////////////////////////////////////////////////////////////////////////

module Unions =
  let choices ts = build typedefof<Choice<_, _>> ts

  let asChoicesField (t: Type) (choices: array<Type>) defineRest =
    Builder.metaField
     (typedefof<AsChoices<_, _>>.MakeGenericType [|choices.[0]; t|])
     [|("Arity", box choices.Length)|]
     ((match FSharpValue.PreComputeUnionTagMemberInfo (t, B.Public) with
        | :? PropertyInfo as prop ->
          Builder.overrideGetMethod "Tag" t prop
        | :? MethodInfo as meth when meth.IsStatic ->
          Builder.overrideMethod "Tag" typeof<int> [|t|]
           (Builder.emit (OpCodes.Ldarg_1) >>
            Builder.emit (OpCodes.Call, meth) >>
            Builder.emit (OpCodes.Ret))
        | _ ->
          failwith "Expected PropertyInfo or static MethodInfo.") >>= fun () ->
      defineRest)

////////////////////////////////////////////////////////////////////////////////

type Prim'<'t> () = inherit Prim<'t> ()
type Unsupported'<'t> () = inherit Unsupported<'t> ()

type Rep () =
  inherit Rules ()

  static member Rep () : Rep<'t> =
    let t = typeof<'t>
    unbox<Rep<'t>> << memo t << Lazy.Create <| fun () ->
    if FSharpType.IsRecord (t, B.Public) then
      let fields = FSharpType.GetRecordFields (t, B.Public)
      let products =
        fields |> Array.map (fun p -> p.PropertyType) |> Products.products

      Builder.metaType typeof<Record<'t>> [||]
       (Products.asPairsField t
         (Builder.emit
           (OpCodes.Newobj,
            FSharpValue.PreComputeRecordConstructorInfo (t, B.Public)))
         fields
         products
         (Builder.forTo 0 (fields.Length-1) <| fun i ->
            Builder.metaField
             (typedefof<Field<_, _, _>>.MakeGenericType
               [|fields.[i].PropertyType; products.[i]; t|])
             [|("Index", box i)
               ("Name", box fields.[i].Name)
               ("IsMutable", box fields.[i].CanWrite)|]
             (Builder.overrideGetMethod "Get" t fields.[i] >>= fun () ->
              Builder.overrideSetMethodWhenCanWrite "Set" fields.[i])))
    elif FSharpType.IsUnion (t, B.Public) then
      let cases = FSharpType.GetUnionCases (t, B.Public)
      let caseFields = cases |> Array.map (fun case -> case.GetFields ())
      let caseProducts =
        caseFields
        |> Array.map
            (Array.map (fun p -> p.PropertyType) >> Products.products)
      let choices =
        caseProducts |> Array.map (fun ts -> ts.[0]) |> Unions.choices

      Builder.metaType typeof<Union<'t>> [||]
       (Unions.asChoicesField t choices
         (Builder.forTo 0 (cases.Length-1) (fun i ->
           Builder.metaField
            (typedefof<Case<_, _, _>>.MakeGenericType
              [|caseProducts.[i].[0]; choices.[i]; t|])
            [|("Name", box cases.[i].Name)
              ("Arity", box caseFields.[i].Length)
              ("Tag", box i)|]
            (Products.defineExtractAndCreate
              t
              (Builder.emit
                (OpCodes.Call,
                 FSharpValue.PreComputeUnionConstructorInfo
                  (cases.[i], B.Public)))
              caseFields.[i]
              caseProducts.[i] >>= fun () ->
             Builder.forTo 0 (caseFields.[i].Length-1) <| fun j ->
              Builder.metaField
               (typedefof<Label<_,_,_,_>>.MakeGenericType
                 [|caseFields.[i].[j].PropertyType;
                   caseProducts.[i].[j];
                   choices.[i];
                   t|])
               [|("Index", box j)
                 ("Name", box caseFields.[i].[j].Name)|]
               (Builder.overrideGetMethod "Get" t caseFields.[i].[j])))))
    elif FSharpType.IsTuple t then
      let elems = FSharpType.GetTupleElements t
      if 7 < elems.Length then
        failwith "XXX: Tuples with more than 7 elements are not yet \
                  supported.  Pull requests are welcome!"

      let props =
        Array.init elems.Length <| fun i ->
        t.GetProperty (sprintf "Item%d" (i + 1))
      let products = Products.products elems

      Builder.metaType typeof<Rep.Tuple<'t>> [||]
       (Products.asPairsField t
         (Builder.emit
           (OpCodes.Newobj,
            match FSharpValue.PreComputeTupleConstructorInfo t with
             | (ctor, None) -> ctor
             | (_, Some _) -> failwith "XXX"))
         props
         products
         (Builder.forTo 0 (elems.Length-1) <| fun i ->
           let elemType =
             typedefof<Item<_, _, _>>.MakeGenericType
              [|elems.[i]; products.[i]; t|]
           Builder.metaField elemType [|("Index", box i)|]
            (Builder.overrideGetMethod "Get" t props.[i])))
    elif t.IsPrimitive then
      upcast Prim'<'t> ()
    else
      upcast Unsupported'<'t> ()

  static member Union (_: Rep<'t>, r: Union<'t>) = r
  static member Product (_: Rep<'t>, r: Product<'t>) = r
  static member Record (_: Rep<'t>, r: Record<'t>) = r
  static member Tuple (_: Rep<'t>, r: Rep.Tuple<'t>) = r
  static member Prim (_: Rep<'t>, r: Prim<'t>) = r
  static member Unsupported (_: Rep<'t>, r: Unsupported<'t>) = r

  static member AsChoices (_: Rep<'t>, c: AsChoices<'s, 't>) = c
  static member AsPairs (_: Rep<'t>, p: AsPairs<'p, 'o, 't>) = p
  static member AsPairs (_: AsChoices<'p, 't>, m: Case<'p, 'p, 't>) =
    m :> AsPairs<'p, 'p, 't>

  static member AsElem (i: Item<'e, 'r, 't>) = i :> Elem<'e, 'r, 't, 't>
  static member AsElem (l: Labelled<'e, 'r, 'o, 't>) = l :> Elem<'e,'r,'o,'t>
  static member AsLabelled (f: Field<'e, 'r, 't>) = f :> Labelled<'e,'r,'t,'t>
  static member AsLabelled (l: Label<'e, 'r, 'o, 't>) = l :> Labelled<'e, 'r, 'o, 't>
