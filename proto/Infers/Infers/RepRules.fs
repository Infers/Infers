#if FSHARP_NON_INTERACTIVE
namespace Infers.Rep

open Infers
open Infers.Engine
#endif

open Microsoft.FSharp.Reflection
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit
open System.Threading

/////////////////////////////////////////////////////////////////////////

[<AutoOpen>]
module Util =
  let repModule =
    let appDomain = AppDomain.CurrentDomain;
    let assemblyName = AssemblyName "RepAssembly"
    let assemblyBuilder =
      appDomain.DefineDynamicAssembly
       (assemblyName, AssemblyBuilderAccess.RunAndCollect)
    assemblyBuilder.DefineDynamicModule assemblyName.Name

  let mutable uniqueId = 0
  let inline uniqueName () =
    let i = uniqueId
    uniqueId <- i+1
    sprintf "Generated%d" i

  type Builder<'a> =
    TypeBuilder * list<string * obj> -> list<string * obj> * 'a

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
  static member result x : Builder<_> = fun (_, vs) -> (vs, x)

  static member getTypeBuilder : Builder<TypeBuilder> =
    fun (typeBuilder, values) ->
      (values, typeBuilder)

  static member metaType (baseType: Type) (builder: Builder<unit>) =
    let typeBuilder =
      repModule.DefineType
       (uniqueName (),
        TypeAttributes.Public,
        baseType)
    let (values, ()) = builder (typeBuilder, [])
    let metaType = typeBuilder.CreateType ()
    values
    |> List.iter (fun (name, value) ->
       let rec find (t: Type) =
         match t.GetField name with // Only public is ok here.
          | null ->
            if t <> typeof<obj> then
              find t.BaseType
            else
              failwithf "Type %A :> %A has no field named %s"
               metaType baseType name
          | field ->
            if field.IsStatic then
              field.SetValue (null, value)
            else
              failwithf "Field %s of type %A :> %A is not static"
               name metaType baseType
       find metaType)
    (metaType.GetConstructor [||]).Invoke [||]

  static member metaValue name (value: Object) : Builder<unit> =
    fun (typeBuilder, values) ->
      ((name, value)::values, ())

  static member metaField baseType (definition: Builder<unit>) : Builder<unit> =
    Builder.getTypeBuilder >>= fun typeBuilder ->
    let name = uniqueName ()
    typeBuilder.DefineField
     (name,
      baseType,
      FieldAttributes.Static ||| FieldAttributes.Public) |> ignore
    Builder.metaValue
     name
     (Builder.metaType baseType definition)

  static member emit op = fun (ilgen: ILGenerator) ->
    ilgen.Emit op ; ilgen

  static member emit (op, m: MethodInfo) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, m) ; ilgen

  static member emit (op, c: ConstructorInfo) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, c) ; ilgen

  static member emit (op, f: FieldInfo) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, f) ; ilgen

  static member emit (op, t: Type) = fun (ilgen: ILGenerator) ->
    ilgen.Emit (op, t) ; ilgen

  static member overrideMethod name resultType paramTypes code : Builder<unit> =
    Builder.getTypeBuilder >>= fun typeBuilder ->
    let methodBuilder =
      typeBuilder.DefineMethod
       (name,
        MethodAttributes.Public
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.Virtual,
        resultType,
        paramTypes)
    let ilgen = methodBuilder.GetILGenerator ()
    code ilgen |> ignore
    Builder.result ()

  static member overrideGetMethod name paramType (property: PropertyInfo) : Builder<unit> =
    Builder.overrideMethod name
     property.PropertyType
     [|paramType (*property.DeclaringType*)|]
     (Builder.emit (OpCodes.Ldarg_1) >>
      Builder.emit (OpCodes.Call, property.GetGetMethod ()) >>
      Builder.emit (OpCodes.Ret))

  static member overrideSetMethodWhenCanWrite name (property: PropertyInfo) : Builder<unit> =
    if property.CanWrite then
      Builder.overrideMethod name
       typeof<Void>
       [|property.DeclaringType; property.PropertyType|]
       (Builder.emit (OpCodes.Ldarg_1) >>
        Builder.emit (OpCodes.Ldarg_2) >>
        Builder.emit (OpCodes.Call, property.GetSetMethod ()) >>
        Builder.emit (OpCodes.Ret))
    else
      Builder.result ()

  static member forTo i0 i1 (on: int -> Builder<unit>) : Builder<unit> =
    if i0 <= i1 then
      on i0 >>= fun () ->
      Builder.forTo (i0+1) i1 on
    else
      Builder.result ()

/////////////////////////////////////////////////////////////////////////

module Products =

  let products ts = build typedefof<And<_, _>> ts

  let defineExtractAndCreate t emitCtor (props: array<PropertyInfo>) (products: array<Type>) =
    let getField (t: Type) name =
      match t.GetField name with // Only public is ok here.
       | null -> failwithf "The %A type has no field named \"%s\"" t name
       | field -> field
    let emitGet i =
      Builder.emit (OpCodes.Ldarg_1) >>
      Builder.emit (OpCodes.Call, props.[i].GetGetMethod true)
    let rec emitLoadAddr arg i =
      if i = 0 then
        Builder.emit arg
      else
        let loadPrev = emitLoadAddr arg (i-1)
        if i <> products.Length-1 then
          loadPrev >>
          Builder.emit (OpCodes.Ldflda, getField products.[i-1] "Rest")
        else
          loadPrev
    let emitStore i =
      if products.Length = 1 then
        Builder.emit (OpCodes.Stobj, products.[i])
      elif i < products.Length-1 then
        Builder.emit (OpCodes.Stfld, getField products.[i] "Elem")
      else
        Builder.emit (OpCodes.Stfld, getField products.[i-1] "Rest")
    let emitCopy i =
      emitLoadAddr OpCodes.Ldarg_2 i >> emitGet i >> emitStore i
    let rec emitCopies n =
      let copyThis = emitCopy (n-1)
      if n > 1 then emitCopies (n-1) >> copyThis else copyThis
    let emitLoad i =
      emitLoadAddr OpCodes.Ldarg_1 i >>
      if products.Length = 1 then
        Builder.emit (OpCodes.Ldobj, products.[i])
      elif i < products.Length-1 then
        Builder.emit (OpCodes.Ldfld, getField products.[i] "Elem")
      else
        Builder.emit (OpCodes.Ldfld, getField products.[i-1] "Rest")
    let rec emitLoads n =
      let loadThis = emitLoad (n-1)
      if n > 1 then emitLoads (n-1) >> loadThis else loadThis
    Builder.overrideMethod "Extract"
     typeof<Void>
     [|t; products.[0].MakeByRefType ()|]
     (if props.Length > 0 then
        emitCopies props.Length >>
        Builder.emit (OpCodes.Ret)
      else
        Builder.emit (OpCodes.Ret)) >>= fun () ->
    Builder.overrideMethod "Create"
     t
     [|products.[0].MakeByRefType ()|]
     (if props.Length > 0 then
        emitLoads props.Length >>
        emitCtor >>
        Builder.emit (OpCodes.Ret)
      else
        emitCtor >>
        Builder.emit (OpCodes.Ret))

  let asProductField t emitCtor (props: array<PropertyInfo>) (products: array<Type>) =
    Builder.metaField (typedefof<AsProduct<_, _>>.MakeGenericType [|t; products.[0]|])
     (defineExtractAndCreate t emitCtor props products)

/////////////////////////////////////////////////////////////////////////

module Unions =
  let choices ts = build typedefof<Choice<_, _>> ts

  let asChoiceField t (choices: array<Type>) =
    Builder.metaField (typedefof<AsChoice<_, _>>.MakeGenericType [|t; choices.[0]|])
     (Builder.result ())

/////////////////////////////////////////////////////////////////////////

type [<InferenceRules>] Rules () =
  member rr.record () : Record<'r> =
    if not (FSharpType.IsRecord (typeof<'r>, BindingFlags.Any)) then
      raise Backtrack

    lock repModule <| fun () ->

    let t = typeof<'r>
    let fields =
      FSharpType.GetRecordFields
       (t, BindingFlags.Public ||| BindingFlags.NonPublic)
    let products = Products.products (fields |> Array.map (fun p -> p.PropertyType))

    match
      Builder.metaType typeof<Record<'r>>
       (Builder.metaValue "Arity" fields.Length         >>= fun () ->
        Builder.metaValue "IsMutable"
         (fields |> Array.exists (fun p -> p.CanWrite)) >>= fun () ->
        Products.asProductField t
         (Builder.emit
           (OpCodes.Newobj,
            FSharpValue.PreComputeRecordConstructorInfo
             (t, BindingFlags.Public ||| BindingFlags.NonPublic)))
         fields
         products                >>= fun () ->
        Builder.forTo 0 (fields.Length-1) (fun i ->
          let fieldType =
            typedefof<Field<_, _, _>>.MakeGenericType
             [|t; fields.[i].PropertyType; products.[i]|]
          Builder.metaField fieldType
           (Builder.overrideGetMethod "Get" t fields.[i] >>= fun () ->
            Builder.overrideSetMethodWhenCanWrite "Set" fields.[i] >>= fun () ->
            Builder.metaValue "Index" i                    >>= fun () ->
            Builder.metaValue "Name" fields.[i].Name       >>= fun () ->
            Builder.metaValue "IsMutable" fields.[i].CanWrite))) with
     | :? Record<'r> as r ->
       r
     | _ -> failwith "Bug"

  member rr.union () : Union<'u> =
    if not (FSharpType.IsUnion (typeof<'u>, BindingFlags.Any)) then
      raise Backtrack

    lock repModule <| fun () ->
    let t = typeof<'u>

    let cases =
      FSharpType.GetUnionCases
       (t, BindingFlags.Public ||| BindingFlags.NonPublic)
    let caseFields =
      cases
      |> Array.map (fun case ->
         case.GetFields ())
    let caseProducts =
      caseFields
      |> Array.map (fun caseFields ->
         caseFields
         |> Array.map (fun prop -> prop.PropertyType)
         |> Products.products)
    let choices =
      caseProducts
      |> Array.map (fun ts -> ts.[0])
      |> Unions.choices

    match
      Builder.metaType typeof<Union<'u>>
       (Builder.metaValue "Arity" cases.Length >>= fun () ->
        (match FSharpValue.PreComputeUnionTagMemberInfo
                (t, BindingFlags.Public ||| BindingFlags.NonPublic) with
          | :? PropertyInfo as prop ->
            Builder.overrideGetMethod "Tag" t prop
          | :? MethodInfo as meth when meth.IsStatic ->
            Builder.overrideMethod "Tag" typeof<int> [|t|]
             (Builder.emit (OpCodes.Ldarg_1) >>
              Builder.emit (OpCodes.Call, meth) >>
              Builder.emit (OpCodes.Ret))
          | _ ->
            failwith "Expected PropertyInfo or static MethodInfo.") >>= fun () ->
        Unions.asChoiceField t choices >>= fun () ->
        Builder.forTo 0 (cases.Length-1) (fun i ->
          Builder.metaField
           (typedefof<Case<_, _, _>>.MakeGenericType
             [|t; caseProducts.[i].[0]; choices.[i]|])
           (Builder.metaValue "Name" cases.[i].Name         >>= fun () ->
            Builder.metaValue "Arity" caseFields.[i].Length >>= fun () ->
            Builder.metaValue "Tag" i                       >>= fun () ->
            Products.defineExtractAndCreate
             t
             (Builder.emit
               (OpCodes.Call,
                FSharpValue.PreComputeUnionConstructorInfo
                 (cases.[i],
                  BindingFlags.Public ||| BindingFlags.NonPublic)))
             caseFields.[i]
             caseProducts.[i]) >>= fun () ->
          Builder.forTo 0 (caseFields.[i].Length-1) (fun j ->
            Builder.metaField
             (typedefof<Label<_,_,_,_>>.MakeGenericType
               [|t;
                 choices.[i];
                 caseFields.[i].[j].PropertyType;
                 caseProducts.[i].[j]|])
             (Builder.metaValue "Name" caseFields.[i].[j].Name >>= fun () ->
              Builder.metaValue "Index" j >>= fun () ->
              Builder.overrideGetMethod "Get" t caseFields.[i].[j])))) with
     | :? Union<'u> as u ->
       u
     | _ -> failwith "Bug"

  member rr.tuple () : Rep.Tuple<'t> =
    if not (FSharpType.IsTuple typeof<'t>) then
      raise Backtrack

    lock repModule <| fun () ->

    let t = typeof<'t>

    let elems = FSharpType.GetTupleElements t
    if 7 < elems.Length then
      failwith "XXX: Tuples with more than 7 elements are not yet supported."

    let props =
      Array.init elems.Length <| fun i ->
      t.GetProperty (sprintf "Item%d" (i + 1))
    let products = Products.products elems

    match
      Builder.metaType typeof<Tuple<'t>>
       (Builder.metaValue "Arity" elems.Length >>= fun () ->
        Products.asProductField t
         (Builder.emit
           (OpCodes.Newobj,
            match FSharpValue.PreComputeTupleConstructorInfo t with
             | (ctor, None) -> ctor
             | (ctor, Some _) -> failwith "XXX"))
         props
         products >>= fun () ->
        Builder.forTo 0 (elems.Length-1) (fun i ->
         let elemType =
           typedefof<Elem<_, _, _>>.MakeGenericType
            [|t; elems.[i]; products.[i]|]
         Builder.metaField elemType
          (Builder.overrideGetMethod "Get" t props.[i] >>= fun () ->
           Builder.metaValue "Index" i))) with
     | :? Rep.Tuple<'t> as t ->
       t
     | _ -> failwith "Bug"
