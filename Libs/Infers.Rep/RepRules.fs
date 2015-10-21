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
  let repModule =
    let appDomain = AppDomain.CurrentDomain;
    let assemblyName = AssemblyName "RepAssembly"
    let assemblyBuilder =
      appDomain.DefineDynamicAssembly
       (assemblyName, AssemblyBuilderAccess.RunAndCollect)
    assemblyBuilder.DefineDynamicModule assemblyName.Name

  let mutable uniqueId = 0
  let inline uniqueName () =
    let i = uniqueId + 1
    uniqueId <- i
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
  static member result x : Builder<_> = fun (_, vs) -> (vs, x)

  static member getTypeBuilder : Builder<TypeBuilder> =
    fun (typeBuilder, values) ->
      (values, typeBuilder)

  static member metaType (baseType: Type)
                         (basePars: array<obj>)
                         (builder: Builder<unit>) =
    lock repModule <| fun () ->
    let typeBuilder = repModule.DefineType (uniqueName (), T.Public, baseType)
    let (values, ()) = builder (typeBuilder, [])
    let baseArgTypes =
      match baseType.GetConstructors (B.Instance|||B.Public|||B.NonPublic) with
       | [|baseCtor|] when baseCtor.GetParameters().Length = basePars.Length ->
         let baseArgTypes =
           baseCtor.GetParameters () |> Array.map (fun p -> p.ParameterType)
         let ctor =
           typeBuilder.DefineConstructor (M.Public, C.Standard, baseArgTypes)
         let ilGen = ctor.GetILGenerator ()
         for i=0 to basePars.Length do
           ilGen.Emit (OpCodes.Ldarg, i)
         ilGen.Emit (OpCodes.Call, baseCtor)
         ilGen.Emit (OpCodes.Ret)
         baseArgTypes
       | cs ->
         failwithf "Type %A used as metatype base must have exactly one ctor \
                    with %d params.  Got: %A"
          baseType basePars.Length cs
    values
    |> List.iter (fun (name, field, baseType, _) ->
       let meth = typeBuilder.DefineMethod (name, M.Public, baseType, [||])
       let ilGen = meth.GetILGenerator ()
       ilGen.Emit (OpCodes.Ldsfld, field)
       ilGen.Emit (OpCodes.Ret))
    let metaType = typeBuilder.CreateType ()
    values
    |> List.iter (fun (name, _, _, value) ->
       let rec find (t: Type) =
         match t.GetField name with // Only public is ok here.
          | null ->
            if t = typeof<obj> then
              failwithf "Type %A :> %A has no field named %s"
               metaType baseType name
            find t.BaseType
          | field ->
            if not field.IsStatic then
              failwithf "Field %s of type %A :> %A is not static"
               name metaType baseType
            field.SetValue (null, value)
       find metaType)
    let ctor = metaType.GetConstructor baseArgTypes
    ctor.Invoke basePars

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
    let emitGet i =
      match props.[i].GetGetMethod () with
       | null ->
         failwithf "Failed to get Get-method for property \"%s\" of \"%s\""
          props.[i].Name props.[i].DeclaringType.Name
       | getMethod ->
         Builder.emit (OpCodes.Ldarg_1) >>
         Builder.emit (OpCodes.Call, getMethod)
    let rec emitLoadAddr arg i =
      if i = 0
      then Builder.emit arg
      else emitLoadAddr arg (i-1)
           >> if i <> products.Length-1
              then Builder.emit (OpCodes.Ldflda, getField products.[i-1] "Rest")
              else id
    let emitLoadOrStore objOp fldOp i =
      if products.Length = 1
      then Builder.emit (objOp, products.[i])
      else Builder.emit (fldOp, if i < products.Length-1
                                then getField products.[i] "Elem"
                                else getField products.[i-1] "Rest")
    let emitStore = emitLoadOrStore OpCodes.Stobj OpCodes.Stfld
    let emitCopy i =
      emitLoadAddr OpCodes.Ldarg_2 i >> emitGet i >> emitStore i
    let rec emitCopies n =
      if n > 1 then emitCopies (n-1) else id
      >> emitCopy (n-1)
    let emitLoad i =
      emitLoadAddr OpCodes.Ldarg_1 i >>
      emitLoadOrStore OpCodes.Ldobj OpCodes.Ldfld i
    let rec emitLoads n =
      if n > 1 then emitLoads (n-1) else id
      >> emitLoad (n-1)
    Builder.overrideMethod "Extract"
     typeof<Void>
     [|t; products.[0].MakeByRefType ()|]
     (if props.Length > 0 then emitCopies props.Length else id
      >> Builder.emit (OpCodes.Ret)) >>= fun () ->
    Builder.overrideMethod "Create"
     t
     [|products.[0].MakeByRefType ()|]
     (if props.Length > 0 then emitLoads props.Length else id
      >> emitCtor
      >> Builder.emit (OpCodes.Ret))

  let asPairsField (t: Type)
                   (emitCtor: ILGenerator -> ILGenerator)
                   (props: array<PropertyInfo>)
                   (products: array<Type>)
                   (defineRest: Builder<unit>) =
    Builder.metaField
     (typedefof<AsPairs<_, _, _>>.MakeGenericType [|products.[0]; t; t|])
     [|box props.Length
       box (props |> Array.exists (fun p -> p.CanWrite))|]
     (defineExtractAndCreate t emitCtor props products >>= fun () ->
      defineRest)

////////////////////////////////////////////////////////////////////////////////

module Unions =
  let choices ts = build typedefof<Choice<_, _>> ts

  let asChoicesField (t: Type) (choices: array<Type>) defineRest =
    Builder.metaField
     (typedefof<AsChoices<_, _>>.MakeGenericType [|choices.[0]; t|])
     [|box choices.Length|]
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

type [<InferenceRules>] Rep () =
  member this.rep () : Rep<'t> = StaticMap<Rep>.Memoize <| fun () ->
    let t = typeof<'t>

    match
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
               [|box i; fields.[i].Name; box fields.[i].CanWrite|]
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
              [|box cases.[i].Name; box caseFields.[i].Length; box i|]
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
                 [|box j; box caseFields.[i].[j].Name|]
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
             Builder.metaField elemType [|box i|]
              (Builder.overrideGetMethod "Get" t props.[i])))
      elif t.IsPrimitive then
        Builder.metaType typeof<Prim<'t>> [||]
         (Builder.result ())
      else
        Builder.metaType typeof<Unsupported<'t>> [||]
         (Builder.result ()) with
    | :? Rep<'t> as rep ->
      rep
    | _ ->
      failwith "Bug"

  member this.union (_: Rep<'t>, r: Union<'t>) = r
  member this.product (_: Rep<'t>, r: Product<'t>) = r
  member this.record (_: Rep<'t>, r: Record<'t>) = r
  member this.tuple (_: Rep<'t>, r: Rep.Tuple<'t>) = r
  member this.prim (_: Rep<'t>, r: Prim<'t>) = r

  member this.asChoices (_: Rep<'t>, c: AsChoices<'s, 't>) = c
  member this.asPairs (_: Rep<'t>, p: AsPairs<'p, 'o, 't>) = p
  member this.viewAsPairs (_: AsChoices<'p, 't>, m: Case<'p, 'p, 't>) =
    m :> AsPairs<'p, 'p, 't>

  member this.asElem (_: Rep.Tuple<'t>, i: Item<'e, 'r, 't>) =
    i :> Elem<'e, 'r, 't, 't>
  member this.asElem (l: Labelled<'e, 'r, 'o, 't>) = l :> Elem<'e, 'r, 'o, 't>
  member this.asLabelled (f: Field<'e, 'r, 't>) = f :> Labelled<'e, 'r, 't, 't>
  member this.asLabelled (_: Union<'t>, l: Label<'e, 'r, 'o, 't>) =
    l :> Labelled<'e, 'r, 'o, 't>
