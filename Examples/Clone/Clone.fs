// Inspired by
//   https://gist.github.com/t0yv0/649a10818119636c0952
//   http://fssnip.net/n1

module Main

open Infers
open Infers.Rep

////////////////////////////////////////////////////////////////////////////////
 
/// Type of cloning functions.
type Clone<'x> = 'x -> 'x

/// Type of an intermediate step in cloning a product type.
type [<AbstractClass>] CloneProduct<'e, 'es, 't> () =
  abstract Clone: byref<'e> -> unit

/// Type of a partial union cloning function (or list of such functions).
type CloneUnion<'c, 'cs, 't> = CloneUnion of list<Clone<'t>>

////////////////////////////////////////////////////////////////////////////////
 
/// A helper function for products (including individual union cases).
let inline CloneProductToClone (asProduct: AsProduct<'es, 't>)
                               (cloneProduct: CloneProduct<'es, 'es, 't>) : Clone<'t> =
  fun original ->
    // First we allocate space for elements of the clone from stack.
    let mutable clone = Unchecked.defaultof<'es>
    // We then extract the original elements of the product.
    asProduct.Extract (original, &clone)
    // Then we clone the elements in-place.
    cloneProduct.Clone (&clone)
    // Finally we create a clone from the cloned elements.
    asProduct.Create (&clone)

/// Inference rules for creating cloning functions.
type [<InferenceRules>] Clone () =
  // Recursion rule ------------------------------------------------------------

  // Rule for creating a proxy when defining a recursive cloning function.
  member this.Fix (recFn: RecFn) : Rec<Clone<'t>> =
    recFn.func ()

  // Rules for simple base types -----------------------------------------------

  /// A rule for the `int32` type.
  member this.Int : Clone<int> = fun x -> x

  // A rule for the `string` type.
  member this.String : Clone<string> = fun x -> x

  // Rules for some specific type constructors ---------------------------------

  // A rule for the `ref<'t>` type constructor.
  member this.Ref (cloneT: Clone<'t>) : Clone<ref<'t>> =
    fun r -> ref (cloneT (!r))

  // A rule for the `array<'t>` type constructor.
  member this.Array (cloneT: Clone<'t>) : Clone<array<'t>> =
    Array.map cloneT

  // Rules for union types -----------------------------------------------------

  /// A rule for cloning an arbitrary union type.
  member this.Union (_: Rep,
                     union: Union<'t>,
                     asChoice: AsChoice<'cs, 't>,
                     CloneUnion cloneUnion: CloneUnion<'cs, 'cs, 't>) =
    // First we stage an array for fast indexing of cases.  It is important that
    // we do this outside of the cloning function that we actually return.
    let cases = Array.ofList cloneUnion
    // The cloning function.
    fun original ->
      // First we extract the case index.
      let caseIndex = union.Tag original
      // And then we invoke the case specific cloning function.
      cases.[caseIndex] original

  /// A rule cloning a partial set of union cases.
  ///
  /// Note that the type for the result is not the type that would be inferred
  /// by F#, so specifying the result type here is necessary.
  member this.Plus (CloneUnion cloneUnionCase:  CloneUnion<       'c,       Choice<'c, 'cs>, 't>,
                    CloneUnion cloneUnionCases: CloneUnion<           'cs,             'cs , 't>)
                                              : CloneUnion<Choice<'c, 'cs>, Choice<'c, 'cs>, 't> =
   // We just concatenate the individual case `cloneUnionCase` (of length 1) and
   // the other cases `cloneUnionCases` together.
   CloneUnion (cloneUnionCase @ cloneUnionCases)

  /// A rule for cloning a non-nullary union case.
  member this.Case (case: Case<'ls, 'cs, 't>,
                    cloneProduct: CloneProduct<'ls, 'ls, 't>) : CloneUnion<'ls, 'cs, 't> =
    // We forward here to a helper function to avoid duplicating code.
    let cloneCase : Clone<'t> =
      CloneProductToClone (case :> AsProduct<'ls, 't>) cloneProduct
    // Then we return just this case. 
    CloneUnion [cloneCase]

  /// A rule for cloning a nullary union case.
  member this.Case (case: Case<Empty, 'cs, 't>) : CloneUnion<Empty, 'cs, 't> =
    // There is nothing to clone in the empty case.
    let cloneCase : Clone<'t> = fun x -> x
    // Then we return just this case. 
    CloneUnion [cloneCase]

  // Rules for product types ---------------------------------------------------

  /// A rule for cloning an arbitrary product (tuple, record or union case).
  member this.Product (_: Rep,
                       _: Product<'t>,
                       asProduct: AsProduct<'es, 't>,
                       cloneProduct: CloneProduct<'es, 'es, 't>) : Clone<'t> =
    // We forward here to a helper function to avoid duplicating this case.
    CloneProductToClone asProduct cloneProduct

  /// A rule for cloning a part of a product consisting of an `Elem: 'e` and
  /// `Rest: 'es`.
  member this.Times (cloneProductElem: CloneProduct< 'e, And<'e, 'es>, 't>,
                     cloneProductRest: CloneProduct<'es,         'es , 't>) =
    {new CloneProduct<And<'e, 'es>, And<'e, 'es>, 't> () with
      override this.Clone (ees: byref<And<'e, 'es>>) =
       // First we clone the elem.  This happens in the stack allocated space.
       cloneProductElem.Clone (&ees.Elem)
       // And then the rest.  This also happens in the stack allocated space.
       cloneProductRest.Clone (&ees.Rest)}

  /// A rule for cloning a specific element of type `'e` within a product.
  member this.Elem (_: Rep, _: Elem<'e, 'es, 't>, cloneElem: Clone<'e>) =
    {new CloneProduct<'e, 'es, 't> () with
      override this.Clone (e: byref<'e>) =
       // We just mutate the element in-place within the stack allocated space.
       e <- cloneElem e}

////////////////////////////////////////////////////////////////////////////////
 
type SimpleRecord = { Name: string; Age: option<int> }
type LotsOfRecords = { People: SimpleRecord [] }
 
[<EntryPoint>]
let Start args =
  let sr = { People = [| {Name = "Rick"; Age = Some 33 }; { Name = "Paul"; Age = None }|] }
  let func = Infers.Engine.TryGenerate<Clone<LotsOfRecords>> (Clone ())
  match func with
   | Some clone ->
     let src = clone sr
     if (sr = src) && not (System.Object.ReferenceEquals(sr, src))
     then printfn "OK"
     else printfn "FAIL"
   | _ ->
     printfn "NO MATCH"
  0
