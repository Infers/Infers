// Copyright (C) by Vesa Karvonen

module CloneSmarter

open System.Collections.Generic
open Infers
open Infers.Rep

////////////////////////////////////////////////////////////////////////////////
 
/// Type of cloning functions.
type Clone<'x> = 'x -> 'x

/// A smart cloning function.
type [<AllowNullLiteral; AbstractClass>] CloneSmarter<'x> () =
  abstract Clone: 'x -> 'x

/// Type of an intermediate step in cloning a product type.
type [<AllowNullLiteral; AbstractClass>] CloneProduct<'e, 'es, 't> () =
  abstract Clone: byref<'e> -> unit
  abstract ForAll: (obj -> bool) -> bool

/// Type of a partial union cloning function (or list of such functions).
type CloneUnion<'c, 'cs, 't> = CloneUnion of list<CloneSmarter<'t>>

////////////////////////////////////////////////////////////////////////////////
 
/// A helper function for products (including individual union cases).
let CloneProductToCloneSmart (isMutable: bool)
                             (asProduct: AsProduct<'es, 't>)
                             (cloneProduct': CloneProduct<'es, 'es, 't>) : CloneSmarter<'t> =
  match cloneProduct' with
   | null ->
     null
   | cloneProduct ->
     if not isMutable &&
        cloneProduct.ForAll (function :? CloneSmarter<'t> -> true | _ -> false) then
       null
     else
       {new CloneSmarter<'t> () with
         override this.Clone (original) =
          // First we allocate space for elements of the clone from stack.
          let mutable elems = Unchecked.defaultof<'es>
          // We then extract the original elements of the product.
          asProduct.Extract (original, &elems)
          // Then we clone the elements in-place.
          cloneProduct.Clone (&elems)
          // Then we create a clone from the cloned elements.
          asProduct.Create (&elems)}

////////////////////////////////////////////////////////////////////////////////

/// Inference rules for creating cloning functions.
type [<InferenceRules>] Clone () =
  // Turn a smart clone to a simple clone --------------------------------------

  member this.AsClone (_: Rep, clone: CloneSmarter<'x>) : Clone<'x> =
    match clone with
     | null  -> fun x -> x
     | clone -> fun x -> clone.Clone (x)

  // Recursion rule ------------------------------------------------------------

  // Rule for creating a proxy when defining a recursive cloning function.
  member this.Fix () : Rec<CloneSmarter<'t>> =
    let proxy : ref<CloneSmarter<'t>> = ref null
    let clone =
      {new CloneSmarter<'t> () with
        override this.Clone (x) =
         match !proxy with
          | null -> x
          | clone -> clone.Clone (x)}
    {new Rec<CloneSmarter<'t>> () with
      override this.Get () = clone
      override this.Set (clone) = proxy := clone}

  // Rules for simple base types -----------------------------------------------

  /// A rule for the `int32` type.
  member this.Int : CloneSmarter<int> = null  // Nothing to copy!

  /// A rule for the `string` type.
  member this.String : CloneSmarter<string> = null // Nothing to copy!

  // Rules for some specific type constructors ---------------------------------

  /// A rule for the `ref<'t>` type constructor.
  member this.Ref (cloneX': CloneSmarter<'t>) : CloneSmarter<ref<'t>> =
    match cloneX' with
     | null ->
       {new CloneSmarter<ref<'t>> () with
         override this.Clone (original) =
          ref (!original)}
     | cloneX ->
       {new CloneSmarter<ref<'t>> () with
         override this.Clone (original) =
          ref (cloneX.Clone (!original))}

  /// A rule for the `array<'t>` type constructor.
  member this.Array (cloneX': CloneSmarter<'x>) : CloneSmarter<array<'x>> =
    match cloneX' with
     | null ->
       {new CloneSmarter<array<'x>> () with
         override this.Clone (original) =
          Array.copy original}
     | cloneX ->
       {new CloneSmarter<array<'x>> () with
         override this.Clone (original) =
          Array.map (fun x -> cloneX.Clone (x)) original}

  // Rules for union types -----------------------------------------------------

  /// A rule for cloning an arbitrary union type.
  member this.Union (union: Union<'t>,
                     _: AsChoice<'cs, 't>,
                     CloneUnion cloneUnion: CloneUnion<'cs, 'cs, 't>) =
    /// Is there something to copy?
    if List.forall ((=) null) cloneUnion then
      null
    else
      let identity =
        {new CloneSmarter<'t> () with 
          override this.Clone (original) = original}
      let cloneUnion =
        Array.ofList cloneUnion
        |> Array.map (fun case ->
           match case with
            | null -> identity
            | case -> case)
      {new CloneSmarter<'t> () with
        override this.Clone (original) =
          // First we extract the case index.
          let caseIndex = union.Tag original
          // And then we invoke the case specific cloning function.
          cloneUnion.[caseIndex].Clone (original)}

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
                    cloneProduct': CloneProduct<'ls, 'ls, 't>) : CloneUnion<'ls, 'cs, 't> =
    // We forward here to a helper function to avoid duplicating code.
    let cloneCase =
      CloneProductToCloneSmart false (case :> AsProduct<'ls, 't>) cloneProduct'
    // Then we return just this case. 
    CloneUnion [cloneCase]

  /// A rule for cloning a nullary union case.
  member this.Case (_: Case<Empty, 'cs, 't>) : CloneUnion<Empty, 'cs, 't> =
    // Then we return just this case.  There is nothing to copy.
    CloneUnion [null]

  // Rules for product types ---------------------------------------------------

  /// A rule for cloning an arbitrary product (tuple, record or union case).
  member this.Product (product: Product<'t>,
                       asProduct: AsProduct<'es, 't>,
                       cloneProduct': CloneProduct<'es, 'es, 't>) : CloneSmarter<'t> =
    // Do we need to copy something?
    match (product.IsMutable, cloneProduct') with
     | (false, null) ->
       null
     | (isMutable, cloneProduct) ->
       CloneProductToCloneSmart isMutable asProduct cloneProduct

  /// A rule for cloning a part of a product consisting of an `Elem: 'e` and
  /// `Rest: 'es`.
  member this.Times (cloneProductElem': CloneProduct< 'e, And<'e, 'es>, 't>,
                     cloneProductRest': CloneProduct<'es,         'es , 't>) =
    // What do we need to clone?
    match (cloneProductElem', cloneProductRest') with
     | (null, null) ->
       null
     | (null, cloneProductRest) ->
       {new CloneProduct<And<'e, 'es>, And<'e, 'es>, 't> () with
         override this.Clone (ees: byref<And<'e, 'es>>) =
          cloneProductRest.Clone (&ees.Rest)
         override this.ForAll (pred) =
          cloneProductRest.ForAll pred}
     | (cloneProductElem, null) ->
       {new CloneProduct<And<'e, 'es>, And<'e, 'es>, 't> () with
         override this.Clone (ees: byref<And<'e, 'es>>) =
          cloneProductElem.Clone (&ees.Elem)
         override this.ForAll (pred) =
          cloneProductElem.ForAll pred}
     | (cloneProductElem, cloneProductRest) ->
       {new CloneProduct<And<'e, 'es>, And<'e, 'es>, 't> () with
         override this.Clone (ees: byref<And<'e, 'es>>) =
          cloneProductElem.Clone (&ees.Elem)
          cloneProductRest.Clone (&ees.Rest)
         override this.ForAll (pred) =
          cloneProductElem.ForAll pred &&
          cloneProductRest.ForAll pred}

  /// A rule for cloning a specific element of type `'e` within a product.
  member this.Elem (_: Elem<'e, 'es, 't>, cloneElem': CloneSmarter<'e>) =
    match cloneElem' with
     | null ->
       null
     | cloneElem ->
       {new CloneProduct<'e, 'es, 't> () with
         override this.Clone (e: byref<'e>) =
          // We just mutate the element in-place within the stack allocated space.
          e <- cloneElem.Clone (e)
         override this.ForAll (pred) =
          pred cloneElem}
