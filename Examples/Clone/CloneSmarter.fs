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
type [<AllowNullLiteral; AbstractClass>] CloneP<'e, 'es, 't> () =
  abstract Clone: byref<'e> -> unit
  abstract ForAll: (obj -> bool) -> bool

/// Type of a partial sum cloning function (or list of such functions).
type CloneS<'c, 'cs, 't> = CloneS of list<CloneSmarter<'t>>

////////////////////////////////////////////////////////////////////////////////
 
/// A helper function for products (including individual union cases).
let toCloneSmart (isMutable: bool)
                 (asProduct: AsProduct<'es, 't>)
                 (cloneProduct': CloneP<'es, 'es, 't>) : CloneSmarter<'t> =
  match cloneProduct' with
   | null ->
     null
   | cloneProduct ->
     if not isMutable &&
        cloneProduct.ForAll (function :? CloneSmarter<'t> -> true | _ -> false) then
       null
     else
       {new CloneSmarter<'t> () with
         override t.Clone (original) =
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

  member t.AsClone (_: Rep, clone: CloneSmarter<'x>) =
    match clone with
     | null  -> fun x -> x
     | clone -> fun x -> clone.Clone (x)

  // Recursion rule ------------------------------------------------------------

  // Rule for creating a proxy when defining a recursive cloning function.
  member t.Fix () =
    let proxy: ref<CloneSmarter<'t>> = ref null
    {new Rec<CloneSmarter<'t>> () with
      override t.Get () =
        {new CloneSmarter<'t> () with
          override t.Clone (x) =
            match !proxy with
             | null -> x
             | clone -> clone.Clone (x)}
      override t.Set (clone) = proxy := clone}

  // Rules for simple base types -----------------------------------------------

  /// A rule for the `int32` type.
  member t.Int : CloneSmarter<int> = null  // Nothing to copy!

  /// A rule for the `string` type.
  member t.String : CloneSmarter<string> = null // Nothing to copy!

  // Rules for some specific type constructors ---------------------------------

  /// A rule for the `ref<'t>` type constructor.
  member t.Ref (cloneX': CloneSmarter<'t>) : CloneSmarter<ref<'t>> =
    match cloneX' with
     | null ->
       {new CloneSmarter<ref<'t>> () with
         override t.Clone (original) =
          ref (!original)}
     | cloneX ->
       {new CloneSmarter<ref<'t>> () with
         override t.Clone (original) =
          ref (cloneX.Clone (!original))}

  /// A rule for the `array<'t>` type constructor.
  member t.Array (cloneX': CloneSmarter<'x>) =
    match cloneX' with
     | null ->
       {new CloneSmarter<array<'x>> () with
         override t.Clone (original) =
          Array.copy original}
     | cloneX ->
       {new CloneSmarter<array<'x>> () with
         override t.Clone (original) =
          Array.map (fun x -> cloneX.Clone (x)) original}

  // Rules for sum types -------------------------------------------------------

  /// A rule for cloning an arbitrary sum type.
  member t.Sum (union: Union<'t>,
                _: AsSum<'cs, 't>,
                CloneS cloneSum: CloneS<'cs, 'cs, 't>) =
    /// Is there something to copy?
    if List.forall ((=) null) cloneSum then
      null
    else
      let identity =
        {new CloneSmarter<'t> () with 
          override t.Clone (original) = original}
      let cloneSum =
        Array.ofList cloneSum
        |> Array.map (fun case ->
           match case with
            | null -> identity
            | case -> case)
      {new CloneSmarter<'t> () with
        override t.Clone (original) =
          // First we extract the case index.
          let caseIndex = union.Tag original
          // And then we invoke the case specific cloning function.
          cloneSum.[caseIndex].Clone (original)}

  /// A rule cloning a partial set of union cases.
  ///
  /// Note that the type for the result is not the type that would be inferred
  /// by F#, so specifying the result type here is necessary.
  member t.Choice (CloneS cloneC: CloneS<       'c     , Choice<'c, 'o>, 't>,
                   CloneS cloneO: CloneS<           'o ,            'o , 't>)
                                : CloneS<Choice<'c, 'o>, Choice<'c, 'o>, 't> =
   // We just concatenate the individual case (of length 1) and the other cases
   // together.
   CloneS (cloneC @ cloneO)

  /// A rule for cloning a non-nullary union case.
  member t.Case (case: Case<'ls, 'cs, 't>,
                 cloneProduct': CloneP<'ls, 'ls, 't>) : CloneS<'ls, 'cs, 't> =
    // We forward here to a helper function to avoid duplicating code.
    let cloneCase = toCloneSmart false case cloneProduct'
    // Then we return just this case. 
    CloneS [cloneCase]

  /// A rule for cloning a nullary union case.
  member t.Case (_: Case<Empty, 'cs, 't>) : CloneS<Empty, 'cs, 't> =
    // There is nothing to copy.
    CloneS [null]

  // Rules for product types ---------------------------------------------------

  /// A rule for cloning an arbitrary product (tuple, record or union case).
  member t.Product (product: Product<'t>,
                    asProduct: AsProduct<'es, 't>,
                    cloneProduct': CloneP<'es, 'es, 't>) : CloneSmarter<'t> =
    // Do we need to copy something?
    match (product.IsMutable, cloneProduct') with
     | (false, null) ->
       null
     | (isMutable, cloneProduct) ->
       toCloneSmart isMutable asProduct cloneProduct

  /// A rule for cloning a part of a product consisting of an `Elem: 'e` and
  /// `Rest: 'es`.
  member t.Pair (cloneE': CloneP<'e, Pair<'e, 'r>, 't>,
                 cloneR': CloneP<'r,          'r , 't>) =
    // What do we need to clone?
    match (cloneE', cloneR') with
     | (null, null) ->
       null
     | (null, cloneR) ->
       {new CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 't> () with
         override t.Clone (er: byref<Pair<'e, 'r>>) =
          cloneR.Clone (&er.Rest)
         override t.ForAll (pred) =
          cloneR.ForAll pred}
     | (cloneE, null) ->
       {new CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 't> () with
         override t.Clone (er: byref<Pair<'e, 'r>>) =
          cloneE.Clone (&er.Elem)
         override t.ForAll (pred) =
          cloneE.ForAll pred}
     | (cloneE, cloneR) ->
       {new CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 't> () with
         override t.Clone (er: byref<Pair<'e, 'r>>) =
          cloneE.Clone (&er.Elem)
          cloneR.Clone (&er.Rest)
         override t.ForAll (pred) =
          cloneE.ForAll pred &&
          cloneR.ForAll pred}

  /// A rule for cloning a specific element of type `'e` within a product.
  member t.Elem (_: Elem<'e, 'es, _, 't>, cloneElem': CloneSmarter<'e>) =
    match cloneElem' with
     | null ->
       null
     | cloneElem ->
       {new CloneP<'e, 'es, 't> () with
         override t.Clone (e: byref<'e>) =
          // We just mutate the element in-place within the stack allocated space.
          e <- cloneElem.Clone (e)
         override t.ForAll (pred) =
          pred cloneElem}
