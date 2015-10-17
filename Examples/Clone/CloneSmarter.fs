// Copyright (C) by Vesa Karvonen

module CloneSmarter

open System.Collections.Generic
open Infers
open Infers.Rep

////////////////////////////////////////////////////////////////////////////////
 
/// Type of cloning functions.
type Clone<'t> = 't -> 't

/// A smart cloning function.
type [<AllowNullLiteral; AbstractClass>] CloneSmarter<'t> () =
  abstract Clone: 't -> 't

/// Type of an intermediate step in cloning a product type.
type [<AllowNullLiteral; AbstractClass>] CloneP<'e, 'r, 'o, 't> () =
  abstract Clone: byref<'e> -> unit
  abstract ForAll: (obj -> bool) -> bool

/// Type of a partial sum cloning function (or list of such functions).
type CloneS<'p, 'o, 't> = CloneS of list<CloneSmarter<'t>>

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
  member t.Int : CloneSmarter<int> = null // Nothing to copy!

  /// A rule for the `string` type.
  member t.String : CloneSmarter<string> = null // Nothing to copy!

  // Rules for some specific type constructors ---------------------------------

  /// A rule for the `ref<'t>` type constructor.
  member t.Ref (cloneX': CloneSmarter<'t>) =
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

  // Rules for product types ---------------------------------------------------

  /// A rule for cloning an arbitrary product (tuple, record or union case).
  member t.Product (asPairs: AsPairs<'p, 'o, 't>,
                    cloneProduct: CloneP<'p, 'p, 'o, 't>) =
    // Do we need to copy something?
    match (asPairs.IsMutable, cloneProduct) with
     | (false, null) ->
       null
     | (isMutable, cloneProduct) ->
       match cloneProduct with
        | null ->
          null
        | cloneProduct ->
          if not isMutable &&
             cloneProduct.ForAll
              (function :? CloneSmarter<'t> -> true | _ -> false) then
            null
          else
            {new CloneSmarter<'t> () with
              override t.Clone (original) =
               // First we allocate space for elements of the clone from stack.
               let mutable elems = Unchecked.defaultof<'p>
               // We then extract the original elements of the product.
               asPairs.Extract (original, &elems)
               // Then we clone the elements in-place.
               cloneProduct.Clone (&elems)
               // Then we create a clone from the cloned elements.
               asPairs.Create (&elems)}

  /// A rule for cloning a part of a product consisting of an `Elem: 'e` and
  /// `Rest: 'es`.
  member t.Pair (cloneE: CloneP<'e, Pair<'e, 'r>, 'o, 't>,
                 cloneR: CloneP<'r,          'r , 'o, 't>) =
    // What do we need to clone?
    match (cloneE, cloneR) with
     | (null, null) ->
       null
     | (null, cloneR) ->
       {new CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> () with
         override t.Clone (er: byref<Pair<'e, 'r>>) =
          cloneR.Clone (&er.Rest)
         override t.ForAll (pred) =
          cloneR.ForAll pred}
     | (cloneE, null) ->
       {new CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> () with
         override t.Clone (er: byref<Pair<'e, 'r>>) =
          cloneE.Clone (&er.Elem)
         override t.ForAll (pred) =
          cloneE.ForAll pred}
     | (cloneE, cloneR) ->
       {new CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> () with
         override t.Clone (er: byref<Pair<'e, 'r>>) =
          cloneE.Clone (&er.Elem)
          cloneR.Clone (&er.Rest)
         override t.ForAll (pred) =
          cloneE.ForAll pred &&
          cloneR.ForAll pred}

  /// A rule for cloning a specific element of type `'e` within a product.
  member t.Elem (_: Elem<'e, 'r, 'o, 't>, cloneElem: CloneSmarter<'e>) =
    match cloneElem with
     | null ->
       null
     | cloneElem ->
       {new CloneP<'e, 'r, 'o, 't> () with
         override t.Clone (e: byref<'e>) =
          // We mutate the element in-place within the stack allocated space.
          e <- cloneElem.Clone (e)
         override t.ForAll (pred) =
          pred cloneElem}

  // Rules for sum types -------------------------------------------------------

  /// A rule for cloning an arbitrary sum type.
  member t.Sum (asChoices: AsChoices<'s, 't>,
                CloneS cloneSum: CloneS<'s, 's, 't>) =
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
          let caseIndex = asChoices.Tag original
          // And then we invoke the case specific cloning function.
          cloneSum.[caseIndex].Clone (original)}

  /// A rule cloning a partial set of union cases.
  ///
  /// Note that the type for the result is not the type that would be inferred
  /// by F#, so specifying the result type here is necessary.
  member t.Choice (CloneS cloneP: CloneS<       'p     , Choice<'p, 'o>, 't>,
                   CloneS cloneO: CloneS<           'o ,            'o , 't>) =
   CloneS (cloneP @ cloneO)     : CloneS<Choice<'p, 'o>, Choice<'p, 'o>, 't>
   // We just concatenate the individual case (of length 1) and the other cases
   // together.

  /// A rule for cloning a non-nullary union case.
  member t.Case (case: Case<'p, 'o, 't>, cloneProduct: CloneP<'p, 'p, 'o, 't>) =
    // We forward to the product rule to avoid duplicating code.
    let cloneCase = t.Product (case, cloneProduct)
    // Then we return just this case. 
    CloneS [cloneCase] : CloneS<'p, 'o, 't>

  /// A rule for cloning a nullary union case.
  member t.Case (_: Case<Empty, 'cs, 't>) : CloneS<Empty, 'cs, 't> =
    // There is nothing to copy.
    CloneS [null]
