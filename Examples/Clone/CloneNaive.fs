// Copyright (C) by Vesa Karvonen

module CloneNaive

open Infers
open Infers.Rep

////////////////////////////////////////////////////////////////////////////////
 
/// Type of cloning functions.
type Clone<'x> = 'x -> 'x

/// Type of an intermediate step in cloning a product type.
type [<AbstractClass>] CloneP<'e, 'r, 'o, 't> () =
  abstract Clone: byref<'e> -> unit

/// Type of a partial sum cloning function (or list of such functions).
type CloneS<'p, 'o, 't> = CloneS of list<Clone<'t>>

////////////////////////////////////////////////////////////////////////////////

/// Inference rules for creating cloning functions.
type [<InferenceRules>] Clone () =
  inherit Rep ()

  // Recursion rule ------------------------------------------------------------

  // Rule for creating a proxy when defining a recursive cloning function.
  member this.Fix (recFn: RecFn) : Rec<Clone<'t>> =
    recFn.func ()

  // Rules for simple base types -----------------------------------------------

  /// A rule for the `int32` type.
  member this.Int : Clone<int> = fun x -> x

  /// A rule for the `string` type.
  member this.String : Clone<string> = fun x -> x

  // Rules for some specific type constructors ---------------------------------

  /// A rule for the `ref<'t>` type constructor.
  member this.Ref (cloneT: Clone<'t>) : Clone<ref<'t>> =
    fun r -> ref (cloneT (!r))

  /// A rule for the `array<'t>` type constructor.
  member this.Array (cloneT: Clone<'t>) : Clone<array<'t>> =
    Array.map cloneT

  // Rules for product types ---------------------------------------------------

  /// A rule for cloning an arbitrary product (tuple, record or union case).
  member this.Product (asProduct: AsProduct<'p, 'o, 't>,
                       cloneP: CloneP<'p, 'p, 'o, 't>) : Clone<'t> =
    fun original ->
      // First we allocate space for elements of the clone from stack.
      let mutable clone = Unchecked.defaultof<'p>
      // We then extract the original elements of the product.
      asProduct.Extract (original, &clone)
      // Then we clone the elements in-place.
      cloneP.Clone (&clone)
      // Finally we create a clone from the cloned elements.
      asProduct.Create (&clone)

  /// A rule for cloning a part of a product consisting of an `Elem: 'e` and
  /// `Rest: 'es`.
  member this.Pair (cloneElem: CloneP<'e, Pair<'e, 'r>, 'o, 't>,
                    cloneRest: CloneP<'r,          'r , 'o, 't>) =
    {new CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> () with
      override this.Clone (ees: byref<Pair<'e, 'r>>) =
       // First we clone the elem.  This happens in the stack allocated space.
       cloneElem.Clone (&ees.Elem)
       // And then the rest.  This also happens in the stack allocated space.
       cloneRest.Clone (&ees.Rest)}

  /// A rule for cloning a specific element of type `'e` within a product.
  member this.Elem (_: Rep, _: Elem<'e, 'r, 'o, 't>, cloneElem: Clone<'e>) =
    {new CloneP<'e, 'r, 'o, 't> () with
      override this.Clone (e: byref<'e>) =
       // We just mutate the element in-place within the stack allocated space.
       e <- cloneElem e}

  // Rules for sum types -------------------------------------------------------

  /// A rule for cloning an arbitrary sum type.
  member this.Sum (asSum: AsSum<'s, 't>,
                   CloneS cloneSum: CloneS<'s, 's, 't>) =
    // First we stage an array for fast indexing of cases.  It is important that
    // we do this outside of the cloning function that we actually return.
    let cases = Array.ofList cloneSum
    // The cloning function.
    fun original ->
      // First we extract the case index.
      let caseIndex = asSum.Tag original
      // And then we invoke the case specific cloning function.
      cases.[caseIndex] original

  /// A rule cloning a partial set of union cases.
  ///
  /// Note that the type for the result is not the type that would be inferred
  /// by F#, so specifying the result type here is necessary.
  member this.Choice (CloneS cloneP: CloneS<       'p,      Choice<'p, 'o>, 't>,
                      CloneS cloneO: CloneS<           'o,             'o , 't>)
                                   : CloneS<Choice<'p, 'o>, Choice<'p, 'o>, 't> =
   // We just concatenate the individual case (of length 1) and the other cases
   // together.
   CloneS (cloneP @ cloneO)

  /// A rule for cloning a non-nullary union case.
  member this.Case (case: Case<'p, 'o, 't>,
                    cloneProduct: CloneP<'p, 'p, 'o, 't>) : CloneS<'p, 'o, 't> =
    // We forward here to a helper function to avoid duplicating code.
    let cloneCase : Clone<'t> = this.Product (case, cloneProduct)
    // Then we return just this case. 
    CloneS [cloneCase]

  /// A rule for cloning a nullary union case.
  member this.Case (_: Case<Empty, 'cs, 't>) : CloneS<Empty, 'cs, 't> =
    // There is nothing to clone in the empty case.
    let cloneCase : Clone<'t> = fun x -> x
    // Then we return just this case. 
    CloneS [cloneCase]
