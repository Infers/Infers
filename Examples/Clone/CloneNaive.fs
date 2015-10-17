// Copyright (C) by Vesa Karvonen

module CloneNaive

open Infers
open Infers.Rep

////////////////////////////////////////////////////////////////////////////////

/// Type of cloning functions.
type Clone<'t> = 't -> 't

/// Type of an intermediate step in cloning a product type.
type CloneP<'e, 'r, 'o, 't> = CloneP of Clone<'e>

/// Type of a partial sum cloning function (or list of such functions).
type CloneS<'p, 'o, 't> = CloneS of list<Clone<'t>>

////////////////////////////////////////////////////////////////////////////////

/// Inference rules for creating cloning functions.
type [<InferenceRules>] Clone = // (_: Rep) =
  new (_: Rep) = {}
//  member t.Require (_: Rep) = ()
//  inherit Rep ()

  // Recursion rule ------------------------------------------------------------

  // Rule for creating a proxy when defining a recursive cloning function.
  member t.Fix (recFn: RecFn) : Rec<Clone<'t>> =
    recFn.func ()

  // Rules for simple base types -----------------------------------------------

  /// A rule for the `int32` type.
  member t.Int : Clone<int> = fun x -> x

  /// A rule for the `string` type.
  member t.String : Clone<string> = fun x -> x

  // Rules for some specific type constructors ---------------------------------

  /// A rule for the `ref<'t>` type constructor.
  member t.Ref (cloneT: Clone<'t>) : Clone<ref<'t>> =
    fun r -> ref (cloneT (!r))

  /// A rule for the `array<'t>` type constructor.
  member t.Array (cloneT: Clone<'t>) : Clone<array<'t>> =
    Array.map cloneT

  // Rules for product types ---------------------------------------------------

  /// A rule for cloning an arbitrary product (tuple, record or union case).
  member t.Product (asPairs: AsPairs<'p, 'o, 't>,
                    CloneP cloneP: CloneP<'p, 'p, 'o, 't>) =
    asPairs.ToPairs >> cloneP >> asPairs.OfPairs

  /// A rule for cloning a part of a product consisting of an `Elem: 'e` and
  /// `Rest: 'es`.
  member t.Pair (CloneP cloneE: CloneP<     'e     , Pair<'e, 'r>, 'o, 't>,
                 CloneP cloneR: CloneP<         'r ,          'r , 'o, 't>)
                              : CloneP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
    CloneP <| fun (Pair (elem, rest)) ->
      Pair (cloneE elem, cloneR rest)

  /// A rule for cloning a specific element of type `'e` within a product.
  member t.Elem (_: Elem<'e, 'r, 'o, 't>, cloneElem: Clone<'e>) =
    CloneP cloneElem

  // Rules for sum types -------------------------------------------------------

  /// A rule for cloning an arbitrary sum type.
  member t.Sum (asChoices: AsChoices<'s, 't>,
                CloneS cloneSum: CloneS<'s, 's, 't>) =
    // First we stage an array for fast indexing of cases.  It is important that
    // we do this outside of the cloning function that we actually return.
    let cloneSum = Array.ofList cloneSum
    // The cloning function.
    fun original ->
      // First we extract the case index.
      let caseIndex = asChoices.Tag original
      // And then we invoke the case specific cloning function.
      cloneSum.[caseIndex] original

  /// A rule cloning a partial set of union cases.
  ///
  /// Note that the type for the result is not the type that would be inferred
  /// by F#, so specifying the result type here is necessary.
  member t.Choice (CloneS cloneP: CloneS<       'p,      Choice<'p, 'o>, 't>,
                   CloneS cloneO: CloneS<           'o,             'o , 't>) =
   CloneS (cloneP @ cloneO)     : CloneS<Choice<'p, 'o>, Choice<'p, 'o>, 't>
   // We just concatenate the individual case (of length 1) and the other cases
   // together.

  /// A rule for cloning a non-nullary union case.
  member t.Case (case: Case<'p, 'o, 't>, cloneProduct: CloneP<'p, 'p, 'o, 't>) =
    // We forward here to a helper function to avoid duplicating code.
    let cloneCase : Clone<'t> = t.Product (case, cloneProduct)
    // Then we return just this case.
    CloneS [cloneCase] : CloneS<'p, 'o, 't>

  /// A rule for cloning a nullary union case.
  member t.Case (_: Case<Empty, 'cs, 't>) : CloneS<Empty, 'cs, 't> =
    // There is nothing to clone in the empty case.
    let cloneCase : Clone<'t> = fun x -> x
    // Then we return just this case.
    CloneS [cloneCase]
