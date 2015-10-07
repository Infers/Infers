// Copyright (C) by Vesa Karvonen

module Toys.Rec

open Infers
open Infers.Rep

type ProductRec<'e, 'r, 't>

/// Rules for computing fixed points over products, single case union types (aka
/// newtypes) and functions.
type [<InferenceRules>] Rec =
  inherit RecFn
  new: unit -> Rec
  member Elem: Elem<'e, 'r, 't> * Rec<'e> -> ProductRec<'e, 'r, 't>
  member Times: ProductRec<    'e     , And<'e, 'r>, 't>
              * ProductRec<        'r ,         'r , 't>
             -> ProductRec<And<'e, 'r>, And<'e, 'r>, 't>
  member Product: Rep * AsProduct<'p, 't> * ProductRec<'p, 'p, 't> -> Rec<'t>
