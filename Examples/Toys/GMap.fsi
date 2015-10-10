// Copyright (C) by Vesa Karvonen

module Toys.GMap

open Infers
open Infers.Rep

type GMap<'w, 'p> = ('p -> 'p) -> 'w -> 'w
type GM<'w, 'p>
type ProductGM<'e, 'r, 'w, 'p>
type SumGM<'c, 's, 'w, 'p>

type [<InferenceRules>] GMap =
  new: unit -> GMap

  member Same: unit -> GMap<'p, 'p>
  member NotSame: Rep * RecFn * GM<'w, 'p> -> GMap<'w, 'p>

  member Prim: Prim<'w> -> GM<'w, 'p>

  member Elem: Elem<'e, 'r, 'c, 'w> * GMap<'e, 'p> -> ProductGM<'e, 'r, 'w, 'p>

  member Times: ProductGM<    'e     , And<'e, 'r>, 'w, 'p>
              * ProductGM<        'r ,         'r , 'w, 'p>
             -> ProductGM<And<'e, 'r>, And<'e, 'r>, 'w, 'p>

  member Product: AsProduct<'r, 'w> * ProductGM<'r, 'r, 'w, 'p> -> GM<'w, 'p>

  member Case: Case<Empty, 's, 'w> -> SumGM<Empty, 's, 'w, 'p>

  member Case: Case<'e, 's, 'w>
             * ProductGM<'e, 'e, 'w, 'p>
            -> SumGM<'e, 's, 'w, 'p>

  member Plus: SumGM<       'e     , Choice<'e, 'r>, 'w, 'p>
             * SumGM<           'r ,            'r , 'w, 'p>
            -> SumGM<Choice<'e, 'r>, Choice<'e, 'r>, 'w, 'p>

  member Sum: Union<'w> * AsChoice<'s, 'w> * SumGM<'s, 's, 'w, 'p> -> GM<'w, 'p>

val gmap: ('p -> 'p) -> ('w -> 'w)
