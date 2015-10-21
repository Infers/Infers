// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Basic =
  open Infers

  type Eq<'x, 'y> = Eq'2
  type Eq<'x, 'y, 'z> = Eq'3

  type [<InferenceRules>] Basic () =
    member g.Eq'2 () : Eq<'x, 'x> = Eq'2
    member g.Eq'3 () : Eq<'x, 'x, 'x> = Eq'3

    member g.Choice1Of2 (x: 'x) = Choice1Of2 x
    member g.Choice2Of2 (x: 'x) = Choice2Of2 x

    member g.Choice1Of3 (x: 'x) = Choice1Of3 x
    member g.Choice2Of3 (x: 'x) = Choice2Of3 x
    member g.Choice3Of3 (x: 'x) = Choice3Of3 x
