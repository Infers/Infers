// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Basic =
  open Infers

  type Eq<'x, 'y> = Eq'2
  type Eq<'x, 'y, 'z> = Eq'3

  type Basic () =
    inherit Rules ()

    static member Eq'2 () : Eq<'x, 'x> = Eq'2
    static member Eq'3 () : Eq<'x, 'x, 'x> = Eq'3

    static member Choice1Of2 (x: 'x) = Choice1Of2 x
    static member Choice2Of2 (x: 'x) = Choice2Of2 x

    static member Choice1Of3 (x: 'x) = Choice1Of3 x
    static member Choice2Of3 (x: 'x) = Choice2Of3 x
    static member Choice3Of3 (x: 'x) = Choice3Of3 x
