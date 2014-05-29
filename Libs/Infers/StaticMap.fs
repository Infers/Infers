namespace Infers

type [<Sealed>] StaticMap<'k, 'v> =
  [<DefaultValue>]
  static val mutable private Value: 'v
  static member Get () = StaticMap<'k, 'v>.Value
  static member Set (value: 'v) = StaticMap<'k, 'v>.Value <- value
