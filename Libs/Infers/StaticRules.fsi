// Copyright (C) by Vesa Karvonen

namespace Infers

/// Combination of `Engine.TryGenerate` and `StaticMap.Memoize` for convenient
/// invocation of inference rules that can be statically memoized.
type [<Sealed>] StaticRules<'rules when 'rules : (new : unit -> 'rules)> =
  /// Memoizes the result of `Engine.TryGenerate (new 'rules () :> obj)`.
  static member Generate : unit -> 'result

  static member GenerateIDDFS : unit -> 'result
