// Copyright (C) by Vesa Karvonen

namespace Infers

/// Interface to the resolution engine.
type [<Sealed>] Engine =
  /// Using IDDFS, tries to generate a value of the type `'a` by using the
  /// given set of inference rules.  See  `InferenceRules`.
  static member TryGenerate: explain: bool
                           * initialDepth: int
                           * maxDepth: int
                           * rules: seq<obj>
                          -> option<'a>

  /// `TryGenerate rules` is equivalent to `TryGenerate (false,
  /// Int32.MaxValue, Int32.MaxValue, [rules])`.  This is equivalent to
  /// doing a DFS search.  Use this method when you know that the set of
  /// rules is essentially unambiquous.
  static member TryGenerate: rules: obj -> option<'a>

  /// `TryGenerate rules` is equivalent to `TryGenerate (false,
  /// Int32.MaxValue, Int32.MaxValue, rules)`.  This is equivalent to
  /// doing a DFS search.  Use this method when you know that the set of
  /// rules is essentially unambiquous.
  static member TryGenerate: rules: seq<obj> -> option<'a>
