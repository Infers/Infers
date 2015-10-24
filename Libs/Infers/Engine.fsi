// Copyright (C) by Vesa Karvonen

namespace Infers

/// Interface to the resolution engine.
module Engine =
  /// Using IDDFS, tries to generate a value of the type `'a` by using the
  /// given set of inference rules.  See  `InferenceRules`.
  val tryGenerate: 'r -> option<'a> when 'r :> Rules

  /// Using DFS, tries to generate a value of the type `'a` by using the given
  /// set of inference rules.  See  `InferenceRules`.
  val tryGenerateDFS: 'r -> option<'a> when 'r :> Rules

  /// Combination of `Engine.tryGenerate` and `StaticMap.Memoize` for convenient
  /// invocation of inference rules.
  val generate<'r, 'v when 'r :> Rules and 'r: (new: unit -> 'r)> : 'v

  /// Combination of `Engine.tryGenerateDFS` and `StaticMap.Memoize` for
  /// convenient invocation of inference rules.
  val generateDFS<'r, 'v when 'r :> Rules and 'r: (new: unit -> 'r)> : 'v
