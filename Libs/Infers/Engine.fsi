// Copyright (C) by Vesa Karvonen

namespace Infers

/// Interface to the resolution engine.
module Engine =
  /// Using IDDFS, tries to generate a value of the type `'a` by using the
  /// given set of inference rules.  See  `InferenceRules`.
  val tryGenerate: rules: obj -> option<'a>

  /// Using DFS, tries to generate a value of the type `'a` by using the given
  /// set of inference rules.  See  `InferenceRules`.
  val tryGenerateDFS: rules: obj -> option<'a>

  /// Combination of `Engine.tryGenerate` and `StaticMap.Memoize` for convenient
  /// invocation of inference rules.
  val generate<'rules, 'result> : 'result when 'rules : (new : unit -> 'rules)

  /// Combination of `Engine.tryGenerateDFS` and `StaticMap.Memoize` for
  /// convenient invocation of inference rules.
  val generateDFS<'rules, 'result> : 'result when 'rules : (new : unit -> 'rules)
