// Copyright (C) by Vesa Karvonen

namespace Infers

/// Interface to the resolution engine.
module Engine =
  /// Using IDDFS, tries to generate a value of the type `'a` by using the
  /// given set of inference rules.  See  `InferenceRules`.
  val tryGenerate: rules: obj -> option<'a>
