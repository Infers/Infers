// Copyright (C) by Vesa Karvonen

namespace Infers

/// Interface to the Infers resolution engine.
[<AutoOpen>]
module Engine =
  /// Using IDDFS, tries to generate a value of the type `'t` by using the given
  /// set of rules.  IDDFS is slow, but works even in cases where the given
  /// rules allow infinite non-productive derivations.
  val generate<'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't

  /// Using DFS, tries to generate a value of the type `'t` by using the given
  /// set of rules.  DFS is fast, but requires that the given rules do not allow
  /// infinite non-productive derivations.
  val generateDFS<'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't
