// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Product =
  /// Gets an element of type `'e` from the product of type `'t`.  If the
  /// product has multiple elements of type `'e` it is not specified which
  /// element is returned.
#if DOC
  ///
  /// For example,
  ///
  ///> get (1, "a", true) |> printf "%s"
  ///
  /// prints "a".
#endif
  val get: 't -> 'e
