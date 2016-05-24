// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

/// Generic functions for ad hoc record manipulation.
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
  /// prints `a`.
#endif
  val get: 't -> 'e

  /// Iterates over the elements of the product of type `'p` using the handler
  /// functions in the product of functions of type `'handlers`.
#if DOC
  ///
  /// Each handler function must have a type of one of the following forms:
  ///
  ///>           'e -> unit
  ///>    int -> 'e -> unit
  ///> string -> 'e -> unit
  ///
  /// Only records and single case union types have labelled elements.  The
  /// handler is chosen by attempting to get an element of one of the above
  /// types from the handlers product.
  ///
  /// For example,
  ///
  ///> iter (printf "%s", printf "%1.1f", printf "%d") (1, "x", 2.0)
  ///
  /// prints `1x2.0`.
#endif
  val iter: 'handlers -> 'p -> unit
