// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

/// Generic functions for ad hoc record manipulation.
module Product =
  /// Gets an element of type `'e` from the product of type `'p`.  If the
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
  val get: 'p -> 'e

  /// Iterates over the elements of the product of type `'p` using the handler
  /// functions in the product of functions of type `'handlers`.
#if DOC
  ///
  /// Each handler function must have a type of one of the forms
  ///
  ///>                  'e -> unit
  ///> int           -> 'e -> unit
  ///>        string -> 'e -> unit
  ///> int -> string -> 'e -> unit
  ///
  /// where the type `'e` is the type of an element of `'p`, the `int` is the
  /// index of the element, and the `string` is the name of the element.
  ///
  /// Only records and single case union types have labeled elements.  The
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

  /// Creates a value of the given product of type `'p` using the handler
  /// functions in the product of function and values of type `'handlers`.
#if DOC
  ///
  /// Each handler function or value must have a type of one of the forms
  ///
  ///>                  'e
  ///> int           -> 'e
  ///>        string -> 'e
  ///> int -> string -> 'e
  ///
  /// where the type `'e` is the type of an element of `'p`, the `int` is the
  /// index of the element, and the `string` is the name of the element.
  ///
  /// Only records and single case union types have labeled elements.  The
  /// handler is chosen by attempting to get an element of one of the above
  /// types from the handlers product.
  ///
  /// For example,
  ///
  ///> init (sprintf "%d") : string * string * string
  ///
  /// returns the tuple `("0", "1", "2")`.
#endif
  val init: 'handlers -> 'p

  /// Fold over the elements of the product of type `'p` using the handler
  /// functions in the product of functions of type `'handlers`.
#if DOC
  ///
  /// Each handler function must have a type of one of the forms
  ///
  ///>                  'e -> 's -> 's
  ///> int           -> 'e -> 's -> 's
  ///>        string -> 'e -> 's -> 's
  ///> int -> string -> 'e -> 's -> 's
  ///
  /// where `'s` is the type of the fold state, `'e` is the type of an element
  /// of `'p`, the `int` is the index of the element, and the `string` is the
  /// name of the element.
  ///
  /// Only records and single case union types have labeled elements.  The
  /// handler is chosen by attempting to get an element of one of the above
  /// types from the handlers product.
  ///
  /// For example,
  ///
  ///> iter (sprintf "%s"    >> (+),
  ///>       sprintf "%1.1f" >> (+),
  ///>       sprintf "%d"    >> (+))
  ///>      (1, "x", 2.0)
  ///
  /// returns `"2.0x1"`.
#endif
  val fold: 'handlers -> 'p -> 's -> 's
