// Copyright (C) by Vesa Karvonen

/// Infers is a library for deriving F# values from their types and, in a way, a
/// direct application of the Curry-Howard correspondence.
///
/// The basic idea of Infers is to view the types of static member functions as
/// Horn clauses.  Given a set of `Rules`, it is then possible to attempt to
/// prove goals using a Prolog-style resolution algorithm.  Infers invokes the
/// rule functions during the resolution process to `generate` a value of the
/// type given as the goal.
///
/// Another way to view Infers is as a specialized logic programming language
/// embedded in F#.  However, to support generation of F# values, the Infers
/// resolution algorithm differs from general purpose logic programming
/// languages in a number of ways:
///
/// - Infers prunes the search space so that when it encounters a goal to build
/// a monomorphic value, it only tries to find one way, rather than all possible
/// ways, to build it.
///
/// - Infers implicitly memoizes final and intermediate results.
///
/// - Infers has special support, `Rec<'t>`, for building cyclic values.  It is
/// very common to need to build cyclic values to manipulate recursive types.
///
/// - Infers has special scoping rules such that when an antecedent value is
/// built that contains `Rules`, those rules are added to the set of rules until
/// the consequent has been built.  This allows new rules, such as rules for
/// viewing a type as a sum of products, to be generated dynamically.
///
/// Infers can be useful, for example, in situations where one might wish to use
/// something like type classes or when one might want to do polytypic or
/// datatype generic programming.  Other kinds of applications are also quite
/// possible.  For example, it is possible to solve logic puzzles using Infers.
///
/// Here is an example of a set of rules that can generate functions to
/// arbitrarily reorder or flip the arguments of a given curried function:
///
///> type GFlip () =
///>   inherit Rules ()
///>   static member Id () = id
///>   static member First ab2yz = fun xb -> xb >> ab2yz
///>   static member Rest (ab2axc, ac2y) = fun ab ->
///>     let axc = ab2axc ab
///>     let xac = fun x a -> axc a x
///>     xac >> ac2y
///
/// To generate flipping functions we invoke `generate`:
///
///> let gflip f = generate<GFlip, (_ -> _) -> _ -> _> f
///
/// Now, for example, we could say:
///
///> gflip (sprintf "%s %d %c!") 2 'U' "Hello" = "Hello 2 U!"
///
/// You might want to try the above in a REPL.  There is a caveat: When you
/// request Infers to generate a value, the value must have a monomorphic type.
///
/// The above `GFlip` example is very much a toy example, although one could
/// sometimes find it useful.
namespace Infers

/// A type that inherits `Rules` is assumed to contain total static rule methods
/// that are used by the resolution algorithm of Infers.  A rule class can also
/// specify dependencies to other rule classes as attributes.
type [<AbstractClass>] Rules =
  inherit System.Attribute
  new: unit -> Rules

/// Proxy for a potentially cyclic value of type `'t`.
///
#if DOC
/// When the Infers resolution algorithm encounters a case where it needs to
/// build a value in terms of itself, for example, when building a function
/// manipulating a recursive union type, it automatically looks for a rule to
/// create a proxy for the value.  So, to support building cyclic values of type
/// `'t`, a rule must be given to build a `Rec<'t>`.
#endif
type [<AbstractClass>] Rec<'x> =
  new: unit -> Rec<'x>

  /// Returns a wrapper of type `'x` that corresponds to the value of the
  /// proxy.  Note that `Get` may be called on a `Rec` proxy before `Set` is
  /// called.
  abstract Get: unit -> 'x

  /// Sets the value of the proxy.
  abstract Set: 'x -> unit

/// Interface to the Infers resolution algorithm.
[<AutoOpen>]
module Infers =
  /// Using IDDFS, tries to generate a value of the type `'t` by using the given
  /// set of rules.  IDDFS is slow, but works even in cases where the given
  /// rules allow infinite non-productive derivations.  An exception is raised
  /// in case Infers detects that there is no way to build the desired value
  /// with the given rules.
  val generate<'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't

  /// Using DFS, tries to generate a value of the type `'t` by using the given
  /// set of rules.  DFS is fast, but requires that the given rules do not allow
  /// infinite non-productive derivations.  An exception is raised in case
  /// Infers detects that there is no way to build the desired value with the
  /// given rules.
  val generateDFS<'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't
