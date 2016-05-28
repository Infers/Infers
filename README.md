[![Travis Build Status](https://travis-ci.org/Infers/Infers.svg?branch=master)](https://travis-ci.org/Infers/Infers) [![NuGet](https://img.shields.io/nuget/v/Infers.svg)](https://www.nuget.org/packages/Infers/) [Infers Library Reference](http://infers.github.io/Infers/Infers.html)

Infers is a library for deriving F# values from their types and, in a way, a
direct application of the
[Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)
aka [Propositions as Types](https://www.youtube.com/watch?v=IOiZatlZtGU).

The basic idea of Infers is to view the types of static member functions as
[Horn clauses](https://en.wikipedia.org/wiki/Horn_clause).  Given a set of
[`Rules`](#Rules), it is then possible to attempt to prove goals using a
[Prolog](https://en.wikipedia.org/wiki/Prolog)-style
[resolution](https://en.wikipedia.org/wiki/Resolution_(logic)) algorithm.
Infers invokes the rule functions during the resolution process to `generate` a
value of the type given as the goal.

Another way to view Infers is as a specialized logic programming language
embedded in F#.  However, to support generation of F# values, the Infers
resolution algorithm differs from general purpose logic programming languages in
a number of ways:

* Infers prunes the search space so that when it encounters a goal to build a
  monomorphic value, it only tries to find one way, rather than all possible
  ways, to build it.

* Infers statically memoizes all final and intermediate values that it has
  built.  When you invoke Infers twice with the same set of rules and the same
  type to generate, Infers returns the same (physical) value.

* Infers has special support, [`Rec<'t>`](#Rec), for building cyclic values.  It
  is very common to need to build cyclic values to manipulate recursive types.

* Infers has special scoping rules such that when an antecedent value is built
  that contains [`Rules`](#Rules), those rules are added to the set of rules
  until the consequent has been built.  This allows new rules, such as rules for
  viewing a type as a sum of products, to be generated dynamically.

Infers can be useful, for example, in situations where one might wish to use
something like [type classes](https://en.wikipedia.org/wiki/Type_class) or when
one might want to do polytypic or datatype generic programming.  Other kinds of
applications are also quite possible.  For example, it is possible to solve
logic puzzles using Infers.

Here is a toy example of a set of rules that can generate functions to
arbitrarily reorder or flip the arguments of a given curried function:

```fs
type GFlip () =
  inherit Rules ()
  static member Id () = id
  static member First ab2yz = fun xb -> xb >> ab2yz
  static member Rest (ab2axc, ac2y) = fun ab ->
    let axc = ab2axc ab
    let xac = fun x a -> axc a x
    xac >> ac2y
```

To generate flipping functions we invoke `generate`:

```fs
let gflip f = generate<GFlip, (_ -> _) -> _ -> _> f
```

Now, for example, we could say:

```fs
gflip (sprintf "%s %d %c!") 2 'U' "Hello" = "Hello 2 U!"
```

You might want to try the above in a REPL.  There is a caveat: When you request
Infers to generate a value, the value must have a monomorphic type.

## Reference

```fs
namespace Infers
```

### <a name="Rules"></a>[`Rules`](#Rules)

```fs
type [<AbstractClass>] Rules =
  inherit System.Attribute
  new: unit -> Rules
```

A type that inherits `Rules` is assumed to contain total static rule methods
that are used by the resolution algorithm of Infers.  Do not inherit from a
class that inherits `Rules`.  A rule class can specify dependencies to other
rule classes as attributes.  Specify any rule classes that you wish to include
as attributes, e.g. `type [<Rules1;...;RulesN>] MyRules`.

### <a name="Rec"></a>[`Rec<'t>`](#Rec)

```fs
type [<AbstractClass>] Rec<'t> =
  new: unit -> Rec<'t>
  abstract Get: unit -> 't
  abstract Set: 't -> unit
```

A `Rec<'t>` is a proxy for a potentially cyclic value of type `'t`.

When the Infers resolution algorithm encounters a case where it needs to
build a value in terms of itself, for example, when building a function
manipulating a recursive union type, it automatically looks for a rule to
create a proxy for the value.  To support building cyclic values of type
`'t`, a rule must be given to build a `Rec<'t>`.

The `Get` method must return a wrapper of type `'t` that corresponds to the
value of the proxy.  Note that `Get` may be called on a `Rec` proxy before `Set`
is called.

The `Set` method must set the value of the proxy to close the resulting cyclic
value.

## <a name="Infers"></a>[`Infers`](#Infers)

```fs
[<AutoOpen>]
module Infers =
  val generate   <'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't
  val generateDFS<'r, 't when 'r :> Rules and 'r: (new: unit -> 'r)> : 't
```

The type-indexed functions of the `Infers` module, `generate<'r, 't>` and
`generateDFS<'r, 't>` are the interface to the Infers resolution algorithm.
They both try to generate a value of type `'t` by using the given set of rules
`'r`.  If a value can be generated, it is statically memoized, so that
subsequently the same value is returned instantly.  An exception is raised in
case Infers detects that there is no way to build the desired value with the
given rules.

The difference between `generate<'r, 't>` and `generateDFS<'r, 't>` is that the
former uses
[IDDFS](https://en.wikipedia.org/wiki/Iterative_deepening_depth-first_search)
while the latter uses [DFS](https://en.wikipedia.org/wiki/Depth-first_search).

IDDFS is slow, but works even in cases where the given rules allow infinite
non-productive derivations.  IDDFS also always finds a minimal solution in the
sense that the depth of the derivation tree is minimal.

DFS is fast, but requires that the given rules do not allow infinite
non-productive derivations.  DFS also does not necessarily find a minimal
solution.  Therefore, DFS should only be used when the rules are essentially
deterministic, which basically means that there is only one way to generate a
value of any given type using the rules.

In practice, one should almost always seek to create rules that work with DFS,
but there are some valid applications where DFS will not work.
