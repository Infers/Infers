# Infers Tutorial

Infers is a library for deriving F# values from their types.  What does that
actually mean?  Let's work on a concrete example.

## Showing off

We can define a family of types, or more concretely, a generic type

```fsharp
type Show<'x> = 'x -> string
```

for functions that convert their input to a string.  Let's gradually create a
class with a family of such functions starting with `Show<bool>` and `Show<int>`
for converting booleans and integers to strings:

```fsharp
type Show () =
  member show.bool: Show<bool> = sprintf "%A"
  member show.int: Show<int> = sprintf "%d"
```

As any functional programmer knows, we can build more complicated show functions
using higher-order functions.  Here are such higher-order functions for pairs
and lists:

```fsharp
type Show () =
  // ...
  member show.pair (showX: Show<'x>, showY: Show<'y>) : Show<'x * 'y> =
    fun (x, y) -> "(" + showX x + ", " + showY y + ")"
  member show.list (showX: Show<'x>) : Show<list<'x>> = fun xs ->
    "[" + String.concat "; " (List.map showX xs) + "]"
```

Suppose that we have bound an instance of `Show` to the variable `show`.

```fsharp
let show = Show ()
```

Using `show` we can build a function for showing lists of pairs of integers and
booleans as follows:

```fsharp
let show_int_bool_pair_list : Show<list<int * bool>> =
  show.list (show.pair (show.int, show.bool))
```

And, just to check, let's see what it produces when we give it the value
`[(1, true)]`:

```fsharp
> show_int_bool_pair_list [(1, true)] ;;
val it : string = "[(1, true)]"
```

To recap, we defined a family of types `Show<'x>` and a family of functions
`bool`, `int`, `pair`, and `list` for building values of the type `Show<'x>`.
Recall that Infers is a library for deriving values from their types.  Can we
use Infers to build values of the `Show<'x>` family of type using the family of
functions we defined.  Yes, we can.

### Enter Infers

First we mark the `Show` class as defining a set of `InferenceRules`.  Here is
the modified `Show` class:

```fsharp
type [<InferenceRules>] Show () =
  member show.bool: Show<bool> = sprintf "%A"
  member show.int: Show<int> = sprintf "%d"
  member show.pair (showX: Show<'x>, showY: Show<'y>) : Show<'x * 'y> =
    fun (x, y) -> "(" + showX x + ", " + showY y + ")"
  member show.list (showX: Show<'x>) : Show<list<'x>> = fun xs ->
    "[" + String.concat "; " (List.map showX xs) + "]"
```

Then we define a function that invokes the Infers engine on those rules:

```fsharp
let show (x: 'x) : string =
  match Engine.TryGenerate (Show ()) with
   | Some show -> show x
   | None -> failwith "No show!"
```

Let's try that then:

```fsharp
> show 1 ;;
val it : string = "1"
> show (1, true) ;;
val it : string = "(1, true)"
> show ([1], (2, [3; 4])) ;;
val it : string = "([1], (2, [3; 4]))"
```

How does this actually work?  It helps to look at the signature of the `Show`
class:

```fsharp
type Show =
  new: unit -> Show
  member bool: Show<bool>
  member int: Show<int>
  member list: Show<'x> -> Show<list<'x>>
  member pair: Show<'x> * Show<'y> -> Show<'x * 'y>
```

When we ask `Engine.TryGenerate` to derive a value of some desired type, say
`Show<list<int>>`, and give an object containing inference rules, say
`Show ()`, it goes over the member functions of the object and tries to match
the result types of those member functions with the desired type.

In the case of `Show<list<int>>`, the engine ultimately reaches the function
`list`

```fsharp
member list: Show<'x> -> Show<list<'x>>
```

and realizes that its result type matches the desired type, if it substitutes
`int` for `'x`.  Making that substitution to the generic method, the engine then
effectively ends up with a concrete version of the `list` method:

```fsharp
member list: Show<int> -> Show<list<int>>
```

The engine now knows that if it can somehow derive a value of type `Show<int>`,
it can derive a value of the desired type by applying the concrete version of
the `list` method.  To do that, the engine takes on a new goal: derive a value
of type `Show<int>`.  It again goes over the members and ultimately finds the
member `int`

```fsharp
member int: Show<int>
```

whose result exactly matches the type it was looking for and furthermore does
not require any parameters.  The engine then calls `int` and then calls `list`
with the returned value, obtaining a value of the desired type.

This is obviously a somewhat simplified description of the process, but should
help you to understand what is going on.  In fact, the Infers engine is quite
powerful.  Technically speaking it implements a complete resolution process for
[Horn clauses](http://en.wikipedia.org/wiki/Horn_clause).  The engine also
implements some heuristics to prefer more specific rules to less specific rules.

## Compared to Type Classes

The way in which we used Infers in the previous section to derive values of the
type family `Show<'x>` is similar to how a
[type and constructor class system](http://en.wikipedia.org/wiki/Type_class)
could be used to infer values given suitable type class definitions.

Compared to Infers, the type class mechanisms that can be found in various
languages are carefully engineered to be tractable.  For example, many of those
systems have the property that the type class resolution process terminates and
class constraints make sure that a program that type checks will not fail at
run-time due to inability to infer a value of some type used in the program.  On
the other hand, this means that the power of the resolution algorithm used by
such type class mechanisms is constrained.

Infers doesn't have such nice safety properties and, on the other hand, Infers
allows pretty much arbitrarily complex heterogeneous sets of inference rules to
be used.  Because there are very little constraints, there are likely to be
applications of Infers that are significantly dissimilar from anything that has
been done so far with type classes or, as described in the next section, with
datatype generic programming techniques.

## The Rep module and Datatype Generic Programming

Let's continue to extend the `Show` class we defined earlier.  One could extend
the `Show` class with members for tuples, `'t1 * ... * 'tn`, of various lengths
and all the standard type families like `option<'t>`, but that would be a never
ending job.  Fortunately, the Infers library comes with an inference rule
module, `Rep`, that can build type representations for various F# types and
using those type representations one can define rules over the structure of
tuple, record and union types.
