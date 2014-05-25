# Infers Tutorial

Infers is a library for deriving F# values from their types.  What does that
actually mean?  Let's work on a concrete example.

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
    "[" + String.concat ", " (List.map showX xs) + "]"
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

First we mark the `Show` class as defining a set of `InferenceRules`.  Here is
the modified `Show` class:

```fsharp
type [<InferenceRules>] Show () =
  member show.bool: Show<bool> = sprintf "%A"
  member show.int: Show<int> = sprintf "%d"
  member show.pair (showX: Show<'x>, showY: Show<'y>) : Show<'x * 'y> =
    fun (x, y) -> "(" + showX x + ", " + showY y + ")"
  member show.list (showX: Show<'x>) : Show<list<'x>> = fun xs ->
    "[" + String.concat ", " (List.map showX xs) + "]"
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
val it : string = "([1], (2, [3, 4]))"
```
