# Infers Tutorial

Infers is a library for deriving F# values from their types.  What does that
actually mean?  Let's work on a concrete example.

## Showing off

We can define a family of types, or more concretely, a generic type

```fsharp
type Show<'x> = 'x -> string
```

for functions that convert their input to a string.  Let's gradually create a
class with a family of such functions defined as members, starting with members
for `Show<bool>` and `Show<int>` for converting booleans and integers to
strings:

```fsharp
type Show () =
  member show.bool: Show<bool> = sprintf "%A"
  member show.int: Show<int> = sprintf "%d"
```

As any functional programmer knows, we like to show off the ability to build
more complicated functions using higher-order functions.  Here are such
higher-order functions for pairs and lists:

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
use Infers to build values of the `Show<'x>` family of types using the family of
functions we defined?  Yes, we can.

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

From the point of view of Infers, the above class definition now contains
logical *rules* for deriving *proofs* to *theorems* specified as F# types.
Those rules are just the members, `bool`, `int`, `pair`, and `list`, that we
defined earlier.  In a way, Infers is a direct application of the
[Curry-Howard Correspondence](http://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).

Using Infers, or more specifically, the `Engine.TryGenerate` method, which is
one entry point to the Infers resolution engine, we can define a function that
tries to derive values of the `Show<'x>` family of types using the rules in the
`Show` class:

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

How does this actually work?  Let's find out.

### The resolution process, informally

It helps to look at the signature of the `Show` class:

```fsharp
type Show =
  new: unit -> Show
  member bool: Show<bool>
  member int: Show<int>
  member list: Show<'x> -> Show<list<'x>>
  member pair: Show<'x> * Show<'y> -> Show<'x * 'y>
```

When we ask `Engine.TryGenerate` to derive a value of some desired type, say
`Show<list<int>>`, and give an object containing inference rules, say `Show ()`,
the resolution engine goes over the members, or rules, of the object and tries
to match the result types, or conclusions, of those member functions with the
desired type.

In the case of `Show<list<int>>`, the engine ultimately reaches the `list` rule

```fsharp
member list: Show<'x> -> Show<list<'x>>
```

and realizes that its result type matches the desired type, if it substitutes
`int` for `'x`.  Making that substitution to the generic method, the engine then
effectively ends up with a concrete version of the `list` rule:

```fsharp
member list: Show<int> -> Show<list<int>>
```

The engine now knows that if it can somehow derive a value of type `Show<int>`,
it can derive a value of the desired type by applying the concrete version of
the `list` method.  To do that, the engine takes on a new goal: derive a value
of type `Show<int>`.  It again goes over the rules and ultimately finds the
`int` rule

```fsharp
member int: Show<int>
```

whose result exactly matches the type it was looking for and furthermore does
not require any parameters.  The engine then calls `int` and then calls `list`
with the returned value, obtaining a value of the desired type.

We've named our rules using names like `int` and `list`, that basically
correspond to the type constructor those rules support.  The Infers engine
actually completely ignores the names of methods defining rules and only looks
at their type signatures.

This is obviously a somewhat simplified description of the process, but should
help you to understand what is going on.  In fact, the Infers engine is quite
powerful.  Technically speaking it implements a complete resolution process for
[Horn clauses](http://en.wikipedia.org/wiki/Horn_clause).  The engine also
implements some heuristics to prefer more specific rules to less specific rules
so that given an overlapping set of rules, Infers first tries the more specific
rules, assuming it can order those overlapping rules.

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
be used.  Because there are very few constraints, there are likely to be
applications of Infers that are significantly dissimilar from anything that has
been done so far with type classes, or, as described in the next section, with
datatype generic programming techniques.

## The Rep class and Datatype Generic Programming

Let's continue to extend the `Show` class we defined earlier.  One could extend
the `Show` class with rules for tuples, `'t1 * ... * 'tn`, of various lengths
and all the standard type families like `option<'t>`, but that would be a never
ending job.  Fortunately, the Infers library comes with an inference rule class,
`Rep`, that can build type representations for various F# types and using those
type representations one can define rules over the structure of tuple, record
and union types.

### Tuples as nested products

The basic idea behind the `Rep` class is to view tuples and records as nested
products and union types as nested sums.  For example, the tuple type

```fsharp
int * unit * bool * float
```

can be viewed as a nested product type

```fsharp
And<int, And<unit, And<bool, float>>>
```

where `And` is defined as the type

```fsharp
type [<Struct>] And<'x, 'xs> =
  val mutable Elem: 'x
  val mutable Rest: 'xs
```

It is easy to define a method to derive `Show<And<'x, 'xs>>` for arbitrary `'x`
and `'xs`:

```fsharp
type [InferenceRules] Show () =
  // ...
  member show.prod (showX: Show<'x>, showXs: Show<'xs>) : Show<And<'x, 'xs>> =
    fun xxs -> showX xxs.Elem + ", " + showXs xxs.Rest
```

And using the features of the `Rep` class, we can define a method for arbitrary
tuples:


```fsharp
type [InferenceRules] Show () =
  // ...
  member show.tuple (_: Rep,
                     _: Tuple<'t>,
                     asP: AsProduct<'p, 't>,
                     showP: Show<'p>) : Show<'t> =
   fun t -> "(" + showP (asP.ToProduct t) + ")"
```

Here is the whole `Show` class so far:

```fsharp
type [<InferenceRules>] Show () =
  member show.bool: Show<bool> = sprintf "%A"
  member show.int: Show<int> = sprintf "%d"
  member show.pair (showX: Show<'x>, showY: Show<'y>) : Show<'x * 'y> =
    fun (x, y) -> "(" + showX x + ", " + showY y + ")"
  member show.list (showX: Show<'x>) : Show<list<'x>> = fun xs ->
    "[" + String.concat "; " (List.map showX xs) + "]"
  member show.prod (showX: Show<'x>, showXs: Show<'xs>) : Show<And<'x, 'xs>> =
    fun xxs -> showX xxs.Elem + ", " + showXs xxs.Rest
  member show.tuple (_: Rep,
                     _: Tuple<'t>,
                     asP: AsProduct<'p, 't>,
                     showP: Show<'p>) : Show<'t> =
   fun t -> "(" + showP (asP.ToProduct t) + ")"
```

We have now defined rules powerful enough to show arbitrary tuples:

```fsharp
> show ([1], true) ;;
val it : string = "([1], true)"
> show (1, (2, [3], 4, 5), true) ;;
val it : string = "(1, (2, [3], 4, 5), true)"
```

Actually, in the above interaction, the first call `show ([1], true)` uses the
previously defined `pair` rule, while the second call uses the new `tuple` rule
twice.  Why is this so?  The Infers engine tries to order the rules from more
specific to less specific by considering how general the result types of rules
are.  The result type of `pair` is `Show<'x * 'y>`, while the result type of
`tuple` is `Show<'t>`.  Any type that unifies with `Show<'x * 'y>` also unifies
with `Show<'t>`, but not vice verse, so Infers prefers the `pair` rule to the
`tuple` rule.  This can be used to specialize rules for specific type families.
Of course, in this case, the `tuple` rule subsumes the `pair` rule, so we'll
drop the `pair` rule in the remainder.

When using Infers and the `Rep` class, the nested product types, using `And<'x,
'xs>` types, can be used merely as *guides* for the rule methods and it is
possible to manipulate tuples without converting them to a nested product like
we did above.  For simplicity, we'll ignore that possibility for the moment.

### The tuple method revisited

Let's take closer look at the `tuple` rule to better understand what is going
on.  Again it helps to look at the signature:

```fsharp
member tuple: Rep * Tuple<'t> * AsProduct<'p, 't> * Show<'p> -> Show<'t>
```

What this signature tells the Infers engine is that in order to create a
`Show<'t>` for an arbitrary `'t` it needs to generate a `Rep`, a `Tuple<'t>`, a
`AsProduct<'p, 't>`, and a `Show<'p>`.

#### The Rep class

The `Rep` type is actually a class that defines inference rules much like the
`Show` class.  Here is a part of the signature of the `Rep` class:

```fsharp
type [<InferenceRules>] Rep =
  new: unit -> Rep
  member union: unit -> Union<'u>
  member record: unit -> Record<'r>
  member tuple: unit -> Tuple<'t>
  // ...
```

As can be seen, the `Rep` class has a default constructor.  As a convenience,
the Infers engine automatically considers default constructors in addition to
ordinary rule methods when it tries to come up with a way to build a value of a
desired type.  So, after matching a type to the return type of the `tuple`
method of our `Show` class, the first parameter, of type `Rep`, is
easy&mdash;just use the default constructor.

As can be seen above, the `Rep` class also has the `InferenceRules` attribute.
When working to generate the parameters required by a rule, Infers always works
from left to right.  After successfully constructing a parameter value, the
Infers engine tests whether the type defines the `InferenceRules` attribute.  If
it does, then the engine adds the rules defined in the object to the set of
available rules to consider while working on the rest of the parameters for the
current rule.

We took advantage of this feature when defining the `tuple` rule

```fsharp
member tuple: Rep * Tuple<'t> * AsProduct<'p, 't> * Show<'p> -> Show<'t>
```

as the `Rep` parameter is actually there only to bring the additional rules
defined by the `Rep` class into scope.  This is how Infers learns about the
`tuple` rule

```fsharp
member tuple: unit -> Tuple<'t>
```

defined in the `Rep` class to create a `Tuple<'t>`.

It is important to understand that the `Rep` class isn't really a part of the
Infers engine, so to speak.  The `Rep` class is an independent module designed
to work with the Infers engine, utilizing reflection and run-time code
generation, to allow the definition of efficient datatype generic functions.
You could write similar classes yourself to help with other kinds of problems.

#### The Tuple type and Backtracking

The `Tuple<'t>` type is also a class that defines inference rules.  Of course,
not every type `'t` is a tuple.  The definition of the `Rep.tuple` method
checks, using reflection, whether the `'t` type actually is a tuple or not.  If
it isn't a tuple, then the method raisea the `Backtrack` exception

```fsharp
exception Backtrack
```

which is understood by the Infers engine as an order to backtrack outwards to
try another way to generate the desired type.

In our case, of course, the `Rep.tuple` rule ultimately succeeds, because we are
generating types containing tuples.  The returned object of type `Tuple<'t>` is
yet another object containing inference rules.  Its signature is somewhat
opaque, however, because the main feature of the `Tuple<'t>` class is that it
contains a run-time generated rule for a `AsProduct<'p, 't>` class, where the
`'p` type defines the nested product, generated at run-time, and there are
run-time generated methods to convert between a nested product and, in our case,
the original tuple.  Here is a part of the signature of the `AsProduct<'p, 't>`
class:

```fsharp
type AsProduct<'p, 't> =
  // ...
  abstract ToProduct: 't -> 'p
  abstract OfProduct: 'p -> 't
```

To recap, the `Show.tuple` method

```fsharp
member tuple: Rep * Tuple<'t> * AsProduct<'p, 't> * Show<'p> -> Show<'t>
```

defines a rule that allows a value of type `Show<'t>` to be derived assuming
that one can derive a `Rep`, a `Tuple<'t>`, a `AsProduct<'p, 't>`, and a
`Show<'p>`.  We just covered all except the `Show<'p>` value, which we defined
earlier in the form of the `Show.prod` member

```fsharp
member prod: Show<'x> * Show<'xs> -> Show<And<'x, 'xs>>
```

because the type `'p`, in the run-time generated `AsProduct<'p, 't>`, is a
nested product of the `And<'x, 'xs>` types, corresponding to the `'t` tuple
type.

### Unions as nested choices or sums

At this point we have a basic understanding of Infers and the `Rep` module.
Let's next work through somewhat more involved rules to show unions, such as:

```
type Example =
  | Concrete of title: string * description: string
  | Abstract of title: string
  | Blank
```

Much like with tuples, we can represent the structure of arbitrary unions using
nested choices of nested products.  The structure of the above `Example` type
could be encoded as follows:

```fsharp
Choice< And<string, string>,  // Concrete of string * string
        Choice< string,       // Abstract of string
                Empty > >     // Blank
```

Above, `Empty` is just an empty struct type used as a special case for empty
products.

Using the `Rep` module, the signature of a basic rule for showing unions would
look like:

```fsharp
member union: Rep
            * Union<'u>
            * AsChoice<'c, 'u>
            * ShowUnion<'c, 'c, 'u>
           -> Show<'u>
```

Comparing to the previously defined `Show.tuple` rule, this looks quite similar.
Like with tuples using nested products, the `'c` type in the above will get
bound to a run-time generated nested choice of products.  The only surprise is
the new `ShowUnion<'c, 'c, 'u>` type, which is we define as follows:

```fsharp
type ShowUnion<'c, 'cs, 'u> =
  SU of list<Show<'u>>
```

The idea is that an instance of the type `ShowUnion<'c, 'cs, 'u>` defines a list
of functions for showing union cases encoded by the nested choice type `'c`
within the (larger) nested choice `'cs`, which is a part of a nested choice
representation of the union type `'u`.  For example, if we were deriving a show
function for the `Example` type, we would end up generating a

```fsharp
ShowUnion<string, Choice<string, Empty>, Example>
```

which would be a singleton list, containing a show function for the `Abstract`
case of the `Example` type.  As another example,

```fsharp
ShowUnion<Choice<string, Empty>, Choice<string, Empty>, Example>
```

would be a list containing two show functions, one for the `Abstract` case and
another for the `Blank` case.

The reason for constructing these lists is that after we have constructed
functions for all the individual cases of the union, we can then efficiently
select the function from the list corresponding to a desired case give the
integer tag of the case.



```fsharp
member plus: ShowUnion<       'c      , Choice<'c, 'cs>, 'u>
           * ShowUnion<           'cs ,            'cs , 'u>
          -> ShowUnion<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u>
```


```fsharp
member case: Case<Empty, 'cs, 'u>             -> ShowUnion<Empty, 'cs, 'u>
member case: Case<  'ls, 'cs, 'u> * Show<'ls> -> ShowUnion<  'ls, 'cs, 'u>
```


Collecting together all the rules we've defined so far, we have the following
definition of the `Show` class:

```fsharp
type [<InferenceRules>] Show () =
  member show.string: Show<string> = sprintf "%A"

  member show.bool: Show<bool> = sprintf "%A"
  member show.int: Show<int> = sprintf "%d"

  member show.list (showX: Show<'x>) : Show<list<'x>> = fun xs ->
    "[" + String.concat "; " (List.map showX xs) + "]"

  member show.prod (showX: Show<'x>, showXs: Show<'xs>) : Show<And<'x, 'xs>> =
    fun xxs -> showX xxs.Elem + ", " + showXs xxs.Rest
  member show.tuple (_: Rep,
                     _: Tuple<'t>,
                     asP: AsProduct<'p, 't>,
                     showP: Show<'p>) : Show<'t> =
   fun t -> "(" + showP (asP.ToProduct t) + ")"

  member show.case (case: Case<Empty, 'cs, 'u>) : ShowUnion<Empty, 'cs, 'u> =
    SU [fun u -> case.Name]
  member show.case (case: Case<'ls, 'cs, 'u>, showP: Show<'ls>)
                  : ShowUnion<'ls, 'cs, 'u> =
    SU [fun u -> case.Name + " (" + showP (case.ToProduct u) + ")"]

  member show.plus (SU showC: ShowUnion<'c, Choice<'c, 'cs>, 'u>,
                    SU showCs: ShowUnion<'cs, 'cs, 'u>)
                  : ShowUnion<Choice<'c, 'cs>, Choice<'c, 'cs>, 'u> =
    SU (showC @ showCs)

  member show.union (_: Rep,
                     union: Union<'u>,
                     _: AsChoice<'c, 'u>,
                     SU showC: ShowUnion<'c, 'c, 'u>)
                   : Show<'u> =
    let showC = Array.ofList showC
    fun u -> showC.[union.Tag u] u
```

In addition to arbitrary tuples, it can now show arbitrary (non-recursive)
unions.

### Recursive types and values

TBD
