# Infers

Infers is a library for deriving F# values from their types.

The basic idea is to view the types of F# member functions as
[Horn clauses](http://en.wikipedia.org/wiki/Horn_clause).  Using a
[Prolog](http://en.wikipedia.org/wiki/Prolog)-style resolution engine it is then
possible to attempt to prove goals given as simple F# types.  During the
resolution process the engine invokes the member functions to build a value of
the type given as the goal.

Infers can be useful, for example, in situations where one might wish to use
something like [type classes](http://en.wikipedia.org/wiki/Type_class) or when
one might want to do *datatype generic* programming.  Other kinds of
applications are also quite possible.

## Download

An experimental build of [Infers](http://www.nuget.org/packages/Infers/) is
available via [NuGet](http://www.nuget.org/).  Infers is not yet ready for
production use.

## Documentation

To gain an understanding on what Infers can do, you might want start with the
[Infers Tutorial](Docs/Tutorial.md).
