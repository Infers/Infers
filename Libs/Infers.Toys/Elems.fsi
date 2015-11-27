// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

/// Provides generic functions very much like Neil Mitchell's Uniplate.
///
/// The operations in the `Elems` module work most conveniently on recursive sum
/// of products types of the form
///
///> type SoP<...> =
///>  | Case1 of Elem<I1,J1> * ... * Elem<I1,Jm1>
///>  | ...
///>  | CaseN of Elem<In,J1> * ... * Elem<Im,Jmn>
///
/// where the `Elem<_,_>` types are not compound types such as `list<SoP<...>>`
/// or `SoP<...> * SoP<...>`.  This is because the primitive `elems` and `subst`
/// operations do not look inside the `Elem<_,_>` types.
///
/// In the documentation we will make use of the following type:
///
///> type BinTr<'x> =
///>   | Lf
///>   | Br of BinTr<'x> * 'x * BinTr<'x>
///
/// Note that `Elems` is not limited to manipulating binary trees.  Elems will
/// work with any recursive or non-recursive union or product type.  Binary tree
/// is merely used as an example of a simple recursive datatype.
[<AutoOpen>]
module Elems =
  /// `elems<'h, 'w> w` returns an array of the immediate elements of type `'h`
  /// in `w`.
#if DOC
  ///
  /// By design, `elems` does not look inside the elements of `'w`.  The
  /// following examples should help to clarify what this means:
  ///
  ///> elems<int, _> Lf = [||]
  ///> elems<int, _> (Br (Lf, 1, Lf)) = [|1|]
  ///> elems<int, _> (Br (Lf, (1, 2), Lf)) = [||]
  ///> elems<int * int, _> (Br (Lf, (1, 2), Lf)) = [|(1, 2)|]
  ///
  /// Note that any type of elements `'h` can be queried, including the input
  /// type `'w`.  For example, `elems<BinTr<int>, _> (Br (Lf, 1, Lf))` gives
  /// `[|Lf; Lf|]`.
#endif
  val elems<'h, 'w> : 'w -> array<'h>

  /// `children` is equivalent to `elems<'w, 'w>`.  `children` only makes sense
  /// when applied to a recursive union or record type.  For example, `children
  /// (Br (Lf, 1, Br (Lf, 2, Lf)))` gives `[|Lf; Br (Lf, 2, Lf)|]`.
  val inline children: 'w -> array<'w>

  /// `elemsDn<'h, 'w> w` returns a sequence of all the `elems` of type `'h` in
  /// `w` and recursively in the `children` of `w`.
#if DOC
  ///
  /// For example,
  ///
  ///> elemsDn<int, _> (Br (Br (Lf, 1, Br (Lf, 2, Lf)), 3, Br (Lf, 4, Lf)))
  ///
  /// gives the sequence
  ///
  ///> seq [3; 1; 2; 4]
#endif
  val elemsDn<'h, 'w> : 'w -> seq<'h>

  /// `universe w` is equivalent to `Seq.append (Seq.singleton w) (elemsDn w)`.
#if DOC
  ///
  /// For example,
  ///
  ///> universe (Br (Br (Lf, 1, Br (Lf, 2, Lf)), 3, Br (Lf, 4, Lf)))
  ///
  /// gives the sequence
  ///
  ///> seq [Br (Br (Lf, 1, Br (Lf, 2, Lf)), 3, Br (Lf, 4, Lf))
  ///>      Br (Lf, 1, Br (Lf, 2, Lf))
  ///>      Br (Lf, 4, Lf)
  ///>      Lf
  ///>      Br (Lf, 2, Lf)
  ///>      Lf
  ///>      Lf
  ///>      Lf
  ///>      Lf]
#endif
  val universe: 'w -> seq<'w>

  /// `para w2rs2r w` performs a fold-like computation called a paramorphism.
#if DOC
  ///
  /// For example,
  ///
  ///> let height t = para (fun _ hs -> Array.fold max 0 hs + 1) t
  ///
  /// computes the "height" of any recursive sum of products type.  For
  /// example, `height` gives
  ///
  ///> height Lf = 1
  ///> height [] = 1
  ///> height ("Non", ("recursive", "type")) = 1
  ///> height (Br (Lf, 1, Br (Lf, 2, Lf))) = 3
  ///> height [1;1;1] = 4
#endif
  val para: ('w -> array<'r> -> 'r) -> 'w -> 'r

  //////////////////////////////////////////////////////////////////////////////

  /// `subst h2h w` returns a new value of type `'w` which is like `w` except
  /// that every immediate element `h_i` of type `'h` in `w` is replaced with
  /// `h2h h_i`.  `elems >> Array.map h2h` is equivalent to `subst h2h >>
  /// elems`.
  val subst<'h, 'w> : ('h -> 'h) -> 'w -> 'w

  /// `descend` is equivalent to `subst<'w, 'w>`.  `descend` only makes sense
  /// when applied to a recursive sum type.
  val inline descend: ('w -> 'w) -> 'w -> 'w

  /// `substUp<'h, 'w> h2h w` performs a bottom-up transformation of the given
  /// value `w`, recursively `descend`ing into `w` and then `subst`ituting with
  /// `h2h`.
  ///
  /// For example,
  ///
  ///> substUp ((+) 1) [1; 2; 3] = [2; 3; 4]
  ///
  /// and
  ///
  ///> substUp (fun xs -> 0::xs) [1; 2; 3] = [1; 0; 2; 0; 3; 0]
  val substUp<'h, 'w> : ('h -> 'h) -> 'w -> 'w

  /// `transform w2w w` is equivalent to `w |> substUp w2w |> w2w`.
  ///
  /// For example, given type of a binary trees
  ///
  ///> type BinTr<'x> =
  ///>   | Lf
  ///>   | Br of BinTr<'x> * 'x * BinTr<'x>
  ///
  /// we can write a function to mirror trees
  ///
  ///> let mirror bt =
  ///>   transform <| function Lf -> Lf
  ///>                       | Br (l, x, r) -> Br (r, x, l)
  ///>             <| bt
  ///
  /// Now
  ///
  ///> Br (Br (Lf, 1, Lf), 2, Br (Br (Lf, 3, Lf), 4, Lf)) |> mirror
  ///
  /// evaluates to
  ///
  ///> Br (Br (Lf, 4, Br (Lf, 3, Lf)), 2, Br (Lf, 1, Lf))
  val inline transform: ('w -> 'w) -> 'w -> 'w

  /// Transforms with given partial function until a fixed point is reached.
  val rewrite: ('w -> option<'w>) -> 'w -> 'w
