// Copyright (C) by Vesa Karvonen

module Toys.Zipper

open Infers
open Infers.Rep
open Toys.Rec

type Zipper<'w> =
  abstract Up: unit -> option<Zipper<'w>>
  abstract Down: unit -> option<Zipper<'w>>
  abstract Left: unit -> option<Zipper<'w>>
  abstract Right: unit -> option<Zipper<'w>>

type Zipper<'w, 'h> =
  inherit Zipper<'w>
  abstract Get: unit -> 'h
  abstract Set: 'h -> Zipper<'w>

type Context<'w, 'h> = 'h -> Zipper<'w>

type Z<'w, 'h> = Context<'w, 'h> -> 'h -> option<Zipper<'w>>

type P0Z<'rs, 'w, 'p> =
  P0 of (Context<'w, 'rs> -> 'rs -> Zipper<'w>)
type PIZ<'ls, 'rs, 'w, 'p> =
  PI of (Context<'w, And<'ls, 'rs>> -> And<'ls, 'rs> -> Zipper<'w>)

type UZ<'c, 'cs, 'w, 'u> = U of list<Z<'w, 'u>>

let inline (|And|) (xxs: And<_, _>) = (xxs.Elem, xxs.Rest)

type [<InferenceRules>] Zipper () =
  member z.ToZipper (_: Rep,
                     _: Rec,
                     down: Z<'w, 'w>) : 'w -> Zipper<'w> =
    let rec mk w =
      {new Zipper<'w, 'w> with
        override z.Get () = w
        override z.Set w = mk w
        override z.Up () = None
        override z.Down () = z.Get () |> down z.Set
        override z.Left () = None
        override z.Right () = None} :> Zipper<_>
    mk

  member z.Prim (_: Prim<'t>) : Z<'w, 't> =
    fun tC ->
      let rec mk t =
        {new Zipper<'w, 't> with
          override z.Get () = t
          override z.Set t = mk t
          override z.Up () = None
          override z.Down () = None
          override z.Left () = None
          override z.Right () = None} :> Zipper<_>
      mk >> Some

  member z.Pos (_: Elem<'r, 'r, 'p>, down: Z<'w, 'r>) : P0Z<'r, 'w, 'p> =
    P0 <| fun wrC ->
      let rec mk r =
        {new Zipper<'w, 'r> with
          override z.Get () = r
          override z.Set r = r |> mk
          override z.Up () = r |> wrC |> Some
          override z.Down () = down z.Set r
          override z.Left () = None
          override x.Right () = None} :> Zipper<_>
      mk

  member z.Pos (_: Elem<'r, And<'r, 'rs>, 'p>,
                down: Z<'w, 'r>,
                PI right: PIZ<    'r, 'rs , 'w, 'p>)
                        : P0Z<And<'r, 'rs>, 'w, 'p> =
    P0 <| fun wrrsC ->
      let rec mk (And (r, rs)) =
        {new Zipper<'w, 'r> with
          override z.Get () = r
          override z.Set r = And (r, rs) |> mk
          override z.Up () = And (r, rs) |> wrrsC |> Some
          override z.Down () = down z.Set r
          override z.Left () = None
          override z.Right () = And (r, rs) |> right wrrsC |> Some} :> Zipper<_>
      mk

  member z.Pos (_: Elem<'r, 'r, 'p>,
                down: Z<'w, 'r>,
                P0 left: P0Z<And<'l, 'r>, 'w, 'p>)
                       : PIZ<    'l, 'r , 'w, 'p> =
    PI <| fun wlrC ->
      let rec mk (And (l, r)) =
        {new Zipper<'w, 'r> with
          override z.Get () = r
          override z.Set r = And (l, r) |> mk
          override z.Up () = And (l, r) |> wlrC |> Some
          override z.Down () = down z.Set r
          override z.Left () = And (l, r) |> left wlrC |> Some
          override z.Right () = None} :> Zipper<_>
      mk

  member z.Pos (_: Elem<'r, 'r, 'p>,
                down: Z<'w, 'r>,
                PI left: PIZ<        'ls , And<'l, 'r>, 'w, 'p>)
                       : PIZ<And<'l, 'ls>,         'r , 'w, 'p> =
    PI <| fun wllsrC ->
      let wlslrC (And (ls, And (l, r))) = And (And (l, ls), r) |> wllsrC
      let rec mk (And (And (l, ls), r)) =
        {new Zipper<'w, 'r> with
          override z.Get () = r
          override z.Set r = And (And (l, ls), r) |> mk
          override z.Up () = And (And (l, ls), r) |> wllsrC |> Some
          override z.Down () = down z.Set r
          override z.Left () = And (ls, And (l, r)) |> left wlslrC |> Some
          override z.Right () = None} :> Zipper<_>
      mk

  member z.Pos (_: Elem<'r, And<'r, 'rs>, 'p>,
                down: Z<'w, 'r>,
                P0 left : P0Z<             And<'l, And<'r, 'rs>>, 'w, 'p>,
                PI right: PIZ<And<'r, 'l>,                 'rs  , 'w, 'p>)
                        : PIZ<        'l ,         And<'r, 'rs> , 'w, 'p> =
    PI <| fun wlrrsC ->
      let wrlrsC (And (And (r, l), rs)) = And (l, And (r, rs)) |> wlrrsC
      let rec mk (And (l, And (r, rs))) =
        {new Zipper<'w, 'r> with
          override z.Get () = r
          override z.Set r = And (l, And (r, rs)) |> mk
          override z.Up () = And (l, And (r, rs)) |> wlrrsC |> Some
          override z.Down () = down z.Set r
          override z.Left () = And (l, And (r, rs)) |> left wlrrsC |> Some
          override z.Right () = And (And (r, l), rs) |> right wrlrsC |> Some} :> Zipper<_>
      mk

  member z.Pos (_: Elem<'r, And<'r, 'rs>, 'p>,
                down: Z<'w, 'r>,
                PI left : PIZ<                'ls  , And<'l, And<'r, 'rs>>, 'w, 'p>,
                PI right: PIZ<And<'r, And<'l, 'ls>>,                 'rs  , 'w, 'p>)
                        : PIZ<        And<'l, 'ls> ,         And<'r, 'rs> , 'w, 'p> =
    PI <| fun wllsrrsC ->
      let wlslrrsC (And (ls, (And (l, And (r, rs))))) =
        And (And (l, ls), And (r, rs)) |> wllsrrsC
      let wrllsrsC (And (And (r, And (l, ls)), rs)) =
        And (And (l, ls), And (r, rs)) |> wllsrrsC
      let rec mk (And (And (l, ls), And (r, rs))) =
        {new Zipper<'w, 'r> with
          override z.Get () = r
          override z.Set r = And (And (l, ls), And (r, rs)) |> mk
          override z.Up () = And (And (l, ls), And (r, rs)) |> wllsrrsC |> Some
          override z.Down () = down z.Set r
          override z.Left () =
            And (ls, And (l, And (r, rs))) |> left wlslrrsC |> Some
          override z.Right () =
            And (And (r, And (l, ls)), rs) |> right wrllsrsC |> Some} :> Zipper<_>
      mk

  member z.Product (m: AsProduct<'es, 'p>,
                    P0 down: P0Z<'es, 'w, 'p>) : Z<'w, 'p> =
    fun wpC -> m.ToProduct >> down (m.OfProduct >> wpC) >> Some

  member z.Case (_: Case<Empty, 'cs, 'u>) : UZ<Empty, 'cs, 'w, 'u> =
    U [fun u wuC -> None]

  member z.Case (m: Case<'ls, 'cs, 'u>,
                 P0 down: P0Z<'ls, 'w, 'u>) : UZ<'ls, 'cs, 'w, 'u> =
    U [fun wuC -> m.ToProduct >> down (m.OfProduct >> wuC) >> Some]

  member z.Plus (U c:  UZ<       'c      , Choice<'c, 'cs>, 'w, 'u>,
                 U cs: UZ<           'cs ,            'cs , 'w, 'u>)
                     : UZ<Choice<'c, 'cs>, Choice<'c, 'cs>, 'w, 'u> =
    U (c @ cs)

  member z.Union (m: Union<'u>,
                  _: AsChoice<'c, 'u>,
                  U u: UZ<'c, 'c, 'w, 'u>) : Z<'w, 'u> =
    let cs = Array.ofList u
    fun wuC u -> cs.[m.Tag u] wuC u

////////////////////////////////////////////////////////////////////////////////

let toZipper (x: 'x) : Zipper<'x> =
  StaticRules<Zipper>.Generate() x

let get (xZ: Zipper<'x>) : option<'y> =
  match xZ with
    | :? Zipper<'x, 'y> as xyZ -> xyZ.Get () |> Some
    | _ -> None

let set (y: 'y) (xZ: Zipper<'x>) =
  match xZ with
   | :? Zipper<'x, 'y> as xyZ -> xyZ.Set y |> Some
   | _ -> None

let up (xZ: Zipper<'x>) = xZ.Up ()
let down (xZ: Zipper<'x>) = xZ.Down ()
let left (xZ: Zipper<'x>) = xZ.Left ()
let right (xZ: Zipper<'x>) = xZ.Right ()

let rec fromZipper (xZ: Zipper<'x>) : 'x =
  match up xZ with
   | None ->
     match xZ with
      | :? Zipper<'x, 'x> as xxZ -> xxZ.Get ()
      | _ -> failwith "Bug"
   | Some xZ ->
     fromZipper xZ

////////////////////////////////////////////////////////////////////////////////

type Term =
  | Var of string
  | Lambda of string * Term
  | App of Term * Term
  | If of Term * Term * Term

let test () =
  do let x = 1
     printfn "%A" x
     let z = toZipper x
     printfn "%d" (get z |> Option.get)
  do let x = (1, 2.0, "3")
     printfn "%A" x
     let z = toZipper x
     let z = z |> down |> Option.get
     printfn "%d" (get z |> Option.get)
     let z = z |> right |> Option.get
     printfn "%f" (get z |> Option.get)
     let z = z |> right |> Option.get
     printfn "%s" (get z |> Option.get)
  do let x = App (Lambda ("x", If (Var "x", Var "a", Var "a")), Var "true")
     printfn "%A" x
     let z = toZipper x
     let z = z |> down |> Option.get
     let z = z |> down |> Option.get
     let z = z |> right |> Option.get
     let z = z |> down |> Option.get
     let z = z |> right |> Option.get
     let z = z |> right |> Option.get
     let z = z |> set (Var "b") |> Option.get
     printfn "%A" (fromZipper z)
