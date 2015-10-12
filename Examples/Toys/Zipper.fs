// Copyright (C) by Vesa Karvonen

module Toys.Zipper

open Infers
open Infers.Rep
open Toys.Rec
open Toys.Basic

type [<AbstractClass>] Zipper<'w> () =
  abstract Up: unit -> option<Zipper<'w>>
  abstract DownFirst: unit -> option<Zipper<'w>>
  abstract DownLast: unit -> option<Zipper<'w>>
  abstract Left: unit -> option<Zipper<'w>>
  abstract Right: unit -> option<Zipper<'w>>

type [<AbstractClass>] Zipper<'w, 'h> () =
  inherit Zipper<'w> ()
  abstract Get: unit -> 'h
  abstract Set: 'h -> Zipper<'w>

type [<AbstractClass>] ProductZipper<'w, 'l, 'h, 'r> () =
  inherit Zipper<'w, 'h> ()
  [<DefaultValue>] val mutable internal L: 'l
  [<DefaultValue>] val mutable internal H: 'h
  [<DefaultValue>] val mutable internal R: 'r
  override this.Get () = this.H

type [<AbstractClass>] Up<'w, 'h> () =
  abstract Up: 'h -> Zipper<'w>

type [<AbstractClass>] ProductUp<'w, 'l, 'h, 'r> () =
  inherit Up<'w, 'h> ()
  [<DefaultValue>] val mutable internal L: 'l
  [<DefaultValue>] val mutable internal R: 'r

type [<AbstractClass>] Down<'w, 'h> () =
  abstract DownFirst: Up<'w, 'h> * 'h -> option<Zipper<'w>>
  abstract DownLast: Up<'w, 'h> * 'h -> option<Zipper<'w>>

type [<AbstractClass;AllowNullLiteral>] ProductDownBase<'w, 'c, 'p, 't> () =
  [<DefaultValue>] val mutable internal L: ProductDownBase<'w, 'c, 'p, 't>
  [<DefaultValue>] val mutable internal R: ProductDownBase<'w, 'c, 'p, 't>
  abstract Down: Up<'w, 't> * AsProduct<'p, 't> * byref<'p> -> Zipper<'w>

type [<AbstractClass>] ProductDownElem<'w, 'l, 'h, 'r, 'sp, 'c, 'p, 't> () =
  inherit ProductDownBase<'w, 'c, 'p, 't> ()

type ProductDown<'w, 'e, 'sp, 'c, 'p, 't> =
  | P of list<ProductDownBase<'w, 'c, 'p, 't>>

type UnionDown<'w, 'c, 'cs, 'u> = U of list<Down<'w, 'u>>

type [<AbstractClass>] Rotate<'l, 'h, 'r, 'sp, 'p> () =
  abstract Do: byref<'p> * byref<'l> * byref<'h> * byref<'r> -> unit
  abstract Un: byref<'l> * byref<'h> * byref<'r> * byref<'p> -> unit

type [<InferenceRules>] Zipper () =
  member z.ToZipper (_: Rep, _: Basic, down: Down<'w, 'w>) =
    {new Up<'w, 'w> () with
      member u.Up w =
        {new Zipper<'w, 'w> () with
          member z.Get () = w
          member z.Set w = u.Up w
          member z.Up () = None
          member z.DownFirst () = down.DownFirst (u, w)
          member z.DownLast () = down.DownLast (u, w)
          member z.Left () = None
          member z.Right () = None} :> Zipper<_>}

  member z.Down () =
    let r = ref (Unchecked.defaultof<Down<_, _>>)
    {new Rec<Down<'w, 'h>> () with
      member t.Get () =
        {new Down<_, _> () with
          member d.DownFirst (up, h) = (!r).DownFirst (up, h)
          member d.DownLast (up, h) = (!r).DownLast (up, h)}
      member t.Set d = r := d}

  member z.Prim (_: Prim<'h>) =
    {new Down<'w, 'h> () with
      member d.DownFirst (_, _) = None
      member d.DownLast (_, _) = None}

  member z.Elem (_: Elem<'h, 'sp, 'c, 't>,
                 rotate: Rotate<'l, 'h, 'r, 'sp, 'p>,
                 down: Down<'w, 'h>) : ProductDown<'w, 'h, 'sp, 'c, 'p, 't> =
    P [{new ProductDownElem<'w, 'l, 'h, 'r, 'sp, 'c, 'p, 't> () with
         member d.Down (up, m, p) =
           let inline back (z: ProductZipper<_, _, _, _>) =
             let back = {new ProductUp<'w, 'l, 'h, 'r> () with
                          member u.Up h =
                            let mutable h = h
                            let mutable p = Unchecked.defaultof<'p>
                            rotate.Un (&u.L, &h, &u.R, &p)
                            d.Down (up, m, &p)}
             back.L <- z.L
             back.R <- z.R
             back
           let inline side (z: ProductZipper<_, _, _, _>)
                           (side: ProductDownBase<_, _, _, _>) =
             match side with
              | null -> None
              | side ->
                let mutable p = Unchecked.defaultof<'p>
                rotate.Un (&z.L, &z.H, &z.R, &p)
                side.Down (up, m, &p) |> Some
           let z = {new ProductZipper<'w, 'l, 'h, 'r> () with
                     member z.Set h =
                       let mutable h = h
                       let mutable p = Unchecked.defaultof<'p>
                       rotate.Un (&z.L, &h, &z.R, &p)
                       d.Down (up, m, &p)
                     member z.Up () =
                       let mutable p = Unchecked.defaultof<'p>
                       rotate.Un (&z.L, &z.H, &z.R, &p)
                       up.Up (m.Create (&p)) |> Some
                     member z.DownFirst () = down.DownFirst (back z, z.H)
                     member z.DownLast () = down.DownLast (back z, z.H)
                     member z.Left () = side z d.L
                     member z.Right () = side z d.R}
           rotate.Do (&p, &z.L, &z.H, &z.R)
           z :> Zipper<_>}]

  member z.Times (P e: ProductDown<'w,     'e     , And<'e, 'r>, 'c, 'p, 't>,
                  P r: ProductDown<'w,         'r ,         'r , 'c, 'p, 't>)
                     : ProductDown<'w, And<'e, 'r>, And<'e, 'r>, 'c, 'p, 't> =
    P (e @ r)

  member z.Product (m: AsProduct<'p, 't>,
                    P downs: ProductDown<'w, 'p, 'p, 'c, 'p, 't>)
                     : Down<'w, 't> =
    let downs = Array.ofList downs
    let n = downs.Length
    for i=0 to n-1 do
      if 0 <= i-1 then downs.[i].L <- downs.[i-1]
      if i+1 < n then downs.[i].R <- downs.[i+1]
    let downFirst = downs.[0]
    let downLast = downs.[n-1]
    {new Down<'w, 't> () with
      member d.DownFirst (up, t) =
        let mutable p = m.ToProduct t
        downFirst.Down (up, m, &p) |> Some
      member d.DownLast (up, t) =
        let mutable p = m.ToProduct t
        downLast.Down (up, m, &p) |> Some}

  member z.Case (_: Case<Empty, 'cs, 'u>) : UnionDown<'w, Empty, 'cs, 'u> =
    U [{new Down<'w, 'u> () with
         member d.DownFirst (_, _) = None
         member d.DownLast (_, _) = None}]

  member z.Case (m: Case<'ls, 'cs, 'u>,
                 downs: ProductDown<'w, 'ls, 'ls, 'cs, 'ls, 'u>)
                  : UnionDown<'w, 'ls, 'cs, 'u> =
    U [z.Product (m, downs)]

  member z.Plus (U c:  UnionDown<'w,        'c      , Choice<'c, 'cs>, 'u>,
                 U cs: UnionDown<'w,            'cs ,            'cs , 'u>)
                     : UnionDown<'w, Choice<'c, 'cs>, Choice<'c, 'cs>, 'u> =
    U (c @ cs)

  member z.Union (m: Union<'u>,
                  _: AsChoice<'c, 'u>,
                  U u: UnionDown<'w, 'c, 'c, 'u>) : Down<'w, 'u> =
    let cs = Array.ofList u
    {new Down<'w, 'u> () with
      override d.DownFirst (up, u) =
        cs.[m.Tag u].DownFirst (up, u)
      override d.DownLast (up, u) =
        cs.[m.Tag u].DownLast (up, u)}

  member r.Nest (r0: Rotate<'l, 'h, 'r, 'sp, 'p>) =
    {new Rotate<And<'l0, 'l>, 'h, 'r, 'sp, And<'l0, 'p>> () with
      member r1.Do (p, l, h, r) = l.Elem <- p.Elem; r0.Do (&p.Rest, &l.Rest, &h, &r)
      member r1.Un (l, h, r, p) = p.Elem <- l.Elem; r0.Un (&l.Rest, &h, &r, &p.Rest)}

  member r.The () =
    {new Rotate<Empty, 'h, 'r, And<'h, 'r>, And<'h, 'r>> () with
      member r1.Do (p, _, h, r) = h <- p.Elem; r <- p.Rest
      member r1.Un (_, h, r, p) = p.Elem <- h; p.Rest <- r}

  member r.Last () =
    {new Rotate<Empty, 'h, Empty, 'h, 'h> () with
      member r1.Do (p, _, h, _) = h <- p
      member r1.Un (_, h, _, p) = p <- h}

////////////////////////////////////////////////////////////////////////////////

let toZipper (x: 'x) : Zipper<'x> =
  (StaticRules<Zipper>.Generate() : Up<'x, 'x>).Up x

let get (xZ: Zipper<'x>) : option<'y> =
  match xZ with
    | :? Zipper<'x, 'y> as xyZ -> xyZ.Get () |> Some
    | _ -> None

let set (y: 'y) (xZ: Zipper<'x>) =
  match xZ with
   | :? Zipper<'x, 'y> as xyZ -> xyZ.Set y |> Some
   | _ -> None

let up (xZ: Zipper<'x>) = xZ.Up ()
let downFirst (xZ: Zipper<'x>) = xZ.DownFirst ()
let downLast (xZ: Zipper<'x>) = xZ.DownLast ()
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
  do let x = (1, 2.0)
     printfn "%A" x
     let z = toZipper x
     let z = z |> downFirst |> Option.get
     printfn "%d" (get z |> Option.get)
     let z = z |> right |> Option.get
     printfn "%f" (get z |> Option.get)
  do let x = (1, 2.0, "3")
     printfn "%A" x
     let z = toZipper x
     let z = z |> downLast |> Option.get
     printfn "%s" (get z |> Option.get)
     let z = z |> left |> Option.get
     printfn "%f" (get z |> Option.get)
     let z = set 2.5 z |> Option.get
     printfn "%f" (get z |> Option.get)
     let z = z |> left |> Option.get
     printfn "%d" (get z |> Option.get)
     printfn "%A" (fromZipper z)
  do let x = App (Lambda ("x", If (Var "x", Var "a", Var "a")), Var "true")
     printfn "%A" x
     //let n = Engine.numAttempts ()
     let z = toZipper x
     //printfn "Attempts: %d" (Engine.numAttempts () - n)
     printfn "%A" (get z |> Option.get : Term)
     let z = z |> downFirst |> Option.get
     printfn "%A" (get z |> Option.get : Term)
     let z = z |> downFirst |> Option.get
     printfn "%A" (get z |> Option.get : string)
     let z = z |> right |> Option.get
     printfn "%A" (get z |> Option.get : Term)
     let z = z |> downFirst |> Option.get
     printfn "%A" (get z |> Option.get : Term)
     let z = z |> right |> Option.get
     printfn "%A" (get z |> Option.get : Term)
     let z = z |> right |> Option.get
     printfn "%A" (get z |> Option.get : Term)
     let z = z |> set (Var "b") |> Option.get
     printfn "%A" (get z |> Option.get : Term)
     printfn "%A" (fromZipper z)
