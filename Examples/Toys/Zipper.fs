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
  [<DefaultValue>] val mutable L: 'l
  [<DefaultValue>] val mutable H: 'h
  [<DefaultValue>] val mutable R: 'r
  override this.Get () = this.H

type [<AbstractClass>] Up<'w, 'h> () =
  abstract Up: 'h -> Zipper<'w>

type [<AbstractClass>] ProductUp<'w, 'l, 'h, 'r> () =
  inherit Up<'w, 'h> ()
  [<DefaultValue>] val mutable L: 'l
  [<DefaultValue>] val mutable R: 'r

type [<AbstractClass>] Down<'w, 'h> () =
  abstract DownFirst: Up<'w, 'h> * 'h -> option<Zipper<'w>>
  abstract DownLast: Up<'w, 'h> * 'h -> option<Zipper<'w>>

type [<AbstractClass;AllowNullLiteral>] ProductDown<'w, 'l, 'h, 'r, 'sp, 'c, 'p, 't> () =
  abstract Down: Up<'w, 't> * AsProduct<'p, 't> * byref<'p> -> Zipper<'w>

type UnionDown<'w, 'c, 'cs, 'u> = U of list<Down<'w, 'u>>

type [<AbstractClass>] Rotate<'l, 'h, 'r, 'sp, 'p> () =
  abstract Do: byref<'p> * byref<'l> * byref<'h> * byref<'r> -> unit
  abstract Un: byref<'l> * byref<'h> * byref<'r> * byref<'p> -> unit

type Out = | Out'0

type Prev<'h, 'sp, 'p, 'lh, 'lsp> = | Prev'5
type Head<'h, 'sp> = | Head'2
type Next<'h, 'sp, 'rh, 'rsp> = | Next'4

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

  member z.ProductDown () =
    let r = ref (Unchecked.defaultof<ProductDown<_, _, _, _, _, _, _, _>>)
    {new Rec<ProductDown<'w, 'l, 'h, 'r, 'sp, 'c, 'p, 't>> () with
      member t.Get () =
        {new ProductDown<_, _, _, _, _, _, _, _> () with
          member d.Down (up, m, p) = (!r).Down(up, m, &p)}
      member t.Set d = r := d}

  member z.Prim (_: Prim<'h>) =
    {new Down<'w, 'h> () with
      member d.DownFirst (_, _) = None
      member d.DownLast (_, _) = None}

  member z.Elem () : ProductDown<'w, Out, Out, Out, Out, 'c, 'p, 't> =
    null

  member z.Elem (_: Elem<'h, 'sp, 'c, 't>,
                 _: Prev<'h, 'sp, 'p, 'lh, 'lsp>,
                 _: Next<'h, 'sp, 'rh, 'rsp>,
                 rotate: Rotate<'l, 'h, 'r, 'sp, 'p>,
                 down: Down<'w, 'h>,
                 left: ProductDown<'w, 'll, 'lh, 'lr, 'lsp, 'c, 'p, 't>,
                 right: ProductDown<'w, 'rl, 'rh, 'rr, 'rsp, 'c, 'p, 't>) =
    {new ProductDown<'w, 'l, 'h, 'r, 'sp, 'c, 'p, 't> () with
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
                        (side: ProductDown<_, _, _, _, _, _ ,_ , _>) =
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
                  member z.Left () = side z left
                  member z.Right () = side z right}
        rotate.Do (&p, &z.L, &z.H, &z.R)
        z :> Zipper<_>}

  member z.Product (m: AsProduct<'p, 't>,
                    downFirst: ProductDown<'w, Empty, _, _, 'p, _, 'p, 't>,
                    downLast: ProductDown<'w, _, 'lh, Empty, 'lh, _, 'p, 't>)
                     : Down<'w, 't> =
    {new Down<'w, 't> () with
      member d.DownFirst (up, t) =
        match downFirst with
         | null -> None
         | downFirst ->
           let mutable p = m.ToProduct t
           downFirst.Down (up, m, &p) |> Some
      member d.DownLast (up, t) =
        match downLast with
         | null -> None
         | downLast ->
           let mutable p = m.ToProduct t
           downLast.Down (up, m, &p) |> Some}

  member z.Case (_: Case<Empty, 'cs, 'u>) : UnionDown<'w, Empty, 'cs, 'u> =
    U [{new Down<'w, 'u> () with
         member d.DownFirst (_, _) = None
         member d.DownLast (_, _) = None}]

  member z.Case (m: Case<'ls, 'cs, 'u>,
                 downFirst: ProductDown<'w, Empty, _, _, 'ls, 'cs, 'ls, 'u>,
                 downLast: ProductDown<'w, _, 'lh, Empty, 'lh, 'cs, 'ls, 'u>)
                  : UnionDown<'w, 'ls, 'cs, 'u> =
    U [z.Product (m, downFirst, downLast)]

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

  member r.Prev (_: Prev<'h, 'sp, 'p, 'l, 'lsp>)
                  : Prev<'h, 'sp, And<'x, 'p>, 'l, 'lsp> = Prev'5

  member r.Prev (): Prev<'h, 'sp, And<'l, 'sp>, 'l, And<'l, 'sp>> = Prev'5

  member r.PrevNone (_: Head<'h, 'sp>)
                      : Prev<'h, 'sp, 'sp, Out, Out> = Prev'5

  member r.Next (_: Head<'n, 'r>)
                  : Next<'h, And<'h, 'r>, 'n, 'r> = Next'4
  member r.NextNone () : Next<'l, 'l, Out, Out> = Next'4

  member r.HeadElem () : Head<'h, And<'h, 'r>> = Head'2
  member r.HeadLast () : Head<'h, 'h> = Head'2

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
