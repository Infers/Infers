// Copyright (C) by Vesa Karvonen

module Toys.Zipper

open Infers
open Infers.Rep

type [<AbstractClass>] Zipper<'w> () =
  abstract DownHeadAny: unit -> option<Zipper<'w>>
  abstract DownHeadThe: unit -> option<Zipper<'w, 'w>>
  abstract DownLastAny: unit -> option<Zipper<'w>>
  abstract DownLastThe: unit -> option<Zipper<'w, 'w>>
  abstract NextAny: unit -> option<Zipper<'w>>
  abstract NextThe: unit -> option<Zipper<'w, 'w>>
  abstract PrevAny: unit -> option<Zipper<'w>>
  abstract PrevThe: unit -> option<Zipper<'w, 'w>>
  abstract Up: unit -> option<Zipper<'w>>

and [<AbstractClass>] Zipper<'w, 'h> () =
  inherit Zipper<'w> ()
  abstract Get: unit -> 'h
  abstract Set: 'h -> Zipper<'w, 'h>

type [<AbstractClass>] ZipperP<'w, 'v, 'h, 'n> () =
  inherit Zipper<'w, 'h> ()
  [<DefaultValue>] val mutable internal Prev: 'v
  [<DefaultValue>] val mutable internal Hole: 'h
  [<DefaultValue>] val mutable internal Next: 'n
  override this.Get () = this.Hole

type [<AbstractClass>] Up<'w, 'h> () =
  abstract Up: 'h -> Zipper<'w>

type [<AbstractClass>] UpP<'w, 'v, 'h, 'n> () =
  inherit Up<'w, 'h> ()
  [<DefaultValue>] val mutable internal Prev: 'v
  [<DefaultValue>] val mutable internal Next: 'n

type [<AbstractClass>] Down<'w, 't> () =
  abstract DownHeadAny: Up<'w, 't> * 't -> option<Zipper<'w>>
  abstract DownHeadThe: Up<'w, 't> * 't -> option<Zipper<'w, 'w>>
  abstract DownLastAny: Up<'w, 't> * 't -> option<Zipper<'w>>
  abstract DownLastThe: Up<'w, 't> * 't -> option<Zipper<'w, 'w>>

type [<AbstractClass;AllowNullLiteral>] DownPAny<'w, 'o, 'p, 't> () =
  [<DefaultValue>] val mutable internal PrevAny: DownPAny<'w,     'o, 'p, 't>
  [<DefaultValue>] val mutable internal PrevThe: DownPThe<'w, 'w, 'o, 'p, 't>
  [<DefaultValue>] val mutable internal NextAny: DownPAny<'w,     'o, 'p, 't>
  [<DefaultValue>] val mutable internal NextThe: DownPThe<'w, 'w, 'o, 'p, 't>
  abstract Down: Up<'w, 't> * AsPairs<'p, 'o, 't> * byref<'p> -> Zipper<'w>

and [<AbstractClass;AllowNullLiteral>] DownPThe<'w, 'h, 'o, 'p, 't> () =
  inherit DownPAny<'w, 'o, 'p, 't> ()

type [<AbstractClass>] DownPElem<'w, 'v, 'h, 'n, 'r, 'o, 'p, 't> () =
  inherit DownPThe<'w, 'h, 'o, 'p, 't> ()

type DownP<'w, 'e, 'r, 'o, 'p, 't> =
  | P of list<DownPAny<'w, 'o, 'p, 't>>

type DownS<'w, 'p, 'o, 't> = U of list<Down<'w, 't>>

type [<AbstractClass>] Rotate<'v, 'h, 'n, 'r, 'p> () =
  abstract Do: byref<'p> * byref<'v> * byref<'h> * byref<'n> -> unit
  abstract Un: byref<'v> * byref<'h> * byref<'n> * byref<'p> -> unit

type [<InferenceRules>] Zipper () =
  member z.ToZipper (_: Rep, wD: Down<'w, 'w>) =
    {new Up<'w, 'w> () with
      member u.Up w =
        {new Zipper<'w, 'w> () with
          member z.DownHeadAny () = wD.DownHeadAny (u, w)
          member z.DownHeadThe () = wD.DownHeadThe (u, w)
          member z.DownLastAny () = wD.DownLastAny (u, w)
          member z.DownLastThe () = wD.DownLastThe (u, w)
          member z.NextAny () = None
          member z.NextThe () = None
          member z.PrevAny () = None
          member z.PrevThe () = None
          member z.Get () = w
          member z.Set w = match u.Up w with
                            | :? Zipper<'w, 'w> as z -> z
                            | _ -> failwith "Bug"
          member z.Up () = None} :> Zipper<_>}

  member z.Down () =
    let r = ref <| Unchecked.defaultof<Down<_, _>>
    {new Rec<Down<'w, 'h>> () with
      member t.Get () =
        {new Down<_, _> () with
          member d.DownHeadAny (up, h) = (!r).DownHeadAny (up, h)
          member d.DownHeadThe (up, h) = (!r).DownHeadThe (up, h)
          member d.DownLastAny (up, h) = (!r).DownLastAny (up, h)
          member d.DownLastThe (up, h) = (!r).DownLastThe (up, h)}
      member t.Set d = r := d}

  member z.Prim (_: Prim<'t>) =
    {new Down<'w, 't> () with
      member d.DownHeadAny (_, _) = None
      member d.DownHeadThe (_, _) = None
      member d.DownLastAny (_, _) = None
      member d.DownLastThe (_, _) = None}

  member z.Elem (_: Elem<'h, 'r, 'o, 't>,
                 rotate: Rotate<'v, 'h, 'n, 'r, 'p>,
                 hD: Down<'w, 'h>) : DownP<'w, 'h, 'r, 'o, 'p, 't> =
    P [{new DownPElem<'w, 'v, 'h, 'n, 'r, 'o, 'p, 't> () with
         member d.Down (up, m, p) =
           let inline back (z: ZipperP<_, _, _, _>) =
             let b = {new UpP<'w, 'v, 'h, 'n> () with
                       member u.Up h =
                         let mutable h = h
                         let mutable p = Unchecked.defaultof<'p>
                         rotate.Un (&u.Prev, &h, &u.Next, &p)
                         d.Down (up, m, &p)}
             b.Prev <- z.Prev
             b.Next <- z.Next
             b
           let inline sideAny (z: ZipperP<_, _, _, _>)
                              (side: DownPAny<_, _, _, _>) =
             match side with
              | null -> None
              | side ->
                let mutable p = Unchecked.defaultof<'p>
                rotate.Un (&z.Prev, &z.Hole, &z.Next, &p)
                side.Down (up, m, &p) |> Some
           let inline sideThe (z: ZipperP<_, _, _, _>)
                              (side: DownPThe<_, _, _, _, _>) =
             match side with
              | null -> None
              | side ->
                let mutable p = Unchecked.defaultof<'p>
                rotate.Un (&z.Prev, &z.Hole, &z.Next, &p)
                match side.Down (up, m, &p) with
                 | :? Zipper<'w, 'w> as z -> Some z
                 | _ -> failwith "Bug"
           let z = {new ZipperP<'w, 'v, 'h, 'n> () with
                     member z.Set h =
                       let mutable h = h
                       let mutable p = Unchecked.defaultof<'p>
                       rotate.Un (&z.Prev, &h, &z.Next, &p)
                       match d.Down (up, m, &p) with
                        | :? Zipper<'w, 'h> as z -> z
                        | _ -> failwith "Bug"
                     member z.Up () =
                       let mutable p = Unchecked.defaultof<'p>
                       rotate.Un (&z.Prev, &z.Hole, &z.Next, &p)
                       up.Up (m.Create (&p)) |> Some
                     member z.DownHeadAny () = hD.DownHeadAny (back z, z.Hole)
                     member z.DownHeadThe () = hD.DownHeadThe (back z, z.Hole)
                     member z.DownLastAny () = hD.DownLastAny (back z, z.Hole)
                     member z.DownLastThe () = hD.DownLastThe (back z, z.Hole)
                     member z.NextAny () = sideAny z d.NextAny
                     member z.NextThe () = sideThe z d.NextThe
                     member z.PrevAny () = sideAny z d.PrevAny
                     member z.PrevThe () = sideThe z d.PrevThe}
           rotate.Do (&p, &z.Prev, &z.Hole, &z.Next)
           z :> Zipper<_>}]

  member z.Pair (P e: DownP<'w,      'e     , Pair<'e, 'r>, 'o, 'p, 't>,
                 P r: DownP<'w,          'r ,          'r , 'o, 'p, 't>)
                    : DownP<'w, Pair<'e, 'r>, Pair<'e, 'r>, 'o, 'p, 't> =
    P (e @ r)

  member z.Product (m: AsPairs<'p, 'o, 't>,
                    P downs: DownP<'w, 'p, 'p, 'o, 'p, 't>) =
    let downs = Array.ofList downs
    let n = downs.Length
    for i=0 to n-1 do
      if 0 <= i-1 then downs.[i].PrevAny <- downs.[i-1]
      if i+1 < n  then downs.[i].NextAny <- downs.[i+1]
    let mutable downLastThe = null
    for i=0 to n-1 do
      downs.[i].PrevThe <- downLastThe
      match downs.[i] with
       | :? DownPThe<'w, 'w, 'o, 'p, 't> as the -> downLastThe <- the
       | _ -> ()
    let downLastThe = downLastThe
    let mutable downHeadThe = null
    for i=n-1 downto 0 do
      downs.[i].NextThe <- downHeadThe
      match downs.[i] with
       | :? DownPThe<'w, 'w, 'o, 'p, 't> as the -> downHeadThe <- the
       | _ -> ()
    let downHeadThe = downHeadThe
    let downHeadAny = downs.[0]
    let downLastAny = downs.[n-1]
    let inline downAny (way: DownPAny<_, _, _, _>) up t =
      match way with
       | null -> None
       | way ->
         let mutable p = m.ToPairs t
         way.Down (up, m, &p) |> Some
    let inline downThe (way: DownPThe<_, _, _, _, _>) up t =
      match way with
       | null -> None
       | way ->
         let mutable p = m.ToPairs t
         match way.Down (up, m, &p) with
          | :? Zipper<'w, 'w> as z -> Some z
          | _ -> failwith "Bug"
    {new Down<'w, 't> () with
      member d.DownHeadAny (up, t) = downAny downHeadAny up t
      member d.DownHeadThe (up, t) = downThe downHeadThe up t
      member d.DownLastAny (up, t) = downAny downLastAny up t
      member d.DownLastThe (up, t) = downThe downLastThe up t}

  member z.Case (_: Case<Empty, 'o, 't>) : DownS<'w, Empty, 'o, 't> =
    U [{new Down<'w, 't> () with
         member d.DownHeadAny (_, _) = None
         member d.DownHeadThe (_, _) = None
         member d.DownLastAny (_, _) = None
         member d.DownLastThe (_, _) = None}]

  member z.Case (m: Case<'p, 'o, 't>,
                 pD: DownP<'w, 'p, 'p, 'o, 'p, 't>)
                   : DownS<'w, 'p, 'o, 't> =
    U [z.Product (m, pD)]

  member z.Choice (U pD: DownS<'w,        'p     , Choice<'p, 'o>, 't>,
                   U oD: DownS<'w,            'o ,            'o , 't>)
                       : DownS<'w, Choice<'p, 'o>, Choice<'p, 'o>, 't> =
    U (pD @ oD)

  member z.Sum (m: AsChoices<'s, 't>, U u: DownS<'w, 's, 's, 't>) =
    let cs = Array.ofList u
    {new Down<'w, 't> () with
      override d.DownHeadAny (up, u) = cs.[m.Tag u].DownHeadAny (up, u)
      override d.DownHeadThe (up, u) = cs.[m.Tag u].DownHeadThe (up, u)
      override d.DownLastAny (up, u) = cs.[m.Tag u].DownLastAny (up, u)
      override d.DownLastThe (up, u) = cs.[m.Tag u].DownLastThe (up, u)}

  member r.Nest (r0: Rotate<'v, 'h, 'n, 'r, 'p>) =
    {new Rotate<Pair<'v0, 'v>, 'h, 'n, 'r, Pair<'v0, 'p>> () with
      member r1.Do (p, l, h, r) =
        l.Elem <- p.Elem; r0.Do (&p.Rest, &l.Rest, &h, &r)
      member r1.Un (l, h, r, p) =
        p.Elem <- l.Elem; r0.Un (&l.Rest, &h, &r, &p.Rest)}

  member r.The () =
    {new Rotate<Empty, 'h, 'n, Pair<'h, 'n>, Pair<'h, 'n>> () with
      member r1.Do (p, _, h, r) = h <- p.Elem; r <- p.Rest
      member r1.Un (_, h, r, p) = p.Elem <- h; p.Rest <- r}

  member r.Last () =
    {new Rotate<Empty, 'h, Empty, 'h, 'h> () with
      member r1.Do (p, _, h, _) = h <- p
      member r1.Un (_, h, _, p) = p <- h}

////////////////////////////////////////////////////////////////////////////////

let rec fromZipper (xZ: Zipper<'x>) : 'x =
  match xZ.Up () with
   | None ->
     match xZ with
      | :? Zipper<'x, 'x> as xxZ -> xxZ.Get ()
      | _ -> failwith "Bug"
   | Some xZ ->
     fromZipper xZ

let toZipperAny (x: 'x) : Zipper<'x> =
  (StaticRules<Zipper>.Generate() : Up<'x, 'x>).Up x
let toZipperThe (x: 'x) : Zipper<'x, 'x> =
  match toZipperAny x with
   | :? Zipper<'x, 'x> as z -> z
   | _ -> failwith "Bug"

let inline getThe (xZ: Zipper<'x, 'x>) : 'x = xZ.Get ()
let getAny (xZ: Zipper<'x>) : option<'y> =
  match xZ with
   | :? Zipper<'x, 'y> as xyZ -> xyZ.Get () |> Some
   | _ -> None

let setAny (y: 'y) (xZ: Zipper<'x>) =
  match xZ with
   | :? Zipper<'x, 'y> as xyZ -> xyZ.Set y :> Zipper<_> |> Some
   | _ -> None
let inline setThe (x: 'x) (xZ: Zipper<'x, 'x>) = xZ.Set x

let mapAny (y2y: 'y -> 'y) (xZ: Zipper<'x>) =
  match xZ with
   | :? Zipper<'x, 'y> as xyZ -> xyZ.Get () |> y2y |> xyZ.Set :> Zipper<_>
   | _ -> xZ

let mapThe (x2x: 'x -> 'x) (xxZ: Zipper<'x, 'x>) =
  xxZ.Get () |> x2x |> xxZ.Set

let inline downHeadAny (xZ: Zipper<'x>) = xZ.DownHeadAny ()
let inline downHeadThe (xZ: Zipper<'x>) = xZ.DownHeadThe ()
let inline downLastAny (xZ: Zipper<'x>) = xZ.DownLastAny ()
let inline downLastThe (xZ: Zipper<'x>) = xZ.DownLastThe ()
let inline nextAny (xZ: Zipper<'x>) = xZ.NextAny ()
let inline nextThe (xZ: Zipper<'x>) = xZ.NextThe ()
let inline prevAny (xZ: Zipper<'x>) = xZ.PrevAny ()
let inline prevThe (xZ: Zipper<'x>) = xZ.PrevThe ()
let inline upAny (xZ: Zipper<'x>) = xZ.Up ()
let rec upThe (xZ: Zipper<'x>) : option<Zipper<'x, 'x>> =
  match upAny xZ with
   | None -> None
   | Some xZ ->
     match xZ with
      | :? Zipper<'x, 'x> as xZ -> Some xZ
      | _ -> upThe xZ

let inline moveQ (move: 'z -> option<'z>)
                 (qL: Lazy<'q>)
                 (z2b: 'z -> 'q)
                 (z: 'z) =
  match move z with
   | None -> qL.Force ()
   | Some z -> z2b z

let inline downHeadAnyQ x = moveQ downHeadAny x
let inline downHeadTheQ x = moveQ downHeadThe x
let inline downLastAnyQ x = moveQ downLastAny x
let inline downLastTheQ x = moveQ downLastThe x
let inline nextAnyQ x = moveQ nextAny x
let inline nextTheQ x = moveQ nextThe x
let inline prevAnyQ x = moveQ prevAny x
let inline prevTheQ x = moveQ prevThe x
let inline upAnyQ x = moveQ upAny x
let inline upTheQ x = moveQ upThe x

let inline moveT (moveInto: 'z -> option<'z>)
                 (moveBack: 'z -> option<'z>)
                 zL
                 z2z
                 (z: 'z) =
  moveQ moveInto zL (moveQ moveBack zL id << z2z) z

let inline downHeadAnyT f z = moveT downHeadAny upAny (lazy z) f z
let inline downHeadTheT f z = moveT downHeadThe upThe (lazy z) f z
let inline downLastAnyT f z = moveT downLastAny upAny (lazy z) f z
let inline downLastTheT f z = moveT downLastThe upThe (lazy z) f z
let inline nextAnyT f z = moveT nextAny prevAny (lazy z) f z
let inline nextTheT f z = moveT nextThe prevThe (lazy z) f z
let inline prevAnyT f z = moveT prevAny nextAny (lazy z) f z
let inline prevTheT f z = moveT prevThe nextThe (lazy z) f z
let upAnyT f z =
  let rec g z = moveT prevAny nextAny (lazy h z) g z
  and h z = moveT upAny downHeadAny (lazy z) f z
  g z
let upTheT f z =
  let rec g z = moveT prevThe nextThe (lazy h z) g z
  and h z = moveT upThe downHeadThe (lazy z) f z
  g z

let inline sibling (up: _ -> option<'z>) (down: _ -> option<'z>) (z: 'z) : 'z =
  match up z with
   | None -> z
   | Some z ->
     match down z with
      | None -> failwith "Bug"
      | Some z -> z

let inline headAny z = sibling upAny downHeadAny z
let inline headThe z = sibling upThe downHeadThe z
let inline lastAny z = sibling upAny downLastAny z
let inline lastThe z = sibling upThe downLastThe z

let rec mapBottomUpThe f z =
  let rec g z = nextTheT g (mapBottomUpThe f z)
  mapThe f (downHeadTheT g z)

let rec mapTopDownThe f z =
  let z = mapThe f z
  let rec g z = nextTheQ (lazy upTheQ (lazy z) g z) (mapTopDownThe f) z
  downHeadTheQ (lazy g z) (mapTopDownThe f) z
