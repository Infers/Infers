// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

module Pretty =
  open Microsoft.FSharp.Reflection
  open System
  open System.Numerics
  open System.Text
  open Infers
  open Infers.Rep
  open PPrint

  type Pretty<'t> = 't -> Doc

  type Fixity =
    | Atomic
    | Part

  type [<AbstractClass>] InternalPretty<'t> () =
    abstract Pretty: byref<'t> -> Fixity * Doc

  type RecPretty<'x> () =
    inherit InternalPretty<'x> ()
    [<DefaultValue>] val mutable impl: InternalPretty<'x>
    override this.Pretty (x) = this.impl.Pretty (&x)

  type PrettyO<'t> = O of InternalPretty<'t>
  type PrettyP<'e, 'r, 'o, 't> = P of InternalPretty<'e>
  type PrettyS<'p, 'o, 't> = S of list<InternalPretty<'t>>

  [<AutoOpen>]
  module Util =
    let inline atom (d: Doc) = (Atomic, d)
    let inline part (d: Doc) = (Part, d)

    let inline atxt x = atom <| txt x
    let inline con x _ = x
    let inline doc fn =
      {new InternalPretty<'x> () with
        override this.Pretty (x) =
         fn x}
    let inline str fn = doc (fn >> txt >> atom)
    let inline fmt fmt = str (sprintf fmt) |> O
    let inline atomize (f, x) =
      match f with
       | Atomic -> x
       | Part -> gnest 1 <| parens x
    let inline just (_, x) = x

    let inline hexc c =
      let c = uint32 c
      if c < 0x10000u
      then sprintf "\\u%04x" c
      else sprintf "\\U%08x" c

    let commaLine = comma <^> line
    let semiLine = choice (semi <^> line) line

    let inline seq (o: string)
                   (c: string)
                   (xP: InternalPretty<'x>)
                   (toSeq: 'xs -> seq<'x>) =
      let i = o.Length
      let o = txt o
      let c = txt c
      O {new InternalPretty<'xs> () with
          override this.Pretty (xs) =
           toSeq xs
           |> Seq.map (fun (x: 'x) ->
                let mutable x = x
                xP.Pretty (&x) |> snd)
           |> joinSep semiLine
           |> gnest i
           |> enclose (o, c)
           |> atom}

  type [<Rep; Integral>] Pretty () =
    inherit Rules ()

    static member Enter (O p: PrettyO<'t>) : Pretty<'t> = fun x ->
      let mutable x = x
      just <| p.Pretty (&x)

    static member Rec () : Rec<PrettyO<'t>> =
      let r = RecPretty<'t> ()
      let o = O r
      {new Rec<PrettyO<'t>> () with
        override t.Get () = o
        override t.Set (O x) = r.impl <- x}

    static member Unit: PrettyO<unit> = O (doc (con (atxt "()")))

    static member Bool: PrettyO<bool> =
      let t = atxt "true"
      let f = atxt "false"
      O (doc (fun b -> if b then t else f))

    static member Integral (i: Integral<'t>) : PrettyO<'t> =
      O (str <| fun x -> x.ToString () + i.Suffix)

    static member Float32: PrettyO<float32> = fmt "%.9gf"
    static member Float64: PrettyO<float>   = fmt "%.17g"

    static member Char =
      let a = atxt "'\\''"
      let b = atxt "'\\b'"
      let n = atxt "'\\n'"
      let q = atxt "'\\\"'"
      let r = atxt "'\\r'"
      let s = atxt "'\\\\'"
      let t = atxt "'\\t'"
      O << doc <| function
       | '\'' -> a | '\b' -> b | '\n' -> n | '\"' -> q | '\r' -> r | '\\' -> s
       | '\t' -> t
       | c when Char.IsControl c -> hexc c |> txt |> squotes |> atom
       | c -> sprintf "'%c'" c |> atxt

    static member String: PrettyO<string> =
      O << doc <| fun s ->
        let mutable sawLF = false
        let mutable sawCR = false
        let wide =
          let sb = StringBuilder ()
          let inline S (s: string) = sb.Append s |> ignore
          let inline C (c: char) = sb.Append c |> ignore
          C '\"'
          for c in s do
            match c with
             | '\'' -> S"'"
             | '\b' -> S"\\b"
             | '\n' -> S"\\n" ; sawLF <- true
             | '\"' -> S"\\\""
             | '\r' -> S"\\r" ; sawCR <- true
             | '\\' -> S"\\\\"
             | '\t' -> S"\\t"
             | c when Char.IsControl c -> S (hexc c)
             | c -> C c
          C '\"'
          txt (sb.ToString ())
        let cutOnLF = sawLF
        let inline narrow () =
          let parts = ResizeArray<_> ()
          let sb = StringBuilder ()
          let cut () =
            parts.Add (txt (sb.ToString ()))
            sb.Clear () |> ignore
          let inline S (s: string) = sb.Append s |> ignore
          let inline C (c: char) = sb.Append c |> ignore
          C '\"'
          for c in s do
            match c with
             | '\'' -> S"'"
             | '\b' -> S"\\b"
             | '\n' ->
               if cutOnLF then
                 C '\\'
                 cut ()
               S"\\n"
             | '\"' -> S"\\\""
             | '\r' ->
               if not cutOnLF then
                 C '\\'
                 cut ()
               S"\\r"
             | '\\' -> S"\\\\"
             | '\t' -> S"\\t"
             | c when Char.IsControl c -> S (hexc c)
             | c -> C c
          C '\"'
          cut ()
          nest 1 (vcat parts)
        atom (if sawLF || sawCR then choice wide (delay narrow) else wide)

    static member Unsupported (_: Unsupported<'t>) : PrettyO<'t> =
      fmt "%A"

    static member Option (O tP) =
      let n = atxt "None"
      let s = txt "Some" <^> line
      O << doc <| function
       | None -> n
       | Some x ->
         let mutable x = x
         part (gnest 2 (s <^> atomize (tP.Pretty (&x))))

    static member Ref (O tP) =
      let con = txt "ref" <^> line
      O {new InternalPretty<ref<'t>> () with
          override t.Pretty (rx) =
           part (gnest 2 (con <^> just (tP.Pretty rx)))}

    static member List (O tP) = seq "[" "]" tP List.toSeq
    static member Array (O tP) = seq "[|" "|]" tP Array.toSeq

    static member Case (case: Case<Empty, 'o, 't>) : PrettyS<Empty, 'o, 't> =
      S [doc (con (atxt case.Name))]

    static member Case (case: Case<'p, 'o, 't>,
                        P lsP: PrettyP<'p, 'p, 'o, 't>) : PrettyS<'p, 'o, 't> =
      let con = txt case.Name <^> line
      S [doc <| fun x ->
         let mutable ls = Unchecked.defaultof<_>
         case.Extract (x, &ls)
         part (gnest 2 (con <^> atomize (lsP.Pretty (&ls))))]

    static member Choice (S pP: PrettyS<       'p,      Choice<'p, 'o>, 't>,
                          S oP: PrettyS<           'o ,            'o , 't>) =
      S (pP @ oP)             : PrettyS<Choice<'p, 'o>, Choice<'p, 'o>, 't>

    static member Sum (asC: AsChoices<'s, 't>, S sP: PrettyS<'s, 's, 't>) =
      let sP = Array.ofList sP
      O {new InternalPretty<'t> () with
          override t.Pretty x = sP.[asC.Tag x].Pretty (&x)}

    static member Item (_: Item<'e, 'r, 't>, O eP: PrettyO<'e>) =
      P eP : PrettyP<'e, 'r, 't, 't>

    static member Labelled (l: Labelled<'e, 'r, 'o, 't>, O eP: PrettyO<'e>) =
      let n = l.Name
      if n.StartsWith "Item" &&
         (let i = l.Index
          i = 0 && n.Length = 4 ||
          let suffix = string (i+1)
          n.Length = suffix.Length + 4 &&
          n.EndsWith suffix)
      then P eP : PrettyP<'e, 'r, 'o, 't>
      else let label = txt n <+> (equals <^> line)
           P {new InternalPretty<'e> () with
               override t.Pretty e =
                part (gnest 2 (label <^> just (eP.Pretty (&e))))}

    static member Pair (P eP: PrettyP<     'e,      Pair<'e, 'r>, 'o, 't>,
                        P rP: PrettyP<         'r ,          'r , 'o, 't>)
                            : PrettyP<Pair<'e, 'r>, Pair<'e, 'r>, 'o, 't> =
      let sep = if FSharpType.IsRecord typeof<'t> then semiLine else commaLine
      P {new InternalPretty<Pair<'e, 'r>> () with
          override t.Pretty ees =
           part (just (eP.Pretty (&ees.Elem)) <^>
                 (sep <^> just (rP.Pretty (&ees.Rest))))}

    static member Product (asP: AsPairs<'p,'t,'t>, P pP: PrettyP<'p,'p,'t,'t>) =
      let lr = if FSharpType.IsRecord typeof<'t> then lrbrace else lrparen
      O << doc <| fun t ->
      let mutable es = Unchecked.defaultof<_>
      asP.Extract (t, &es)
      atom (enclose lr (gnest 1 (just (pP.Pretty (&es)))))

  let pretty x = generateDFS<Pretty, _ -> Doc> x
  let show x = render None (pretty x)
