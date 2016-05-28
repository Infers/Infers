// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open System
open System.IO
open System.Text
open System.Collections.Generic
open Infers
open Infers.Rep
open FParsec.Primitives
open FParsec.CharParsers
open PPrint

module Json =
  type AST =
    |  Object of Dictionary<string, AST>
    |   Array of ResizeArray<AST>
    |  String of string
    |  Number of string
    | Boolean of bool
    |    Null

  type N = NumberLiteralOptions

  let astTrue = Boolean true
  let astFalse = Boolean false

  let inline p c = skipChar c .>> spaces
  let tColon = p ':'
  let tBraceL = p '{'
  let tBraceR = p '}'
  let tBracketL = p '['
  let tBracketR = p ']'
  let tComma = p ','

  let pNull =
    skipString "null" .>> spaces
  let pBoolean =
    choiceL [skipString "false" >>% false
             skipString "true"  >>% true] "boolean"
  let pNumber =
    numberLiteral (N.AllowMinusSign ||| N.AllowFraction ||| N.AllowExponent)
     "number" |>> (fun n -> n.String) .>> spaces
  let pString =
    let uEscape =
      pipe4 hex hex hex hex <| fun d1 d2 d3 d4 ->
      let inline d d s =
        s * 16 + int d - (if   '0' <= d && d <= '9' then int '0'
                          elif 'a' <= d && d <= 'f' then int 'a' - 10
                          else                           int 'A' - 10)
      0 |> d d1 |> d d2 |> d d3 |> d d4 |> char
    skipChar '\"' >>= fun () ->
    let b = StringBuilder ()
    let (outside, outside') = createParserForwardedToRef ()
    let (escaped, escaped') = createParserForwardedToRef ()
    let app (c: char) = b.Append c |> ignore ; outside
    outside' :=
      anyChar >>= function
       | '\"' -> spaces >>% b.ToString ()
       | '\\' -> escaped
       | c -> app c
    escaped' :=
      anyChar >>= function
       | '\"' -> app '\"'
       | '\\' -> app '\\'
       | '/'  -> app '/'
       | 'b'  -> app '\b'
       | 'f'  -> app '\f'
       | 'n'  -> app '\n'
       | 'r'  -> app '\r'
       | 't'  -> app '\t'
       | 'u'  -> uEscape >>= app
       | c -> sprintf "Invalid escape: %A" c |> fail
    outside
  let pObject pV =
    tBraceL >>.
    Inline.SepBy (fun (k, v) -> let d = Dictionary () in d.Add (k, v) ; d
                , fun d _ (k, v) -> d.Add (k, v) ; d
                , id
                , pString .>> tColon .>>. pV
                , tComma) .>>
    tBraceR
  let pArray pV =
    tBracketL >>.
    Inline.SepBy (fun x -> let xs = ResizeArray () in xs.Add x ; xs
                , fun xs _ x -> xs.Add x ; xs
                , id
                , pV
                , tComma) .>>
    tBracketR
  let pAST =
    let (pAST, pAST') = createParserForwardedToRef ()
    pAST' :=
      choiceL [pObject pAST |>> Object
               pArray pAST  |>> Array
               pString      |>> String
               pNumber      |>> Number
               pBoolean     |>> function true -> astTrue | false -> astFalse
               pNull         >>%Null] "value"
    spaces >>. pAST

  //////////////////////////////////////////////////////////////////////////////

  let colonLn = colon <^> line
  let commaLn = comma <^> line
  let inline isPrintable c = 32 <= int c && int c <= 255

  let fTrue = txt "true"
  let fFalse = txt "false"

  let fNull = txt "null"
  let fBoolean b = if b then fTrue else fFalse
  let fNumber = txt
  let fString s =
    let b = StringBuilder ()
    let inline appc (c: char) = b.Append c |> ignore
    let inline apps (s: string) = b.Append s |> ignore
    appc '"'
    String.iter
     <| function
         | '\"' -> apps "\\\""
         | '\\' -> apps "\\\\"
         | '\b' -> apps "\\b"
         | '\f' -> apps "\\f"
         | '\n' -> apps "\\n"
         | '\r' -> apps "\\r"
         | '\t' -> apps "\\t"
         | c ->
           if isPrintable c then
             appc c
           else
             let inline d x =
               let c = (x >>> 12) &&& 0xF
               (if c < 10 then int '0' else int 'a' - 10) + c |> char |> appc
               x <<< 4
             apps "\\u"
             int c |> d |> d |> d |> d |> ignore
     <| s
    appc '"'
    b.ToString () |> txt
  let fArray fV vs =
    vs |> Seq.map fV |> punctuate commaLn |> hcat |> brackets |> gnest 1
  let fObject fV kvs =
    kvs
    |> Seq.map ^ fun (kv: KeyValuePair<_, _>) ->
         gnest 1 (fString kv.Key <^> colonLn <^> fV kv.Value)
    |> punctuate commaLn |> hcat |> braces |> gnest 1

  let rec fAST = function
    | Null       -> fNull
    | Boolean b  -> fBoolean b
    | Number s   -> fNumber s
    | String s   -> fString s
    | Array vs   -> fArray fAST vs
    | Object kvs -> fObject fAST kvs

  //////////////////////////////////////////////////////////////////////////////

  type Is<'t> = | Is

  type Error = unit -> string

  type [<AbstractClass>] Json<'t> () =
    abstract parse: AST -> Choice<'t, Error>
    abstract toAST: 't -> AST

  type [<AbstractClass>] JsonR<'e,'r,'o,'t> () =
    abstract parse: Dictionary<string, AST> * byref<'e> -> option<Error>
    abstract toAST: byref<'e> * Dictionary<string, AST> -> unit

  type [<AbstractClass>] JsonT<'e,'r,'t> () =
    abstract count: int
    abstract parse: ResizeArray<AST> * int * byref<'e> -> option<Error>
    abstract toAST: byref<'e> * ResizeArray<AST> -> unit

  type JsonS<'p,'o,'t> = S of list<Json<'t>>

  let number (ofString: string -> 't) (toString: 't -> string) =
    {new Json<'t> () with
      member __.parse ast =
       match ast with
        | Number s -> Choice1Of2 ^ ofString s
        | _        -> Choice2Of2 ^ fun () -> "number"
      member __.toAST t = Number ^ toString t}

  let seq (tJ: Json<'t>)
          (ofResizeArray: ResizeArray<'t> -> 'ts when 'ts :> seq<'t>) =
    {new Json<'ts> () with
      member __.parse ast =
        match ast with
         | Array js ->
           let n = js.Count
           let ts = ResizeArray n
           let rec lp i =
             if i < n then
               match tJ.parse js.[i] with
                | Choice1Of2 v ->
                  ts.Add v
                  lp (i+1)
                | Choice2Of2 expected ->
                  Choice2Of2 ^ fun () -> "[" + expected () + "]"
             else
               Choice1Of2 ^ ofResizeArray ts
           lp 0
         | _ ->
           Choice2Of2 ^ fun () -> "[ ... ]"
      member __.toAST xs = Seq.map tJ.toAST xs |> ResizeArray |> Array}

  let dict (tJ: Json<'t>)
           (ofDictionary: Dictionary<string, 't> -> 'kvs
                           when 'kvs :> seq<KeyValuePair<string, 't>>) =
    {new Json<'kvs> () with
      member __.parse ast =
        match ast with
         | Object o ->
           let d = Dictionary<_, _> ()
           use e = (o :> seq<_>).GetEnumerator ()
           let rec lp () =
             if e.MoveNext () then
               let kv = e.Current
               match tJ.parse kv.Value with
                | Choice1Of2 v ->
                  d.Add (kv.Key, v)
                  lp ()
                | Choice2Of2 expected ->
                  Choice2Of2 ^ fun () -> "{\"...\":" + expected () + "}"
             else
               Choice1Of2 ^ ofDictionary d
           lp ()
         | _ ->
           Choice2Of2 ^ fun () -> "{ ... }"
      member __.toAST kvs =
        let d = Dictionary ()
        kvs
        |> Seq.iter ^ fun kv ->
             d.Add (kv.Key, tJ.toAST kv.Value)
        Object d}

  type [<Rep>] Json () =
    inherit Rules ()

    static member Null =
      {new Json<unit> () with
        member __.parse ast =
         match ast with
          | Null -> Choice1Of2 ()
          | _    -> Choice2Of2 ^ fun () -> "null"
        member __.toAST _ = Null}

    static member Bool =
      {new Json<bool> () with
        member __.parse ast =
         match ast with
          | Boolean b -> Choice1Of2 b
          | _         -> Choice2Of2 ^ fun () -> "bool"
        member __.toAST b = Boolean b}

    static member Int32 = number int string
    static member Int64 = number int64 string

    static member Float =
      number float ^ fun x ->
      if Double.IsNaN x || Double.IsInfinity x
      then "null"
      else sprintf "%.17g" x

    static member String =
      {new Json<string> () with
        member __.parse ast =
         match ast with
          | String s -> Choice1Of2 s
          | _        -> Choice2Of2 ^ fun () -> "string"
        member __.toAST s = String s}

    static member Literal () =
      let name = typeof<'t>.Name
      let nameJ = String name
      {new Json<Is<'t>> () with
        member __.parse ast =
         match ast with
          | String s when s = name -> Choice1Of2 Is
          | _                      -> Choice2Of2 ^ fun () -> name
        member __.toAST _ = nameJ}

    static member ResizeArray (tJ: Json<'t>) = seq tJ id
    static member Array       (tJ: Json<'t>) = seq tJ Array.ofSeq
    static member List        (tJ: Json<'t>) = seq tJ List.ofSeq

    static member Dictionary (tJ: Json<'t>) = dict tJ id
    static member Map        (tJ: Json<'t>) = dict tJ Map.ofDictionary

    static member Item (_: Item<'e,'r,'t>, eJ: Json<'e>) =
      {new JsonT<'e,'r,'t> () with
        member __.count = 1
        member __.parse (ast, i, e) =
         match eJ.parse ast.[i] with
          | Choice1Of2 v -> e <- v ; None
          | Choice2Of2 e -> Some e
        member __.toAST (e, ast) =
         ast.Add ^ eJ.toAST e}

    static member Pair (eJ: JsonT<'e,Pair<'e,'r>,'t>,
                        rJ: JsonT<'r,        'r ,'t>) =
      let eCount = eJ.count
      let count = eCount + rJ.count
      {new JsonT<Pair<'e,'r>,Pair<'e,'r>,'t> () with
        member __.count = count
        member __.parse (ast, i, er) =
          match eJ.parse (ast, i, &er.Elem) with
           | None -> rJ.parse (ast, i+eCount, &er.Rest)
           | some -> some
        member __.toAST (er, ast) =
          eJ.toAST (&er.Elem, ast)
          rJ.toAST (&er.Rest, ast)}

    static member Product (asP: AsPairs<'p,'o,'t>, pJ: JsonT<'p,'p,'t>) =
      {new Json<'t> () with
        member __.parse ast =
          match ast with
           | Array js ->
             let mutable p = Unchecked.defaultof<_>
             match pJ.parse (js, 0, &p) with
              | None -> asP.Create (&p) |> Choice1Of2
              | Some e -> Choice2Of2 ^ fun () ->  "[" + e () + "]"
           | _ -> Choice2Of2 ^ fun () -> "[ ... ]"
        member __.toAST t =
         let mutable p = asP.ToPairs t
         let ast = ResizeArray pJ.count
         pJ.toAST (&p, ast)
         Array ast}

    static member Elem (eL: Labelled<'e,'r,'o,'t>, eJ: Json<'e>) =
      {new JsonR<'e,'r,'o,'t> () with
        member __.parse (d, e) =
          match d.TryGetValue eL.Name with
           | true, v ->
             match eJ.parse v with
              | Choice1Of2 v -> e <- v ; None
              | Choice2Of2 e -> Some ^ fun () -> eL.Name + ":" + e ()
           | _ ->
             Some ^ fun () -> eL.Name + ": ..."
        member __.toAST (e, d) =
          d.Add (eL.Name, eJ.toAST e)}

    static member OptElem (eL: Labelled<option<'e>,'r,'o,'t>, eJ: Json<'e>) =
      {new JsonR<option<'e>,'r,'o,'t> () with
        member __.parse (d, eO) =
         match d.TryGetValue eL.Name with
          | true, v ->
            match eJ.parse v with
             | Choice1Of2 v -> eO <- Some v; None
             | Choice2Of2 e -> Some ^ fun () -> eL.Name + ":" + e ()
          | _ -> eO <- None; None
        member __.toAST (eO, d) =
          eO
          |> Option.iter ^ fun e ->
               d.Add (eL.Name, eJ.toAST e)}

    static member Pair (eJ: JsonR<'e,Pair<'e,'r>,'o,'t>,
                        rJ: JsonR<'r,        'r ,'o,'t>) =
      {new JsonR<Pair<'e,'r>,Pair<'e,'r>,'o,'t> () with
        member __.parse (d, er) =
          match eJ.parse (d, &er.Elem) with
           | None -> rJ.parse (d, &er.Rest)
           | some -> some
        member __.toAST (er, d) =
          eJ.toAST (&er.Elem, d)
          rJ.toAST (&er.Rest, d)}

    static member Product (asP: AsPairs<'p,'o,'t>, pJ: JsonR<'p,'p,'o,'t>) =
      {new Json<'t> () with
        member __.parse (ast) =
         match ast with
          | Object d ->
            let mutable p = Unchecked.defaultof<_>
            match pJ.parse (d, &p) with
             | None -> asP.Create (&p) |> Choice1Of2
             | Some e -> Choice2Of2 ^ fun () -> "{" + e () + "}"
          | _ -> Choice2Of2 ^ fun () -> "{ ... }"
        member __.toAST t =
         let mutable p = asP.ToPairs t
         let d = Dictionary ()
         pJ.toAST (&p, d)
         Object d}

    static member Case (_: Case<Empty,'o,'t>) : JsonS<Empty,'o,'t> =
      failwithf "Empty cases not supported, see type: %s" typeof<'t>.Name

    static member Case (m: Case<Pair<'e,'r>,'o,'t>,
                        pJ: JsonR<Pair<'e,'r>,Pair<'e,'r>,'o,'t>) =
      S [Json.Product (m, pJ)] : JsonS<Pair<'e,'r>,'o,'t>

    static member Case (m: Case<'e,'o,'t>,
                        eL: Labelled<'e,'e,'o,'t>,
                        eJ: Json<'e>) =
      S [(if eL.Name = "Item"
          then {new Json<'t> () with
                 member __.parse ast = eJ.parse ast |> Choice.map m.OfPairs id
                 member __.toAST e = m.ToPairs e |> eJ.toAST}
          else Json.Product (m, Json.Elem (eL, eJ)))] : JsonS<'e,'o,'t>

    static member Choice (S pJ: JsonS<       'p    , Choice<'p,'o>,'t>,
                          S oJ: JsonS<          'o ,           'o ,'t>) =
      S (pJ @ oJ)             : JsonS<Choice<'p,'o>, Choice<'p,'o>,'t>

    static member Sum (asC: AsChoices<'p,'t>, S pJ: JsonS<'p,'p,'t>) =
      let pJ = Array.ofList pJ
      {new Json<'t> () with
        member __.parse ast =
          let rec lp es i =
            if i < pJ.Length
            then match pJ.[i].parse ast with
                  | Choice2Of2 e -> lp (e::es) (i+1)
                  | success -> success
            else Choice2Of2 ^ fun () ->
                   es
                   |> Seq.map ^ fun e -> e ()
                   |> String.concat "|"
          lp [] 0
        member __.toAST t =
          pJ.[asC.Tag t].toAST t}

    static member AST =
      {new Json<AST> () with
        member __.parse ast = Choice1Of2 ast
        member __.toAST ast = ast}

  //////////////////////////////////////////////////////////////////////////////

  let toDoc (t: 't) = generateDFS<Json, Json<'t>>.toAST t |> fAST
  let toString t = toDoc t |> render None

  let tryParseString<'t> s =
    runParserOnString pAST () "string" s
    |> function Success (result, (), _) ->
                generateDFS<Json, Json<'t>>.parse result
                |> Choice.map id ^ fun e -> e ()
              | Failure (error, _, ()) ->
                Choice2Of2 error

  let ofString<'t> s =
    match tryParseString<'t> s with
     | Choice1Of2 t -> t
     | Choice2Of2 m -> failwith m
