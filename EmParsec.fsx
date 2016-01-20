#if INTERACTIVE
#else
module internal EmParsec
#endif

open System

type ParserLabel = string
type ErrorMessage = string

type Location =
  { Line : int; Column : int }
with
  override x.ToString() =
    sprintf "line %d, column %d" x.Line x.Column

type CharStream<'UserState> =
  { Remaining : char list
    Original  : string
    State     : 'UserState
    Location  : Location }

type Reply<'Result, 'UserState> =
  | Success of 'Result * CharStream<'UserState>
  | Error of ParserLabel * ErrorMessage * Location

let (|Next|) (stream : CharStream<_>) : char list * CharStream<_> =
  match stream.Remaining with
  | '\r'::'\n'::t ->
    ['\r';'\n'],
    { stream with
       Remaining = t
       Location = { stream.Location with
                     Line = stream.Location.Line + 1
                     Column = 0 } }
  | h::t when h = '\r' || h = '\n' ->
    [h],
    { stream with
       Remaining = t
       Location = { stream.Location with
                     Line = stream.Location.Line + 1
                     Column = 0 } }
  | h::t ->
    [h],
    { stream with
       Remaining = t
       Location = { stream.Location with
                     Column = stream.Location.Column + 1 } }
  | [] ->
    [], stream

type Parser<'Result, 'UserState> =
  { ParseFunc : (CharStream<'UserState> -> Reply<'Result, 'UserState>)
    Label : ParserLabel }

type UParser<'Result> = Parser<'Result, unit>

module Reply =
  let map f r =
    match r with
    | Success (r, s) ->
      Success (f r, s)
    | Error (lab, err, l) ->
      Error (lab, err, l)

  let bind f r =
    match r with
    | Success (r, s) ->
      f r s
    | Error (lab, err, l) -> Error (lab, err, l)

let setLabel parser newLabel =
  let newFunc stream =
    match parser.ParseFunc stream with
    | Success (r, s) -> Success (r, s)
    | Error (lab, msg, l) -> Error (newLabel, msg, l)
  { ParseFunc = newFunc; Label = newLabel }

let (<?>) = setLabel

let showError (original : string) (label, msg, location) =
  if original = "" then
    sprintf "Parser %s failed on empty string\n" msg
  else
    let failingLine =
      let rec lines (r : IO.StringReader) =
        seq {
          let line = r.ReadLine()
          if line <> null then
            yield line
            yield! lines r }
      use r = new IO.StringReader(original)
      lines r
      |> Seq.item location.Line
    let errorMarker =
      List.concat [[for i in 0..location.Column - 1 -> " "];["^"]]
      |> String.concat ""
    sprintf
      "Parser %s failed at line %d column %d\n%s\n%s\n%s"
      label
      location.Line
      location.Column
      msg
      failingLine
      errorMarker

let andThen p1 p2 =
  let inner stream =
    p1.ParseFunc stream
    |> Reply.bind
      (fun r s ->
        match p2.ParseFunc s with
        | Success (r', s') -> Success ((r, r'), s')
        | Error (lab, err, l) -> Error (lab, err, l))
  { ParseFunc = inner
    Label = "" }
  <?> sprintf "(%s andThen %s)" p1.Label p2.Label

module Parser =
  let map f p =
    { ParseFunc = fun s -> p.ParseFunc s |> Reply.map f
      Label = p.Label }

  let return' x =
    let inner stream =
      Success (x, stream)
    { ParseFunc = inner; Label = "unknown" }

  let apply f p =
    andThen f p
    |> map (fun (f, x) -> f x)

  let lift2 f x y =
    apply (apply (return' f) x) y

let run (parser : Parser<_, unit>) (str : string) =
  let cs = {
      Remaining = str |> List.ofSeq
      Original = str
      State = ()
      Location = { Line = 0; Column = 0 }
    }
  match parser.ParseFunc cs with
  | Success (r, _) -> Choice1Of2 r
  | Error (lab, m, l) -> Choice2Of2 <| showError str (lab, m, l)

let orElse p1 p2 =
  let inner stream =
    match p1.ParseFunc stream with
    | Success (r, s) -> Success (r, s)
    | Error (lab, e, _) ->
      match p2.ParseFunc stream with
      | Success (r, s) -> Success (r, s)
      | Error (lab, e', l) -> Error (lab, e', l)
  { ParseFunc = inner; Label = "" }
  <?> sprintf "(%s orElse %s)" p1.Label p2.Label

let (<!>) = Parser.map

let (|>>) x f = Parser.map f x

let (.>>.) p1 p2 = andThen p1 p2

let (.>>) p1 p2 =
  p1 .>>. p2 |>> fst

let (>>.) p1 p2 =
  p1 .>>. p2 |>> snd

let (<|>) p1 p2 = orElse p1 p2

let choice parsers =
  List.reduce orElse parsers

let sequence parsers =
  let cons = Parser.lift2 (fun h t -> h::t)

  List.foldBack cons parsers (Parser.return' [])

let many parser =
  let rec inner results stream =
    match parser.ParseFunc stream with
    | Success (r, s) ->
      inner (r::results) s
    | Error _ ->
      Success (results |> List.rev, stream)
  { ParseFunc = inner []; Label = sprintf "<many %s>" parser.Label }

let many1 parser =
  parser .>>. (many parser)
  |>> (fun (x, xs) -> x::xs)

let opt parser =
  (parser |>> Some) <|> (Parser.return' None)

let between start finish value =
  start >>. value .>> finish

let sepBy1 parser sep =
  let sepThenP = sep >>. parser
  parser .>>. many sepThenP
  |>> fun (r, rs) -> r::rs
  <?> sprintf "<sepBy1 %s separated by %s>" parser.Label sep.Label

let sepBy parser sep =
  sepBy1 parser sep <|> Parser.return' []
  <?> sprintf "<sepBy %s separated by %s>" parser.Label sep.Label

let recompose (chars : char list) =
  chars
  |> List.map string
  |> String.concat ""

let pend =
  let label = "<end>"
  let inner stream =
    match stream with
    | Next ([], _) -> Success ((), stream)
    | _ -> Error (label,
                  sprintf "Unexpected input remaining '%s'" <| recompose stream.Remaining,
                  stream.Location)
  { ParseFunc = inner; Label = label }

let satisfy predicate label =
  let inner stream =
    match stream with
    | Next ([], _) -> Error (label, "No more input", stream.Location)
    | Next ([h], s) when predicate h ->
      Success (h, s)
    | Next (a, _) ->
      Error (label, sprintf "Unexpected %A" a, stream.Location)
  { ParseFunc = inner; Label = label }

let pchar c : Parser<char, 'a> =
  satisfy ((=) c) (sprintf "<char %c>" c)

let anyOf chars =
  chars
  |> List.map pchar
  |> List.reduce orElse
  <?> (sprintf "<anyOf %A>" chars)

let pstring (str : string) : Parser<string, 'a> =
  str
  |> List.ofSeq
  |> List.map pchar
  |> List.reduce (.>>)
  |> Parser.map (fun _ -> str)
  <?> (sprintf "<string %s>" str)

