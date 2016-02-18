#load "EmParsec.fs"
open System
open EmParsec

let test name parser input expected =
  match run parser input with
  | Choice1Of2 r ->
    if r = expected then
      printfn "*** Test '%s' passed" name
    else
      sprintf "*** Test '%s' failed, expect %A but received %A" name expected r
      |> failwith
  | Choice2Of2 err ->
    failwithf "*** Test '%s' failed with parser error:\n%s" name err

let shouldFailTest name parser input =
  match run parser input with
  | Choice1Of2 r ->
    printfn "*** Test '%s' failed - parsing returned %A" name r
  | Choice2Of2 e ->
    printfn "*** Test '%s' passed with message:\n%s" name e

let thingP = pstring "thing"

test "pstring with additional" thingP "thing  " "thing"
test "pstring exact" thingP "thing" "thing"

test "eof exact match" (thingP .>> eof) "thing" "thing"
shouldFailTest "eof with additional stuff on end" (thingP .>> eof) "thing  h"

let quotedString =
  between (pchar '"') (pchar '"') (many (satisfy (fun c -> c <> '"') ""))
  |>> recompose

test "quotedString" quotedString "\"hello world\"" "hello world"
shouldFailTest "unclosed quotedString" quotedString "\"hello!"

let number =
  (many (satisfy Char.IsDigit ""))
  |>> recompose

test "number or quoted string (string)" (quotedString <|> number) "\"Boo\"" "Boo"
test "number or quoted string (number)" (quotedString <|> number) "10" "10"

let stringList =
  sepBy quotedString (pchar ';')

test "separated strings (empty)" stringList "" []
test "separated strings (single)" stringList "\"hello\"" ["hello"]
test "separated strings (list)" stringList "\"hello\";\"world\"" ["hello";"world"]

let choiceOfThings =
  choice [
    quotedString
    pchar ' ' |>> fun c -> ""
    number
  ]

test "choice (string)" choiceOfThings "\"yeah\"" "yeah"
test "choice (number)" choiceOfThings "22" "22"
test "choice (space)" choiceOfThings " " ""

let operatorWorkout =
  int <!> number .>> pchar ' '
  .>>. pstring "漢字"
  |>> fun (n, s) -> sprintf "%s %d" s n

test "operators" operatorWorkout "10 漢字" "漢字 10"

test "spaces" spaces "   " ()

test "spaces with no spaces" spaces "a" ()

test "spaces1" spaces1 "  \n\t\r\n" ()

shouldFailTest "spaces1 with no space" spaces1 "a"

// Mini template parser example

type TemplatePart =
  | Text of string
  | Value of string

let notOpenBracket : UParser<char> =
  satisfy (fun c -> c <> char '{') "not open bracket"

let textParser : UParser<TemplatePart> =
  many1 notOpenBracket
  |>> (fun charList ->
         charList
         |> List.map string
         |> String.concat ""
         |> Text)
  <?> "<text parser>"

let valueName : UParser<string> =
  many1 (satisfy (fun c -> c <> '}' && (not <| System.Char.IsWhiteSpace c)) "")
  |>> (fun charList -> charList |> List.map string |> String.concat "")
  .>> spaces

let openValue : UParser<unit> =
  pchar '{' .>>. spaces
  |>> ignore

let closeValue : UParser<unit> =
  pchar '}'
  |>> ignore

let value : UParser<TemplatePart> =

  between openValue closeValue valueName
  |>> Value
  <?> "<value parser>"

let template : UParser<TemplatePart list> =
  many (value <|> textParser)
  .>> eof
  <?> "<template parser>"

test "mini template 1" template "hello world" [Text "hello world"]
test "mini template 2" template "hello { bob\n }" [Text "hello "; Value "bob"]
test "mini template 3" template "hello { name1 } and {name2}" [Text "hello "; Value "name1"; Text " and "; Value "name2"]
shouldFailTest "mini template 4" template "hello { name"