#load "EmParsec.fs"
open System
open EmParsec

let test name parser input expected =
  match run parser input with
  | Choice1Of2 r ->
    if r = expected then
      printfn "*** Test '%s' passed" name
    else
      failwithf "*** Test '%s' failed, expect %A but received %A" name expected r
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