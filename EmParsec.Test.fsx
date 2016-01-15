#load "EmParsec.fsx"
open EmParsec

let test name parser input expected =
  match run parser input with
  | Choice1Of2 r ->
    if r = expected then
      printfn "Test '%s' passed" name
    else
      failwithf "Test '%s' failed, expect %A but received %A" name expected r
  | Choice2Of2 err ->
    failwithf "Test '%s' failed with parser error:\n%s" name err

let shouldFailTest name parser input =
  match run parser input with
  | Choice1Of2 r ->
    printfn "Test '%s' failed - parsing returned %A" name r
  | Choice2Of2 _ ->
    printfn "Test '%s' passed" name

let thingP = pstring "thing"

test "pstring with additional" thingP "thing  " "thing"
test "pstring exact" thingP "thing" "thing"

test "pend exact match" (thingP .>> pend) "thing" "thing"
test "pend exact match" (thingP .>> pend) "thing  h" "thing"
