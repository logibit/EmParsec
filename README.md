EmParsec
========

EmParsec is a single file, embeddable combinator parser for use in situations where it is more important to avoid binary dependencies than it is to have the best possible performance.

If you've used FParsec, it should seem very familiar to you!

You can find an introductory blog post at http://blog.mavnn.co.uk/emparsec-embedded-parser-library/

Example code:
------------

``` fsharp
let whitespace : UParser<unit> =
  many (satisfy System.Char.IsWhiteSpace "")
  |>> ignore
  <?> "<whitespace>"

let valueName : UParser<string> =
  many1 (satisfy (fun c -> c <> '}' && (not <| System.Char.IsWhiteSpace c)) "")
  |>> (fun charList -> charList |> List.map string |> String.concat "")

let openValue : UParser<unit> =
  pchar '{' .>>. whitespace
  |>> ignore

let closeValue : UParser<unit> =
  whitespace .>>. pchar '}'
  |>> ignore

let value : UParser<TemplatePart> =
  between openValue closeValue valueName
  |>> Value
  <?> "<value parser>"

```
