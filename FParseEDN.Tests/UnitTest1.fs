module FParseEDN.Tests

open NUnit.Framework

open FParseEDN.Parser
open FParsec

let assertSuccess result expected = 
    match result with
    | Success (s, _, _) -> Assert.AreEqual(expected, s)
    | Failure (msg, _, _) -> Assert.Fail(msg)

[<Test>]
let Boolean () =
    let r = parseString "true"
    assertSuccess r (EBoolean true)

    let r = parseString "false"
    assertSuccess r (EBoolean false)

[<Test>]
let Int () =
    let r = parseString "123"
    assertSuccess r (EInteger (int64 123))

[<Test>]
let Float () =
    let r = parseString "123.123"
    assertSuccess r (EFloat 123.123)

[<Test>]
let Nil () =
    let r = parseString "nil"
    assertSuccess r ENil

[<Test>]
let Symbol () =
    let r = parseString "symbol"
    assertSuccess r (ESymbol ("symbol", (None)))

    let r = parseString "prefix/suffix"
    assertSuccess r (ESymbol ("prefix", (Some "suffix")))

[<Test>]
let Keyword () =
    let r = parseString ":keyword"
    assertSuccess r (EKeyword ("keyword", (None)))

    let r = parseString ":ns/keyword"
    assertSuccess r (EKeyword ("ns", (Some "keyword")))

[<Test>]
let Character () =
    let r = parseString "\\c"
    assertSuccess r (ECharacter 'c')

[<Test>]
let String () =
    let r = parseString "\"some string\""
    assertSuccess r (EString "some string")

    let r = parseString "\"some string\\nsome string\""
    assertSuccess r (EString "some string\nsome string")