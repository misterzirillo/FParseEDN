module FParseEDN.Tests

open NUnit.Framework

open FParseEDN.Parser
open FParsec

let assertResult result expected = 
    match result with
    | Success (s, _, _) -> Assert.AreEqual(expected, s)
    | Failure (msg, _, _) -> Assert.Fail(msg)

[<Test>]
let Boolean () =
    let r = parseString "true"
    assertResult r (EBoolean true)

    let r = parseString "false"
    assertResult r (EBoolean false)

[<Test>]
let Int () =
    let r = parseString "123"
    assertResult r (EInteger (int64 123))

[<Test>]
let Float () =
    let r = parseString "123.123"
    assertResult r (EFloat 123.123)

[<Test>]
let Nil () =
    let r = parseString "nil"
    assertResult r ENil