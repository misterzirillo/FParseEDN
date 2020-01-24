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
    assertSuccess r (ESymbol {name="symbol"; prefix=None})

    let r = parseString "prefix/suffix"
    assertSuccess r (ESymbol {name="suffix"; prefix=(Some "prefix")})

[<Test>]
let Keyword () =
    let r = parseString ":keyword"
    assertSuccess r (EKeyword {name="keyword"; prefix=None})

    let r = parseString ":ns/keyword"
    assertSuccess r (EKeyword {name="keyword"; prefix=(Some "ns")})

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

[<Test>]
let List () =
    let r = parseString "(:keyword \"string\" 1)"
    assertSuccess r (EList [
        (EKeyword {name="keyword"; prefix=None})
        (EString "string")
        (EInteger (int64 1))
    ])

    let r = parseString "(:keyword,\"string\",,,1)"
    assertSuccess r (EList [
        (EKeyword {name="keyword"; prefix=None})
        (EString "string")
        (EInteger (int64 1))
    ])

    let r = parseString "(:keyword (\"string\"))"
    assertSuccess r (EList [
        (EKeyword {name="keyword"; prefix=None})
        (EList [
            (EString "string")
        ])
    ])

[<Test>]
let Set () =
    let r = parseString "#{,, 1 1 ()()}"
    assertSuccess r (ESet (set [
        (EInteger (int64 1))
        (EList [])
    ]))

[<Test>]
let Vector () =
    let r = parseString "[[][\\t]]"
    assertSuccess r (EVec [
        (EVec [])
        (EVec [
            (ECharacter '\t')
        ])
    ])

[<Test>]
let Map () =
    let r = parseString "{ \"k1\" :v1,[:k2] v2}"
    assertSuccess r (EMap (Map.ofList [
        (
            (EString "k1"),
            (EKeyword {name="v1"; prefix=None})
        )
        (
            (EVec [(EKeyword {name="k2"; prefix=None})]),
            (ESymbol {name="v2"; prefix=None})
        )
    ]))

[<Test>]
let Tag () =
    let r = parseString "#my/tag \"my string\""
    assertSuccess r (ETagged
        (
            {name="tag"; prefix=(Some "my")},
            (EString "my string")
        ))