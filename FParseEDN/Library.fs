namespace FParseEDN

type Element =
    | ENil
    | EBoolean of bool
    | EString of string
    | ECharacter of char
    | ESymbol of string
    | EKeyword of string
    | EInteger of int64
    | EFloat of float
    | EList of Element list
    | EVec of Element list
    | EMap of Map<Element, Element>
    | ESet of Set<Element>
    | ETagged of string * Element

module Parser =

    open FParsec.Primitives
    open FParsec.CharParsers

    // scalars
    let private enil = stringReturn "nil" ENil <?> "nil"
    let private efalse = stringReturn "false" (EBoolean false) <?> "boolean"
    let private etrue = stringReturn "true" (EBoolean true) <?> "boolean"

    let private numberFormat = NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction

    let private enumber =
        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then EInteger (int64 nl.String)
                else EFloat (float nl.String)

    // helpers
    let private digit = satisfy isDigit <?> "digit"
    let private letter = satisfy isLetter <?> "letter"
    let private slash = pchar '/' <?> "forward slash"
    let private colon = pchar ':' <?> "colon"

    // keyword
    let private ekw =
        let followChar = colon <|> letter <|> digit
        let restChar = digit <|> letter <|> slash <|> colon
        let restChars = manyTill restChar spaces1
        colon >>. (followChar .>>. restChars) |>> string |>> EKeyword

    let private eelement = choice [
        enil
        efalse
        etrue
        enumber
        ekw
    ]

    let parseString s = runParserOnString (eelement .>> eof) () "string" s