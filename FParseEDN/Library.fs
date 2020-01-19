﻿namespace FParseEDN

type Element =
    | ENil
    | EBoolean of bool
    | EString of string
    | ECharacter of char
    | ESymbol of string * string option
    | EKeyword of string * string option
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
    let private enil = stringReturn "nil" ENil
    let private efalse = stringReturn "false" (EBoolean false)
    let private etrue = stringReturn "true" (EBoolean true)

    let private numberFormat = NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction

    let private enumber =
        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then EInteger (int64 nl.String)
                else EFloat (float nl.String)

    // helpers
    let private digit = satisfy isDigit <?> "digit"
    let private letter = satisfy isLetter <?> "letter"
    let private otherChars = anyOf "*!_?$%&=<>"
    let private slash = pchar '/' <?> "/"
    let private colon = pchar ':' <?> ":"
    let private pound = pchar '#' <?> "#"

    // symbol
    let private esymFiller =
        digit <|> letter <|> otherChars <|> pound <|> colon

    let private esymPrefix =
        let leadingSpecial = pchar '+' <|> pchar '-' <|> pchar '.'
        let followingSpecial = letter <|> otherChars
        let leadingNormal = otherChars <|> letter
        let restChars = many esymFiller

        let specialCase =
            pipe3 leadingSpecial followingSpecial restChars (fun l f r -> [l; f;] @ r)

        let normalCase =
            pipe2 leadingNormal restChars (fun l r -> l::r)

        specialCase <|> normalCase
        |>> (List.map string >> List.reduce (+))

    let private esymSuffix = slash >>. esymPrefix

    let private esymWhole = esymPrefix .>>. (opt esymSuffix)
    
    let private esymbol = esymWhole |>> ESymbol

    // keyword - symbol with a : on the front
    let private ekw =
        let leadChar = pchar ':'
        leadChar >>. esymWhole |>> EKeyword

    // chars an strings
    let private escape =
        satisfy (fun _ -> true)
        |>> function
            | 'b' -> '\b'
            | 'f' -> '\u000C'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c   -> c // every other char is mapped to itself

    let private unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char
        )

    let private escapedCharSnippet = pchar '\\' >>. (escape <|> unicodeEscape)

    let private echar = escapedCharSnippet |>> ECharacter

    let private normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

    let private estr =
        let dblQuote = pstring "\""
        let escString = escapedCharSnippet |>> string
        between dblQuote dblQuote
            (stringsSepBy normalCharSnippet escString)
        |>> EString

    let private eelement = choice [
        enil <?> "nil"
        efalse <?> "boolean"
        etrue <?> "boolean"
        enumber <?> "number"
        esymbol <?> "symbol"
        ekw <?> "keyword"
        echar <?> "character"
        estr <?> "string"
    ]

    let parseString s = runParserOnString (eelement .>> eof) () "input string" s