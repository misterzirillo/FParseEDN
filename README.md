# FParseEDN
EDN is defined [here](https://github.com/edn-format/edn).
It is implemented in F# using the excellent [fparsec](https://github.com/stephan-tolksdorf/fparsec) parser combinator library.

## TODO
- implement comments
- implement discard
- expose a function to parse input other than `string`
- tags are over-permissive with regard to the spec (`#` must be followed by a letter)
