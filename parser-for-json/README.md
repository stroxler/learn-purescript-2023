# A simple parser combinator in Purescript cannot parse json

## Problem: nonlaziness and stack safety

I have nonrecursive parsers working okay, but as soon as I get to json arrays
I'm in trouble - it turns out that because parsers are data, allowing mutually
recursive parsers requires a much more complex understanding of purescript
than I currently have.

In Haskell, you can define mutually recursive parsers easily using plain
textbook parser-combinator logic, but in Purescript mutually recursive data (as
opposed to functions) presents a problem - the laziness in Haskell allows us
to implicitly build up the data out of thunks, but in Purescript the cycle can't
compile (and if I wrap it in thunks naively it will compile but then blow the
stack the first time I try to instantiate something).

We need to implement Control.Lazy to make this work, but I don't yet understand
the semantics of Control.Lazy well enough to do this.

I'm probably going to give up on this for now - I can learn to use the
`purescript-parsing` library, which has solved this (and also other problems, like
big callstacks on huge parses, by using CPS + trampolining), and revisit the
implementation later.

## Solution: a combination of Control.Lazy and Data.Lazy, I think

The purescript parsing library's core is fairly small, I can probably read
it (and maybe copy it to my own implementation with comments on how everything
works) in, say, 5-15 hours of work.

The entire core monad is defined in Parsing.purs:
https://github.com/purescript-contrib/purescript-parsing/blob/main/src/Parsing.purs

The rest of the library (all the code in the src/Parsing/ directory) defines
the various combinators, which include
- general combinators (which are list based for sequence operations)
- array versions of the sequence operations in Parsing.Combinators.Array
- combinators specific to parsing string state (which is also sort of lexing
  related) in Parsing.String
- lexing-oriented combinators in the Parsing.Token module, plus full token-parser
  defining helpers like `GenLanguageDef`
- example language defs in `Parsing.Language`

My feeling is that I can probably start using this library first, and implement
some fun examples (e.g. maybe a tiny lisp), and come back to understanding the
implementation / making my own simpler minimal version later.
