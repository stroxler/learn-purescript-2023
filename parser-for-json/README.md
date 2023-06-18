# A simple parser combinator in purescript

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
