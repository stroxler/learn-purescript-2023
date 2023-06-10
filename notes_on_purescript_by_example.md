## What to study next?

### For now, reference back to the Effect / Aff / FFI sections

There's too much information here to consume it all at once but the
content isn't interesting enough to me to focus on right now.

The Effect section does cover react-basic-hooks which could be cool
eventually; the problem is that right now my focus is on languages
and maybe scripting tools so I don't have near-term use for react.

### First thing to really study: Monadic Adventures

I should go through the game example code and associated exercises;
this will familiarize me with
- monad transformers
- all of the most common transformers (State, Reader, Writer)
- the Alternative typeclasses (which are handy in parsing)

I can probably easily jump from there to both parser-combinators
and small-scale interpreter implementations :)

The Mal interpreter would be a good next step.

### Eventually, I should also do the DSL chapter

It would be good to learn about free monads sooner rather than later :)

### Secondary resources

Look at Jordan's reference, which has a bunch of little console apps
illustrating these ideas, for example:
https://github.com/JordanMartinez/purescript-jordans-reference/blob/latestRelease/21-Hello-World/02-Effect-and-Aff/src/01-Effect/06-Mutable-State/02-Local.purs

I think the Application Structure sub-section in particular will cover
both transformers and free monads.

## Notes about early sections (up to but not including Effect)

### Deriving typeclasses

The book actually just links to purescript's main docs:
https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md

### Functional dependencies in typeclasses

This could be very easy to forget about:
https://book.purescript.org/chapter6.html#functional-dependencies

### How to use Partial functions

The unsafePartial function is key:
https://book.purescript.org/chapter6.html#nullary-type-classes

### `Functor <= Apply <= Applicative <= Monad`

What are examples of functors that are not Apply?
- Arbitrary-shaped generic containers have a derivable functor instance, but
  unless they are either same-shape (e.g. tuples with a single type and fixed
  size) or flattenable (e.g. lists) they don't have an Apply.

What are examples of Apply that are not Applicative?
- I don't have an example yet; it's easy to imagine eliminating the `pure`
  operation from same-shape generic containers.

What are examples of Applicative that are not Monad?
- Same-shape generic containers (e.g. tuples of a single type and fixed size)
- Applicative validation that collects multiple errors (monadic validation will
  stop at the first error)
- Parallel / concurrent futures (mondic futures will run in sequence)

In the context of validation, an interesting example of monadic validation
that isn't applicative is any nested extraction - for example if we have an
applicative validation library that handles possibly-missing key lookups
in maps, we can do applicative validation over many top-level key extractions
using the `V` Applicative instance and get multiple error messages.  But if we
allow nested maps and they can be possibly missing at any level, we'll need
to use the monadic form for nested lookups, and (as you'd expect) we can't get
nested errors if we hit a top-level missing value. Note that we can potentially
nest monadic lookups inside applicative lookups and vice-versa to get multiple
errors at any *particular* level.

