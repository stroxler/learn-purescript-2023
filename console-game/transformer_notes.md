# High-level notes on monad transformers

## The type signature and value are in reverse order

Consider a classic transformers, `MaybeT`:
```purs
data MaybeT m a = { runMaybeT :: m (Maybe a) }
```
Note that although the type signature nests `MaybeT`
*around* the `m` monad, the actual value has `m` on the
outside.

This is true of all the transformers, in some sense (in
particular, when considering the output values), for example
the `StateT` requires an `s` but returns the `Tuple a s` nested
inside an `m` context:
```purs
data StateT s m a = { runStateT :: s -> m (Tuple a s) }
```

So when we are building up a monad transformer stack, we are
typically building up actual representations in the reverse
order.

## What does this mean, in terms of the runtime?

What this means in terms of the runtime is that the "outermost"
transformers in a monad are the "innermost" in the runtime values
we actually encounter.

For example, if we have a
```purs
StateT MyState (ExceptT Error (Effect)) a
```
then we actually run it by invoking the `runXxxT` operators in
reverse order:
```purs
run :: StateT MyState (ExceptT Error (Effect)) a
       -> MyState
       -> Effect (Either Error (Tuple MyState a))
run computation initialState =
  runExceptT $ runStateT computation initialState
```
So given a computation in our transformer stack context, we first
unwrap the `StateT` which pushes the `Tuple a s` context into
the return type and produces a computation of type
```purs
ExceptT Error Effect (Tuple a s)
```
and then we unwrap the `ExceptT` which produces a computation of type
```purs
Effect (Either Error (Tuple a s))
```

We can't directly unwrap the `Effect` in purescript (or `IO` in Haskell),
but this is enough: once we have an `Effect` computation we can invoke
it as part of `Main`.

Note that this means the runtime contexts and the return values are always
nested in the opposite order of the monad transformers we are stacking.

When we actually build up these monads out of primitive operations
(in the general case `pure`, `bind`, and `lift`, but for specific monads there
will be other operators like `get` / `put` / `tell` / etc) what actually
winds up happening is that the data consists of callbacks that are generic
over "lower in the stack" monads, with each one lifted to the suitable layer.

When we invoke `runXxxT`, we don't actually *run* these callbacks (we can't: consider
the case where the bottom monad is `Effect`: we can't run this, only `Main` can). Instead,
what the `runXxxT` functions wind up doing is re-wiring the callbacks in such a way
that the values nest in the reverse direction.

This is in fact exactly why transformers are even a thing: remember that we
can't nest monads because of the `F (G (F (G a)))` problem, where `join` can't
flatten in general. Transformers consist of exactly those monads where we know
how to generically flip the ordering by registering callbacks specific to the
outer type / inner value `F` in terms of the abstract monad operations of the outer
context / inner monad `G`.

## What does this mean, in terms of actual usage?

Okay so we've talked about how the transfomrer stack, as a generic type,
nests in the opposite direction of the return types.

This also implies that the type stack nests in the opposite direction of the
runtime *contexts*. This is because the return types more or less correspond to
the context, e.g. an `Effect a` is an "a in the Effect context` and a `(Either e
(Maybe a))` is an "A in a Maybe context in an Either context".

But what does that mean in terms of application structure?

It means that we can compose applications written in a "shorter" monad stack
in terms of components that involve "taller" stacks built on top of those
"shorter" stacks.

For example, let's say we have two monad transformer stacks:
```purs
data App :: ExceptT Error (StateT AppState Effect)

runApp :: App a -> AppState -> Effect (Tuple (Either Error a) AppState) 
```
and some operations in the relevant monadic contexts:
```purs
fetchInitialState :: Effect AppState
getUser :: String -> App User
sendMessage :: User -> String -> App MessageId
log :: String -> Effect Unit
```
we could write a simple `main` like this:
```purs
main :: Effect Unit
main = do
  -- we're in the outer Effect context
  initialState <- fetchInitialState
  messageId, newState <- runApp $ do
    -- we're now in the App context
    lift $ log "Fetching user..."
    user <- getUser "stroxler"
    lift $ log "Got user, sending message..."
    sendMessage user "Purescript is cool, and so are monad tranformers!"
  log $ "we just sent a message with id " <> show messageId
```

## Nesting do blocks that add more capabilities

Note that we only need transformers when we want to make use of the
outer monadic effects inside of an inner computation with some additional
effect. For example, if we wanted to nest another `do` block using the
`Maybe` monad inside of our app, then:
- if we have some computation returning `Maybe a` for which we want to use
  monadic syntax then we could just use a normal `do` block via the `Maybe`
  monad (either in a let binding or a `return`) no need for a transfomer.
- but if the function requires accessing `Effect` monad then we'll need
  to convert it into a `MaybeT`, at which point we can leverate `Effect a`
  actions by lifting them.
  - We can ignore the intermediate monads (state and either) if we never
    actually use an effect from them; this is one of the points of
    transformers is you can ignore the parts you don't use
  - if you want a precise type for the operation (e.g. so you can test
    it), then you'll want to describe the type not as the precise stack
    but rather in terms of "abstract monad transformer" type classes,
    which are invariant to the exact ordering of capabilities.

The type classes in question are, e.g., `MonadState`, `MonadError`,
`MonadWriter`, etc. Since the order of type class constraints doesn't
matter and the presence of additional type classes is irrelevant, then
we can make computations using some set of effects depend only
on those effects by using the effects as type class constraints over
the concrete monad, instead of pinning our type signatures to an exact
monad transformer stack type.

## In practice, how do stacks generally work?

- Most likely, the bottom of the stack will be either Identity (for a
  pure monadic computation) or some kind of `IO`, either `Effect` or `Aff`.
  - this means the outermost context is either regular values
    (Identity is just a boxed value) or an `IO`.
- Often the top of the type stack will be `ExceptT` (and hence the innermost
  context of a computation will be an `Either`). We tend to want this because
  `ExceptT` throws away any data in the right-side type when it hits an
  error, and if we are using `State` or `Writer` or whatnot we usually don't
  want to be throwing away all our effects when we hit an error.
- The order of reader / writer / state don't matter as much; it is also
  common to use RWS which flattens all three effects into a single
  type.

I'm not sure where something like the continuation monad would fit into
this; that's a future thing for me to learn!