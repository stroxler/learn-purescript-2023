module Split
  ( Errors
  , Log
  , Parser
  , eof
  , lower
  , runParser
  , split
  , upper
  )
  where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.State.Trans (StateT, put, runStateT, get)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.String (drop, take, toLower, toUpper)
import Data.Tuple (Tuple)

type Errors = Array String

type Log = Array String

-- Note how the Identity is the right-most type here... that's always going to
-- be the case in transformer stacks, whose type signatures are built outward
-- right-to-left because:
-- - the monadic type variable always has to be right-most type in a
--   multi-type-param monad typeclass.
-- - the inner monad is always the monadic type parameter
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

-- Output is an error, or a nested tuple: ((resulting_a_value, remaining_state)
-- log_lines)
--
-- Note that the result type nests in the opposite direction as the
-- transformers, i.e. a StateT (WriterT (ExceptT ...))) winds up returing an
-- Either (log_tuple (state_tuple ...))
--
-- The underlying reason is in the implementation: the run functions *execute*
-- in the same order as the transfomrer stack, which means the calls *nest* in
-- the opposite order, and the output types will match the nesting.
runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p = runExcept <<< runWriterT <<< runStateT p
--
-- Let's break this down, ignoring the weirdness of identity by
-- using Except a as a pun for ExceptT Indentity a:
--
-- Parser a ~= StateT String (WriterT Log (Except Errors a))
--
-- runStateT :: StateT s m a -> m (Tuple a s)
--   ... output is a:
--         WriterT Log (Except Errors (Tuple a String))
--
-- runWriterT :: WriterT w m a -> m (Tuple a w)
--   ... output is an:
--         Except Errors (Tuple (Tuple a String) Log)
--
-- runExcept :: Except e a == ExceptT e Identity a -> Either e a
--   ... output is an:
--         Either Errors (Tuple (Tuple a String) Log)
--
-- Note that the run functions here don't actually run anything until
-- we get down to the Identity level. What they really do is register
-- themselves as callbacks to the *next* run function; that action
-- is what pops the outermost transformer off the stack and then pushes
-- it's "output type modifier" into the "output type stack"
--
-- This is another way of looking at why the transfomer types and output types
-- are flipped: running a transformer consists of registering callbacks that pop
-- off one stack and push onto the other until we reach the "ground truth" level
-- and can actually run all of the callbacks, so the resulting output stack is
-- in the reverse order.
--
-- Another angle on this is that the transformer types and the actual
-- ground-truth types represented in their run functions actually stack in the
-- opposite direction. For example, consider:
--   MaybeT m a = { runMaybeT :: m (Maybe a) }
--   StateT s m a = { runStateT :: m (Tuple a S) }
-- The monad itself, as concrete data, is nesting the opposite direction of
-- the transformer generic type constructors. That's why, for example, a stack
-- grounded in IO - whose generic type signature *ends* in IO - is in fact
-- an IO once we invoke all of the *runXxxT* functions, which essentially
-- wind up registering their duals as callbacks.
--
-- If you look at the definition of the bind function for a monad transformer,
-- by the way, any use of monadic combinators or notation always refers to the
-- input type parameter `m` (which is an "inner" monad in the transformer stack,
-- but actually an "outer" monad at runtime).
--
-- For example MaybeT:
--    bind { runMaybeT } f = do
--      x <- runMaybeT
--      case x of
--        Just something -> runMaybeT $ f something
--        Nothing -> pure Nothing
--
-- the `do`, `<-`, and `pure` are all referring to `m`, which at runtime is
-- the "outer" monad, and we're registering the Maybe bind logic as a callback.
--
-- Now let's consider lift for a moment. Why is it that we have to lift
-- operations defined on the inner monad, and what's really going on here? Well,
-- there's once again two ways to look at it:
-- - At the type level, we lift in order to add nesting. That's clear
-- - But at runtime, remember that the data types are actually flipped: the inner
--   monad of a stack is actually the outermost one in the evaluation. When we
--   "lift" an operation, we are lifting a *bare* value from that outermost monad
--   so that it becomes a nested value with all the inner context. Remember, lift
--   is actually just fmap: we're just mapping over the inner functors - for example
--   in a `MaybeT IO a`, we lift an `IO a` action into the `MaybeT IO a` monad
--   (which will produce an IO (Maybe a) at runtime) by fmapping that action over
--   the Maybe context.
-- 
-- Also another note: the "ground truth" monad for a pure computation that is
-- using monads simply as combinators is Identity. For effectful computations,
-- on the other hand, the "ground truth" will be some IO-like monad (either
-- Effect or Aff).

split :: Parser String
split = do
  s <- get
  tell ["[split] Parser state is: " <> show s]
  case s of
    "" -> throwError ["Unexpected end-of-file"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)


eof :: Parser Unit
eof = do
  s <- get
  tell ["[eof] Parser state is: " <> show s]
  case s of
    "" -> pure unit
    _ -> throwError ["Expected end-of-file"]


-- Note: the ExceptT component encodes a MonadPlus instance
-- - MonadPlus = Plus + Monad
-- - there's also a weaker Alternative = Plus + Applicative
-- - here, Plus is Alt along with an `empty` value
--
-- This fact is important for understanding:
-- - how the guard works (it creates an `empty` value)
-- - how combanators like `alt = (<|>)`, `many`, and `some`

upper :: Parser String
upper = do
  s <- get
  guard $ toUpper s == s
  pure s


lower :: Parser String
lower = do
  s <- get
  guard $ toLower s == s
  pure s


