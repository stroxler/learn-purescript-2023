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

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

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
-- runStateT :: StateT s m a -> s -> m (Tuple a s)
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


