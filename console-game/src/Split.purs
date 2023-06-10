module Split where

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