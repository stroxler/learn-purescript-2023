module Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExcept, throwError)
import Control.Monad.Rec.Class (untilJust)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List(..))
import Data.String as S
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Debug (trace)
import Effect (Effect)
import Effect.Class.Console (logShow)


-- *** PARSER CODE ***

type Error = String

type ParserState =
  { content :: String
  , location :: Int
  }

createInitialState :: String -> ParserState
createInitialState content = { content, location: 0 }

-- Remember: the types stack in the opposite direction as the
-- actual effects. We want the either to be on the outside
-- so that state resets on errors, which means we put ExceptT on
-- the inside.
type Parser = (StateT ParserState (ExceptT Error Identity)) 


fail :: forall a . String -> Parser a
fail message = do
  state <- get
  throwError $ message <> " (at position " <> show state.location <> ")"


consumeN :: Int -> Parser String
consumeN n = do
    state <- get
    if S.length state.content >= state.location + n
    then do
        put $ state { location = state.location + n }
        pure $ S.take n (S.drop state.location state.content)
    else throwError "Unexpected end of input"

consume1 :: Parser String
consume1 = consumeN 1

predicateN :: Int -> (String -> Boolean) -> Parser String
predicateN n pred = do
  head <- consumeN n
  let _ = trace ("n is " <> show n <> ", head is " <> head) \_ -> unit
  if pred head
  then pure head
  else throwError $ "Head '" <> show head <> "' does not pass `pred`"


getRemainingContent :: Parser String
getRemainingContent = remainingContent <$> get
    where remainingContent state = S.drop state.location state.content


matches :: String -> Parser String
matches match = predicateN (S.length match) (_ == match)


many :: forall a. Parser a -> Parser (List a)
many p = many1 p <|> pure Nil


many1 ::  forall a. Parser a -> Parser (List a)
many1 p = do
  first <- p
  rest <- many p
  pure (Cons first rest)


runParser :: forall a. ParserState -> Parser a -> Either Error (Tuple a ParserState)
runParser initialState parser = runExcept $ runStateT parser initialState
