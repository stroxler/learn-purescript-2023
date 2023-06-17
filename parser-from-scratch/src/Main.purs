module Main
  where

import Prelude

import Control.Monad.Except (ExceptT, runExcept, throwError)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.String as S
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
        pure $ S.take n state.content
    else throwError "Unexpected end of input"

consume1 :: Parser String
consume1 = consumeN 1

predicateN :: Int -> (String -> Boolean) -> Parser String
predicateN n pred = do
  head <- consumeN n
  if pred head
  then pure head
  else throwError $ "Head '" <> show head <> "' does not pass `pred`"

predicate1 :: (String -> Boolean) -> Parser String
predicate1 = predicateN 1


getRemainingContent :: Parser String
getRemainingContent = remainingContent <$> get
    where remainingContent state = S.drop state.location state.content


matches :: String -> Parser String
matches match = predicateN (S.length match) (_ == match)



runParser :: forall a. ParserState -> Parser a -> Either Error (Tuple a ParserState)
runParser initialState parser = runExcept $ runStateT parser initialState

-- *** A SMALL LANGUAGE ***

main :: Effect Unit
main =
    let result = runParser (createInitialState "hello, world") (matches "hello")
    in logShow result
