module Main
  where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (StateT, get, put)
import Data.Enum.Generic (genericToEnum)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String as S
import Effect (Effect)
import Effect.Console (log)


type Error = String

type ParserState =
  { content :: String
  , location :: Int
  }

type Parser = ExceptT Error (StateT ParserState Identity) 


fail :: forall a . String -> Parser a
fail message = do
  state <- get
  throwError $ message <> " (at position " <> show state.location <> ")"


consumeN :: Int -> Parser String
consumeN n = do
    state <- get
    if S.length state.content <= state.location + n
    then
        put $ state { location = state.location + n }
        pure $ S.take n state.content
    else throwError "Unexpected end of input"

consume :: Parser String
consume = consumeN 1


getRemainingContent :: Parser String
getRemainingContent = remainingContent <$> get
    where remainingContent state = S.drop state.location state.content


string :: String -> Parser String
string match = do
    maybeMatches <- consumeN (S.length match)
    if (match == maybeMatches)
    then pure match
    else throwError $ "No match for " <> show match




main :: Effect Unit
main = log "hello"
