module Main
  ( Json(..)
  , parseCharacter
  , parseRawNull
  , parseString
  , parseSymbol
  )
  where

import Parser
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExcept, throwError)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Show.Generic (genericShow)
import Data.String as S
import Data.Tuple (Tuple)
import Debug (trace)
import Effect (Effect)
import Effect.Class.Console (log, logShow)


data JNode a
  = JNull
  | JNumber Number
  | JBoolean Boolean
  | JString String
  | JArray (Array a)
  | JAssoc (Array (Tuple String a))



derive instance genericJNode ∷ Generic (JNode a) _

instance showJNode :: Show a => Show (JNode a) where
  show = genericShow


newtype Json = JNode Json


parseSymbol :: String -> Parser Unit
parseSymbol symbol = matches symbol >>= \_ -> pure unit


parseRawNull :: Parser Unit
parseRawNull = parseSymbol "null"


parseCharacter :: Parser String
parseCharacter = do
  char <- consume1
  case char of
    "\"" -> throwError "End of string"
    "\\" -> do
      nextChar <- consume1
      pure nextChar
    _ -> pure char


parseString :: Parser String
parseString = parseQuote *> parseStringMiddle <* parseQuote
  where
    parseQuote = parseSymbol "\""
    parseStringMiddle = do
      characters <- many parseCharacter
      pure $ fold characters


main :: Effect Unit
main = do
    log "hello, world"
