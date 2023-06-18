module Main
  where

import Parser (Parser, consume1, createInitialState, many, matches, runParser)
import Prelude

import Control.Monad.Except (throwError)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console (logShow)


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
      case nextChar of
        "n" -> pure "\n"
        _ -> pure nextChar
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
    logShow $ runParser (createInitialState "\"this is a\\ string!\\\"\\\\\\n\" and this is the rest") parseString
