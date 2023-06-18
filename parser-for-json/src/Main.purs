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


data Json
  = JNull
  | JNumber Number
  | JBoolean Boolean
  | JString String
  | JArray (Array Json)
  | JAssoc (Array (Tuple String Json))


derive instance genericJNode ∷ Generic Json _

instance showJson :: Show Json where
  -- making the j argument explicit is required here
  -- see https://github.com/purescript/purescript/issues/2975
  show j = genericShow j



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


parseJString :: Parser Json
parseJString = parseQuote *> parseContent <* parseQuote
  where
    parseQuote = parseSymbol "\""
    parseContent = do
      characters <- many parseCharacter
      pure $ JString $ fold characters


main :: Effect Unit
main = do
    logShow $ runParser (createInitialState "\"this is a\\ string!\\\"\\\\\\n\" and this is the rest") parseJString
