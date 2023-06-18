module Main
  where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parser (Parser, consume1, createInitialState, many1, many, matches, runParser)


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


parseNatural :: Parser Number
parseNatural = do
  digits <- many1 parseDigit
  intFromDigits digits
    where
      parseDigit :: Parser String
      parseDigit = do
        char <- consume1
        if "0" <= char && char <= "9"
        then pure char
        else throwError "not a digit"
      intFromDigits :: List String -> Parser Number
      intFromDigits digits =
        case Number.fromString $ fold digits of
          Just value -> pure value
          Nothing -> throwError "failed to convert"
  

parseJNumber :: Parser Json
parseJNumber = JNumber <$> parseInteger
  where
    parseInteger = parseWithNegation parseNatural
    parseWithNegation parseNumeric = parseNumeric <|> parseNegated parseNumeric
    parseNegated parseNumeric = (\n -> -n) <$> (matches "-" *> parseNumeric)


parseJNull :: Parser Json
parseJNull = matches "null" *> pure JNull


parseJBoolean :: Parser Json
parseJBoolean = (\raw -> JBoolean $ raw == "true") <$> (matches "true" <|> matches "false")


main :: Effect Unit
main = do
    logShow $ runParser (createInitialState "\"this is a\\ string!\\\"\\\\\\n\" and this is the rest") parseJString
    logShow $ runParser (createInitialState "-178 and some more") parseJNumber
    logShow $ runParser (createInitialState "087 and some more") parseJNumber
    logShow $ runParser (createInitialState "null and some more") parseJNull
    logShow $ runParser (createInitialState "true and some more") parseJBoolean
    logShow $ runParser (createInitialState "false and some more") parseJBoolean
