module Main
  ( Json(..)
  , main
  , parseCharacter
  , parseJBoolean
  , parseJNull
  , parseJNumber
  , parseJString
  , parseJson
  , parseNatural
  , parseRawNull
  , parseSymbol
  )
  where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parser (Parser, consume1, createInitialState, many, many1, matches, runParser)


data Json
  = JNull
  | JNumber Number
  | JBoolean Boolean
  | JString String
  | JArray (List Json)
  | JAssoc (List (Tuple String Json))


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


-- Note the delayed evaluation here: mutually recursive functions are allowed,
-- but mutually recursive *data* definitions are not because the javascript runtime
-- cannot evaluate a forward reference at the top-level (this is unlike in Haskell,
-- where laziness + compiled code makes this possible).
parseJson :: Unit -> Parser Json
parseJson _ = parseJNull <|> parseJBoolean <|> parseJNumber <|> parseJString <|> (parseJArray unit)


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


parseJArray :: Unit -> Parser Json
parseJArray _ = JArray <$> (matches "[" *> parseList <* matches "]")
  where
  parseList :: Parser (List Json)
  parseList = parseListAtLeast1 <|> pure Nil
  parseListAtLeast1 :: Parser (List Json)
  parseListAtLeast1 = do
    first <- parseJson unit
    rest <- parseRestOfList <|> pure Nil
    pure $ Cons first rest
  parseRestOfList :: Parser (List Json)
  parseRestOfList = do
    _ <- matches ","
    _ <- matches " " <|> pure ""
    parseListAtLeast1




main :: Effect Unit
main = do
    let _ = parseJson unit
    logShow "hello"
    --logShow $ runParser (createInitialState "\"this is a\\ string!\\\"\\\\\\n\" and this is the rest") (parseJson unit)
    --logShow $ runParser (createInitialState "-178 and some more") (parseJson unit)
    --logShow $ runParser (createInitialState "087 and some more") (parseJson unit)
    --logShow $ runParser (createInitialState "null and some more") (parseJson unit)
    --logShow $ runParser (createInitialState "true and some more") (parseJson unit)
    --logShow $ runParser (createInitialState "false and some more") (parseJson unit)
    --logShow $ runParser (createInitialState "[false, null, \"hi\", -86] and some more") (parseJson unit)
