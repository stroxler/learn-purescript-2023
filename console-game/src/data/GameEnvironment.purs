module Data.GameEnvironment where

import Prelude


newtype GameEnvironment = GameEnvironment
  { playerName :: String
  , debugMode :: Boolean
  }

gameEnvironment :: String -> Boolean -> GameEnvironment
gameEnvironment playerName debugMode = GameEnvironment { playerName, debugMode }

derive newtype instance Show GameEnvironment