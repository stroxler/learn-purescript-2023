module Data.GameEnvironment where


newtype GameEnvironment = GameEnvironment
  { playerName :: String
  , debugMode :: Boolean
  }

gameEnvironment :: String -> Boolean -> GameEnvironment
gameEnvironment playerName debugMode = GameEnvironment { playerName, debugMode }