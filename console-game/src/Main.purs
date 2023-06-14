module Main where

import Prelude

import Control.Monad.RWS (RWSResult(..), runRWS)
import Data.Array (fold)
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Data.GameState (GameState, initialGameState)
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Console (log)
import Game (gameCommand)
import Node.ReadLine as RL
import Options.Applicative ((<**>))
import Options.Applicative as OP


getGameEnvironment :: Effect GameEnvironment
getGameEnvironment = OP.customExecParser prefs argParser
  where
    prefs :: OP.ParserPrefs
    prefs = OP.prefs $ OP.showHelpOnEmpty <> OP.showHelpOnError
    argParser :: OP.ParserInfo GameEnvironment
    argParser = OP.info (env <**> OP.helper) parserOptions
    parserOptions :: OP.InfoMod _
    parserOptions = fold
      [ OP.fullDesc
      , OP.progDesc "Play the game as <player name>"
      , OP.header "Monadic console-based adventures! A game to learn monad tranformers."
      ]
    env :: OP.Parser GameEnvironment
    env = gameEnvironment <$> player <*> debug
    player :: OP.Parser String
    player = OP.strOption $ fold
      [ OP.long "player"
      , OP.short 'p'
      , OP.metavar "<player name>"
      , OP.help "The player's name"
      ]
    debug :: OP.Parser Boolean
    debug = OP.switch $ fold
      [ OP.long "debug"
      , OP.short 'd'
      , OP.help "Use debug mode"
      ]

gameLineHandler :: RL.Interface -> GameEnvironment -> GameState -> String -> Effect Unit
gameLineHandler interface environment initialState =
  lineHandler initialState
  where
    lineHandler currentState input =
      do
        case runRWS (gameCommand $ split (wrap " ") input) environment currentState of
          RWSResult newState _ written -> do
            for_ written log
            RL.setLineHandler (lineHandler newState) $ interface
        RL.prompt interface
        pure unit
  

main :: Effect Unit
main = do
  environment <- getGameEnvironment
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "adventure-game> " interface
  let
    lineHandler = gameLineHandler interface environment initialGameState
  RL.setLineHandler lineHandler interface
  RL.prompt interface

