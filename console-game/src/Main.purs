module Main where

import Prelude

import Data.Array (fold)
import Data.GameEnvironment (GameEnvironment(..), gameEnvironment)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
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

main :: Effect Unit
main = do
  environment <- getGameEnvironment
  log "Here is the game environment..."
  logShow environment
  log "And here is a dummy readline interface"
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "adventure-game> " interface
  RL.prompt interface

