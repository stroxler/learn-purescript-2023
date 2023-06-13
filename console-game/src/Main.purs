module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine as RL

main :: Effect Unit
main = do
  log "Here is a readline prompt (it doesn't do anything yet):"
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "adventure-game> " interface
  RL.prompt interface

