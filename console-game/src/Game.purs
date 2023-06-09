module Game
  where

import Prelude

import Control.Monad.RWS (RWS, ask, get, modify_, put, tell)
import Data.Coords (Coords(..))
import Data.Coords as Coords
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment(..))
import Data.GameItem (GameItem(..), readItem)
import Data.GameState (GameState(..))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S

type Log = L.List String

type Game = RWS GameEnvironment Log GameState


-- Helper Game actions not directly exposed to the user

message :: String -> Game Unit
message = tell <<< L.singleton

describePosition :: Game Unit
describePosition = do
  GameState state <- get
  case state.playerPosition of
    Coords { x: 0, y: 0} -> message "You are in a dark forest. You can see a path to the north."
    Coords { x: 0, y: 1} -> message "You are in a clearing."
    _ -> message "You are lost deep in a forest."


pickUpItem :: GameItem -> Game Unit
pickUpItem item = do
  GameState state <- get
  case state.playerPosition `M.lookup` state.items of 
    Just items | item `S.member` items -> do
      put $ GameState state
        { items = M.update (Just <<< S.delete item) state.playerPosition state.items
        , inventory = item `S.insert` state.inventory }
      message $ "You now have " <> show item
    _ -> message "I don't see that item here."


move :: Int -> Int -> Game Unit
move dx dy = do
  GameState state <- get
  let
    newPosition = Coords.withDelta state.playerPosition dx dy
  put $ GameState state { playerPosition = newPosition }

-- Note: the book implements move in terms of modify_. I find the do version
-- cleaner but it's nice to include both for practice.
moveWithoutDo :: Int -> Int -> Game Unit
moveWithoutDo dx dy = modify_ $ updatePlayerPosition
  where
    updatePlayerPosition (GameState oldState) =
      GameState oldState { playerPosition = Coords.withDelta oldState.playerPosition dx dy }

hasItem :: GameItem -> Game Boolean
hasItem item = do
  GameState { inventory } <- get
  pure $ item `S.member` inventory


useItem :: GameItem -> Game Unit
useItem Candle = message "I don't know what to do with just a candle."
useItem Matches = do
  hasCandle <- hasItem Candle
  if hasCandle
  then do
    GameEnvironment env <- ask
    tell $ L.fromFoldable
      [ "You have lit the candle with the matches."
      , "Congratulations, " <> show env
      , "You win!"
      ]
  else message "I don't know what to do with just matches."

-- The actual game actions
north :: Game Unit
north = move 0 1

south :: Game Unit
south = move 0 (-1)

east :: Game Unit
east = move 1 0

west :: Game Unit
west = move (-1) 0

look :: Game Unit
look = do
  GameState state <- get
  message $ "You are at " <> Coords.prettyPrintCoords state.playerPosition
  describePosition
  -- (the for_ here is over the option foldable)
  for_ (state.playerPosition `M.lookup` state.items) describeItems
  pure unit
    where
    describeItems items =
      tell (map (\item -> "You can see the " <> show item <> ".") itemsList)
        where itemsList = (S.toUnfoldable items :: L.List GameItem)

inventory :: Game Unit
inventory = do
  GameState state <- get
  let inventoryList = (S.toUnfoldable state.inventory :: L.List GameItem)
  tell (map (\item -> "You have the " <> show item <> ".") inventoryList)

take :: String -> Game Unit
take itemString = case readItem itemString of
  Nothing -> message "I don't know what item you are referring to"
  Just item -> pickUpItem item


use :: String -> Game Unit
use itemString = case readItem itemString of
  Nothing -> message "I don't know what item you are referring to"
  Just item -> do
    has <- hasItem item
    if has
    then useItem item
    else message "You don't have that item."

debug :: Game Unit
debug = do
  GameEnvironment env <- ask
  if env.debugMode
  then do
    state <- get
    message (show state)
  else message "Not running in debug mode"


-- Converting string commands into game actions
gameCommand :: Array String -> Game Unit
gameCommand ["north"] = north
gameCommand ["south"] = south
gameCommand ["east"] = east
gameCommand ["west"] = west
gameCommand ["look"] = look
gameCommand ["inventory"] = inventory
gameCommand ["take", item] = take item
gameCommand ["use", item] = use item
gameCommand ["debug"] = debug
gameCommand _ = message "I don't understand that command"