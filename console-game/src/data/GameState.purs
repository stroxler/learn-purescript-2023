module Data.GameState where

import Prelude

import Data.Coords (Coords, coords)
import Data.GameItem (GameItem(..))
import Data.Map as M
import Data.Set as S
import Data.Tuple (Tuple(..))


newtype GameState = GameState
  { playerPosition :: Coords
  , items :: M.Map Coords (S.Set GameItem)
  , inventory :: S.Set GameItem
  }

-- This works because Map / Set / etc all have Show already.
--
-- As a rule, for "named records" the derive newtype approach will work; we
-- mostly need the generic approach (see GameItem) for sum types.
derive newtype instance Show GameState

initialGameState :: GameState
initialGameState = GameState
  { playerPosition: coords 0 0
  , items: M.fromFoldable
    [ Tuple (coords 0 1) (S.singleton Candle)
    , Tuple (coords 0 0) (S.singleton Matches)
    ]
  , inventory: S.empty
  }