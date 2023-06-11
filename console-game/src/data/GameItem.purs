module Data.GameItem where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)


data GameItem = Candle | Matches

derive instance Eq GameItem
derive instance Ord GameItem

-- This is the somewhat unfortunate standard pattern for auto-deriving quite a
-- few type calsses that Haskell would be able to auto-derive, see
-- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#deriving-from-generic.
derive instance genericGameItem ∷ Generic GameItem _
instance Show GameItem where
  show = genericShow


readItem :: String -> Maybe GameItem
readItem "Candle" = Just Candle
readItem "Matches" = Just Matches
readItem _ = Nothing