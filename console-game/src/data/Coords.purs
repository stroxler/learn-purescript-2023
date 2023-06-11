module Data.Coords where

import Prelude


newtype Coords = Coords
  { x :: Int
  , y :: Int
  }

derive instance Eq Coords
derive instance Ord Coords

-- For newtypes (but not other simple types) we can derive show directly
-- via newtype deriving, see
-- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#derive-from-newtype
derive newtype instance Show Coords


coords :: Int -> Int -> Coords
coords x y = Coords { x, y }

prettyPrintCoords :: Coords -> String
prettyPrintCoords (Coords p) = "(" <> show p.x <> ", " <> show p.y <> ")"
