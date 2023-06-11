module Data.Coords where

import Prelude


newtype Coords = Coords
  { x :: Int
  , y :: Int
  }

derive newtype instance Show Coords
derive instance Eq Coords
derive instance Ord Coords


coords :: Int -> Int -> Coords
coords x y = Coords { x, y }

prettyPrintCoords :: Coords -> String
prettyPrintCoords (Coords p) = "(" <> show p.x <> ", " <> show p.y <> ")"
