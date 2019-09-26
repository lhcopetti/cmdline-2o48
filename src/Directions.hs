module Directions
    ( Direction (..)
    ) where

data Direction = DUp | DLeft | DRight | DDown
        deriving (Show, Eq, Ord)
