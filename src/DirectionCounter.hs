module DirectionCounter
    ( DirectionCounter
    , emptyDC
    , countFor
    , totalCount
    , incCountFor
    ) where

import qualified Data.Map as M
import           Data.Map (Map)
import           Directions

data DirectionCounter = DC (Map Direction Int)

emptyDC :: DirectionCounter
emptyDC = DC M.empty

countFor :: Direction -> DirectionCounter -> Int
countFor dir (DC map) = M.findWithDefault 0 dir map

totalCount :: DirectionCounter -> Int
totalCount (DC map) = sum . M.elems $ map

incCountFor :: Direction -> DirectionCounter -> DirectionCounter
incCountFor dir (DC map) = DC $ M.insertWith (+) dir 1 map
