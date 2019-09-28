module Custom2048Boards
    ( newAlmostWinningBoard
    , newAlmostLostBoard
    ) where

import System.Random (newStdGen)
import Data.Maybe (fromJust)

import Types2048

newAlmostWinningBoard :: [[Int]]
newAlmostWinningBoard = [ [1024, 0, 0, 0]
                        , [0, 0, 0, 0 ]
                        , [0, 0, 0, 0 ]
                        , [0, 0, 0, 1024]
                        ]        

newAlmostLostBoard :: [[Int]]
newAlmostLostBoard = [ [64, 32, 64, 32]
                     , [32, 64, 8, 8 ]
                     , [64, 32, 64, 32]
                     , [32, 64, 32, 64]
                     ]
