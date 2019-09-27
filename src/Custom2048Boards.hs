module Custom2048Boards
    ( newAlmostWinningBoard
    , newAlmostLostBoard
    ) where

import System.Random (newStdGen)
import Data.Maybe (fromJust)

import Game2048
import Types2048

newAlmostWinningBoard :: IO Game2048
newAlmostWinningBoard = newStdGen >>= return . fromJust . fromArrayG arr
    where arr = [ [1024, 0, 0, 0]
                , [0, 0, 0, 0 ]
                , [0, 0, 0, 0 ]
                , [0, 0, 0, 1024]
                ]

newAlmostLostBoard :: IO Game2048
newAlmostLostBoard = newStdGen >>= return . fromJust . fromArrayG arr
    where arr = [ [64, 32, 64, 32]
                , [32, 64, 8, 8 ]
                , [64, 32, 64, 32]
                , [32, 64, 32, 64]
                ]
