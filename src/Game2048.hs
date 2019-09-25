
module Game2048
    ( Game2048
    , Direction (..)
    , new2048GameIO 
    , new2048Game
    , board
    , fromArrayG
    , step
    ) where

import Board2048
import Control.Monad.State
import System.Random

data Game2048 = Game2048 Board2048 StdGen

data Direction = DUp | DLeft | DRight | DDown

new2048GameIO :: IO Game2048
new2048GameIO = return . new2048Game =<< newStdGen

new2048Game :: StdGen -> Game2048
new2048Game gen = 
    let board = newEmptyBoard
        (b', gen') = runState (addTileToBoard board) gen
     in Game2048 b' gen'

board :: Game2048 -> Board2048
board (Game2048 b _) = b

fromArrayG :: [[Int]] -> StdGen -> Maybe Game2048
fromArrayG xs gen = do
    b <- fromArray xs
    return (Game2048 b gen)

step :: Game2048 -> Direction -> Game2048
step (Game2048 board gen) dir = do
    let stepped = stepDir board dir
    if stepped == board then Game2048 board gen
    else let (b', gen') = runState (addTileToBoard stepped) gen
         in Game2048 b' gen'

stepDir :: Board2048 -> Direction -> Board2048
stepDir b DUp = reduceUp b
stepDir b DDown = reduceDown b
stepDir b DLeft = reduceLeft b
stepDir b DRight = reduceRight b

