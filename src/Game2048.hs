module Game2048
    ( Game2048
    , new2048GameIO 
    , new2048Game
    , board
    , count
    , fromArrayG
    , step
    ) where

import Board2048
import Control.Monad.State
import Directions
import System.Random
import DirectionCounter

data Game2048 = Game2048 
    { board :: Board2048
    , gen   :: StdGen
    , count :: DirectionCounter
    }

new2048GameIO :: IO Game2048
new2048GameIO = return . new2048Game =<< newStdGen

new2048Game :: StdGen -> Game2048
new2048Game gen = 
    let board = newEmptyBoard
        (b', gen') = runState (addTileToBoard board) gen
     in Game2048 b' gen' emptyDC

fromArrayG :: [[Int]] -> StdGen -> Maybe Game2048
fromArrayG xs gen = do
    b <- fromArray xs
    return (Game2048 b gen emptyDC)

step :: Game2048 -> Direction -> Game2048
step (Game2048 board gen dc) dir = do
    let stepped = stepDir board dir
        boardChanged = stepped /= board
    if not boardChanged then Game2048 board gen dc
    else 
        let (b', gen') = runState (addTileToBoard stepped) gen
            newDC = incCountFor dir dc
         in Game2048 b' gen' newDC

stepDir :: Board2048 -> Direction -> Board2048
stepDir b DUp = reduceUp b
stepDir b DDown = reduceDown b
stepDir b DLeft = reduceLeft b
stepDir b DRight = reduceRight b

