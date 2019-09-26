{-# LANGUAGE RecordWildCards #-}
module Game2048
    ( new2048GameIO 
    , new2048Game
    , board
    , count
    , fromArrayG
    , step
    ) where

import Board2048
import Control.Monad.State
import Control.Monad.Writer
import Directions
import LogRecord
import System.Random
import Types2048
import DirectionCounter

new2048GameIO :: IO Game2048
new2048GameIO = return . new2048Game =<< newStdGen

new2048Game :: StdGen -> Game2048
new2048Game gen = runM2048Gen gen (addTileToBoard newEmptyBoard >>= constructEmpty2048)

constructEmpty2048 :: Board2048 -> M2048 Game2048
constructEmpty2048 board = do
    gen <- get
    return (Game2048 board gen emptyDC emptyLogRecord)

fromArrayG :: [[Int]] -> StdGen -> Maybe Game2048
fromArrayG xs gen = do
    b <- fromArray xs
    return (Game2048 b gen emptyDC emptyLogRecord)

step :: Game2048 -> Direction -> Game2048
step game dir = runM2048 game (step' game dir)

step' :: Game2048 -> Direction -> M2048 Game2048
step' (Game2048 board gen dc lr) dir = do
    logSeparator
    info $ "Stepping board to direction: " ++ show dir
    let stepped = stepDir board dir
        boardChanged = stepped /= board

    if not boardChanged then do
        debug $ "Board did not change, ignoring input"
        return (Game2048 board gen dc lr)
    else do
        withNewTile <- addTileToBoard stepped

        let emptySlots = emptySlotsCount withNewTile
        when (emptySlots > 0 && emptySlots <= 3) $
            warn $ "Careful, you currently only have " ++ show emptySlots ++ " empty slots available"
        when (emptySlots == 0) $
            warn "You have run OUT OF SPACE. Careful not to lose the game"




        gen' <- get
        let newDC = incCountFor dir dc
         in return (Game2048 withNewTile gen' newDC lr)

stepDir :: Board2048 -> Direction -> Board2048
stepDir b DUp = reduceUp b
stepDir b DDown = reduceDown b
stepDir b DLeft = reduceLeft b
stepDir b DRight = reduceRight b

