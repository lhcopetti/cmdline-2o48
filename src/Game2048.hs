{-# LANGUAGE RecordWildCards #-}
module Game2048
    ( new2048GameIO 
    , new2048Game
    , board
    , count
    , fromArrayG
    , step
    , winningTileValue
    , enableDevelopmentMode
    , disableDevelopmentMode
    , toggleDevelopmentMode
    ) where

import Board2048
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Either
import Directions
import LogRecord
import System.Random
import Types2048
import DirectionCounter


winningTileValue :: Int
winningTileValue = 2048

new2048GameIO :: IO Game2048
new2048GameIO = new2048Game =<< newStdGen

new2048Game :: StdGen -> IO Game2048
new2048Game gen = runM2048Gen gen (addTileToBoard newEmptyBoard >>= constructEmpty2048)

constructEmpty2048 :: Board2048 -> M2048 Game2048
constructEmpty2048 board = do
    gen <- get
    return (Game2048 board gen emptyDC emptyLogRecord False)

fromArrayG :: [[Int]] -> StdGen -> Maybe Game2048
fromArrayG xs gen = do
    b <- fromArray xs
    return (Game2048 b gen emptyDC emptyLogRecord False)

step :: Game2048 -> Direction -> IO (Either GameEnded Game2048)
step game dir = erunM2048 game (step' game dir)

step' :: Game2048 -> Direction -> EM2048 Game2048
step' Game2048 {..} dir = do
    logSeparator
    info $ "Stepping board to direction: " ++ show dir
    let stepped = stepDir board dir
        boardChanged = stepped /= board

    if not boardChanged then do
        debug $ "Board did not change, ignoring input"
        return (Game2048 board gen count logR devel)
    else do

        when (gameIsWon stepped) $ do
            info $ "You have reached the unbelievable " ++ show winningTileValue ++ " score. Congratulations!"
            left (YouWon (Game2048 stepped gen count logR devel))

        withNewTile <- addTileToBoard stepped

        when (gameIsLost withNewTile) $ do
            info $ "I am sorry to inform you that you have just lost the game"
            left (YouLose (Game2048 withNewTile gen count logR devel))

        let emptySlots = emptySlotsCount withNewTile
        when (emptySlots > 0 && emptySlots <= 3) $
            warn $ "Careful, you currently only have " ++ show emptySlots ++ " empty slots available"
        when (emptySlots == 0) $
            warn "You have run OUT OF SPACE. Careful not to lose the game"


        gen' <- get
        let newDC = incCountFor dir count
         in return (Game2048 withNewTile gen' newDC logR devel)

gameIsWon :: Board2048 -> Bool
gameIsWon b = highestTileValue b == winningTileValue

gameIsLost :: Board2048 -> Bool
gameIsLost b = all (== b) validMoves
    where
        validMoves = [reduceLeft, reduceRight, reduceUp, reduceDown] <*> pure b

stepDir :: Board2048 -> Direction -> Board2048
stepDir b DUp = reduceUp b
stepDir b DDown = reduceDown b
stepDir b DLeft = reduceLeft b
stepDir b DRight = reduceRight b

enableDevelopmentMode :: Game2048 -> Game2048
enableDevelopmentMode g = g { devel = True }

disableDevelopmentMode :: Game2048 -> Game2048
disableDevelopmentMode g = g { devel = False }

toggleDevelopmentMode :: Game2048 -> Game2048
toggleDevelopmentMode g = g { devel = not (devel g) }
