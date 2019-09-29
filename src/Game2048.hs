{-# LANGUAGE RecordWildCards #-}
module Game2048
    ( new2048GameIO 
    , new2048Game
    , constructEmpty2048
    , devReset2048Game
    , gameScore
    , fromArrayG
    , step
    , winningTileValue
    , enableDevelopmentMode
    , disableDevelopmentMode
    , toggleDevelopmentMode

    , devNewAlmostWinningGame
    , devNewAlmostLosingGame
    , devReplace2048BoardFor
    ) where

import Board2048
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Either
import Custom2048Boards
import Data.Maybe
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
new2048Game gen = runM2048Gen gen $ do
    info $ "Creating new board with StdGen = " ++ show gen
    addTileToBoard newEmptyBoard >>= constructEmpty2048

constructEmpty2048 :: Board2048 -> M2048 Game2048
constructEmpty2048 board = do
    gen <- get
    return (Game2048 board gen emptyDC emptyLogRecord False)

update2048Board :: Board2048 -> Game2048 -> Game2048
update2048Board b g = g { board = b }

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

gameScore :: Game2048 -> Int
gameScore Game2048 {..} = score board

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

runInDevelopmentMode :: Monad m => (Game2048 -> m Game2048) -> Game2048 -> m Game2048
runInDevelopmentMode m g = if devel g then m g else return g

reset2048Game :: Game2048 -> IO Game2048
reset2048Game g = runM2048Gen (gen g) $ do
    logSeparator
    info $ "Resetting board to empty state. Score was: " ++ show (gameScore g) 
    newRandomBoard <- addTileToBoard newEmptyBoard
    return (update2048Board newRandomBoard g)

devReset2048Game :: Game2048 -> IO Game2048
devReset2048Game g = runInDevelopmentMode reset2048Game g

replace2048BoardFor :: String -> Game2048 -> IO Game2048
replace2048BoardFor input g = runM2048Gen (gen g) $ do
    logSeparator
    info $ "Replacing board with user-entered custom input. The score was: " ++ show (gameScore g) 
    case boardFromString input of
        Nothing -> do
            warn $ "The input is not a valid board. [" ++ show input ++ "]" 
            return g
        Just newBoard -> do
            info $ "The input is valid. Initial score is: " ++ show (score newBoard) 
            return (update2048Board newBoard g)

devReplace2048BoardFor :: String -> Game2048 -> IO Game2048
devReplace2048BoardFor = runInDevelopmentMode . replace2048BoardFor

newAlmostWinningGame :: Game2048 -> IO Game2048
newAlmostWinningGame g = runM2048Gen (gen g) $ do
    info $ "Setting board up for a really close win"
    let newBoard = fromJust (fromArray newAlmostWinningBoard)
    return (update2048Board newBoard g)

devNewAlmostWinningGame :: Game2048 -> IO Game2048
devNewAlmostWinningGame g = runInDevelopmentMode newAlmostWinningGame g

newAlmostLosingGame :: Game2048 -> IO Game2048
newAlmostLosingGame g = runM2048Gen (gen g) $ do
    info $ "Setting board up for a really ugly loss"
    let newBoard = fromJust (fromArray newAlmostLostBoard)
    return (update2048Board newBoard g)

devNewAlmostLosingGame :: Game2048 -> IO Game2048
devNewAlmostLosingGame g = runInDevelopmentMode newAlmostLosingGame g
