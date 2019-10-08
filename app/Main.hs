{-# LANGUAGE RecordWildCards #-}
module Main where

import Arrays
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (bimap)
import Data.Char
import System.IO
import System.Process
import System.Random

import BoardPrinter
import Directions
import Game2048
import Types2048
import Custom2048Boards

import Control.Concurrent (threadDelay)


loop :: Game2048 -> IO (Maybe GameEnded)
loop game = do
    render game
    ch <- getChar
    if ch /= '\ESC' then do
        updated <- update game ch
        case updated of
            Right g -> loop g
            Left ended -> return (Just ended)
    else return Nothing

render :: Game2048 -> IO ()
render game = clearScreen >> printGame game

update :: Game2048 -> Char -> IO (Either GameEnded Game2048)
update g 'w' = step g DUp
update g 'a' = step g DLeft
update g 's' = step g DDown
update g 'd' = step g DRight
update g 'q' = Right <$> devNewAlmostWinningGame g
update g 'e' = Right <$> devNewAlmostLosingGame g
update g 'x' = return . Right . toggleDevelopmentMode $ g
update g ' ' = Right <$> reset2048Game g
update g 'c' = getLine >>= \newBoard -> Right <$> devReplace2048BoardFor newBoard g
update g _ = return (Right g)

handleGameEnded :: GameEnded -> IO ()
handleGameEnded (YouWon b) = render b >> newLine >> putStrLn "You won the game. Congratulations!!!"
handleGameEnded (YouLose b) = render b >> newLine >> putStrLn "You lost, better luck next time. Practice makes it perfect."

clearScreen :: IO ()
clearScreen = callCommand "clear"

newLine :: IO ()
newLine = putStrLn ""


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    game <- loop =<< new2048GameIO
    case game of
        Nothing -> putStrLn "You have quit the game. Please, come back soon!"
        Just gEnded -> handleGameEnded gEnded
    newLine
    putStrLn "The end! I hope you are happy."
