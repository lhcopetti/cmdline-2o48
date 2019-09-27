module Main where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import System.IO
import System.Process
import System.Random

import BoardPrinter
import Directions
import Game2048
import Types2048
import Custom2048Boards


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

loop :: Game2048 -> IO (Maybe GameEnded)
loop game = do
    render game
    ch <- hGetChar stdin
    if (ch /= '\ESC') then do
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
update g 'q' = liftM Right newAlmostWinningBoard
update g 'e' = liftM Right newAlmostLostBoard
update g ' ' = liftM Right new2048GameIO
update g _ = return (Right g)

handleGameEnded :: GameEnded -> IO ()
handleGameEnded (YouWon b) = render b >> newLine >> putStrLn "You won the game. Congratulations!!!"
handleGameEnded (YouLose b) = render b >> newLine >> putStrLn "You lost, better luck next time. Practice makes it perfect."

clearScreen :: IO ()
clearScreen = callCommand "clear"

newLine :: IO ()
newLine = putStrLn ""
