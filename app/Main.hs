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


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    game <- loop =<< new2048GameIO
    putStrLn "The end! I hope you are happy."
    mapM_ putStrLn (logR game)

loop :: Game2048 -> IO Game2048
loop game = do
    clearScreen
    printGame game
    ch <- hGetChar stdin
    if (ch /= '\ESC') then loop (update game ch)
    else return game

    


update :: Game2048 -> Char -> Game2048
update g 'w' = step g DUp
update g 'a' = step g DLeft
update g 's' = step g DDown
update g 'd' = step g DRight
update g _ = g

clearScreen :: IO ()
clearScreen = callCommand "clear"
