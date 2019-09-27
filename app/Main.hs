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
    putStrLn "The end! I hope you are happy."

loop :: Game2048 -> IO Game2048
loop game = do
    clearScreen
    printGame game
    ch <- hGetChar stdin
    if (ch /= '\ESC') then update game ch >>= loop
    else return game

    
update :: Game2048 -> Char -> IO Game2048
update g 'w' = return $ step g DUp
update g 'a' = return $ step g DLeft
update g 's' = return $ step g DDown
update g 'd' = return $ step g DRight
update g 'q' = newAlmostWinningBoard
update g ' ' = new2048GameIO
update g _ = return g

clearScreen :: IO ()
clearScreen = callCommand "clear"
