module Main where

import Control.Monad (when)
import Control.Monad.State
import Data.Char
import System.IO
import System.Process
import System.Random

import Game2048
import BoardPrinter

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    loop =<< new2048GameIO
    putStrLn "The end! I hope you are happy."

loop :: Game2048 -> IO ()
loop game = do
    clearScreen
    printBoard (board game)
    ch <- hGetChar stdin
    when (ch /= '\ESC') $ loop (update game ch)


update :: Game2048 -> Char -> Game2048
update g 'w' = step g DUp
update g 'a' = step g DLeft
update g 's' = step g DDown
update g 'd' = step g DRight
update g _ = g

clearScreen :: IO ()
clearScreen = callCommand "clear"
