module Main where

import Control.Monad (when)
import Control.Monad.State
import Data.Char
import System.IO
import System.Process
import System.Random

import Board2048
import BoardPrinter

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    stdGen <- newStdGen
    let (board, stdGen') = runState newRandomBoard stdGen
    loop board stdGen'
    putStrLn "The end! I hope you are happy."

loop :: Board2048 -> StdGen -> IO ()
loop board stdGen = do
    clearScreen
    printBoard board
    ch <- hGetChar stdin
    when (ch /= '\ESC') $ do
        let (b', s') = runState (update board ch) stdGen
        loop b' s'


update :: Board2048 -> Char -> State StdGen Board2048
update b 'w' = step b DUp
update b 'a' = step b DLeft
update b 's' = step b DDown
update b 'd' = step b DRight
update b _ = return b

clearScreen :: IO ()
clearScreen = callCommand "clear"
