module Main where

import Control.Monad (when)
import Control.Monad.State
import Data.Char
import Lib
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
    loop (evalState newRandomBoard stdGen)
    putStrLn "The end! I hope you are happy."

loop :: Board2048 -> IO ()
loop board = do
    clearScreen
    printBoard board
    ch <- hGetChar stdin
    when (ch /= '\ESC') (loop (update board ch))


update :: Board2048 -> Char -> Board2048
update b 'w' = reduceUp    b
update b 'a' = reduceLeft  b
update b 's' = reduceDown  b
update b 'd' = reduceRight b
update b _ = b

clearScreen :: IO ()
clearScreen = callCommand "clear"
