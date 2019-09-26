module BoardPrinter 
    ( bottomValue
    , topValue
    , format
    , printGame
    , printBoard
    ) where

import Board2048
import Game2048
import Types2048
import DirectionCounter
import Directions
import Data.List (intercalate)

printGame :: Game2048 -> IO ()
printGame game = mapM_ putStrLn output
    where
        b = board game
        output =   printBoardAndLog game
                ++ printDC (count game)
                ++ printScore b

printDC :: DirectionCounter -> [String]
printDC dc = [ "T: " ++ total ++ " | "
                        ++ up ++ "↑  " 
                        ++ left ++ "←  " 
                        ++ right ++ "→  " 
                        ++ down ++ "↓ " ]
    where
        total = show (totalCount dc)
        left = show (countFor DLeft dc)
        right = show (countFor DRight dc)
        up = show (countFor DUp dc)
        down = show (countFor DDown dc)

printScore :: Board2048 -> [String]
printScore b = ["Score: " ++ show (score b)]

printBoardAndLog :: Game2048 -> [String]
printBoardAndLog game = zipWith (++) boardAsString $ zipWith (++) separator logAsString
    where
        boardAsString = printBoard (board game)
        logAsString = printLog game
        separator = repeat printSpaceBetweenBoardAndLog

printBoard :: Board2048 -> [String]
printBoard board = 
                   [horizontalLines] ++
                   printRow (row 0)  ++
                   [horizontalLines] ++
                   printRow (row 1)  ++
                   [horizontalLines] ++
                   printRow (row 2)  ++
                   [horizontalLines] ++
                   printRow (row 3)  ++
                   [horizontalLines]
                   where
                        row = getRow board

printLog :: Game2048 -> [String]
printLog game = header ++ separator ++ logBody ++ separator
    where
        header = ["     -- Log History --"]
        separator = ["------------------------------"]
        logBody = mkLogBody . logR $ game

logBodyLength :: Int
logBodyLength = 10

mkLogBody :: [String] -> [String]
mkLogBody xs
    | length xs < logBodyLength = xs ++ (replicate (logBodyLength - length xs) "")
    | otherwise = reverse . take logBodyLength . reverse $ xs


printSpaceBetweenBoardAndLog :: String
printSpaceBetweenBoardAndLog = "   #   "


printRow :: [Int] -> [String]
printRow xs = ["|" ++ printTopRow xs ++ "|"] ++
              ["|" ++ printBottomRow xs ++ "|"]
    where
        printTopRow    = intercalate "|" . map (formatS . topValue)
        printBottomRow = intercalate "|" . map (formatS . bottomValue)
        formatS x = " " ++ format x ++ " "

bottomValue :: Int -> Int
bottomValue xs = xs `mod` 100

topValue :: Int -> Int
topValue xs = xs `div` 100

format :: Int -> String
format n
    | n == 0 = "  "
    | n < 10 = " " ++ show n
    | otherwise = show n

horizontalLines :: String
horizontalLines = "+----+----+----+----+"
