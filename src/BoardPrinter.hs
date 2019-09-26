module BoardPrinter 
    ( bottomValue
    , topValue
    , format
    , printGame
    , printBoard
    ) where

import Board2048
import Game2048
import DirectionCounter
import Directions
import Data.List (intercalate)

printGame :: Game2048 -> IO ()
printGame game = mapM_ putStrLn output
    where
        b = board game
        output =   printBoard b
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
