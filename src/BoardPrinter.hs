module BoardPrinter 
    ( bottomValue
    , topValue
    , format
    , printBoard
    , boardToString
    ) where

import Board2048
import Data.List (intercalate)


printBoard :: Board2048 -> IO ()
printBoard = mapM_ putStrLn . boardToString

boardToString :: Board2048 -> [String]
boardToString board = 
                   [horizontalLines] ++
                   printRow (row 0)  ++
                   [horizontalLines] ++
                   printRow (row 1)  ++
                   [horizontalLines] ++
                   printRow (row 2)  ++
                   [horizontalLines] ++
                   printRow (row 3)  ++
                   [horizontalLines] ++
                   printScore board
                   where
                        row = getRow board

printRow :: [Int] -> [String]
printRow xs = ["|" ++ printTopRow xs ++ "|"] ++
              ["|" ++ printBottomRow xs ++ "|"]
    where
        printTopRow    = intercalate "|" . map (formatS . topValue)
        printBottomRow = intercalate "|" . map (formatS . bottomValue)
        formatS x = " " ++ format x ++ " "

printScore :: Board2048 -> [String]
printScore b = ["Score: " ++ show (score b)]

bottomValue :: Int -> Int
bottomValue xs = xs `mod` 100

topValue :: Int -> Int
topValue xs = xs `div` 100

format :: Int -> String
--format n = show (n `div` 10) ++ show (n `mod` 10)
format n
    | n == 0 = "  "
    | n < 10 = " " ++ show n
    | otherwise = show n

horizontalLines :: String
horizontalLines = "+----+----+----+----+"
