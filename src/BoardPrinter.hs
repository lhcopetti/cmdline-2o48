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
                ++ [""] ++ [""]
                ++ printControls game

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

printControls :: Game2048 -> [String]
printControls g = [ "At any time, press <ESC> or any of the <arrow keys> to exit"
                , "Press <SPACE> to reset"
                ] ++ printDevelopmentOptions g

printDevelopmentOptions :: Game2048 -> [String]
printDevelopmentOptions g = if devel g then printDevelOptions else printDevelHelp
    where
        printDevelHelp =  [ "Press <x> to enable development options" ]
    
        printDevelOptions = [ "Press <q> to use a board that is really close to winning"
                            , "Press <e> to use a board that is really close to losing"
                            , "Press <c> to use a custom board, then type 16 comma-separated values"
                            ]


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
