module Custom2048Boards
    ( newAlmostWinningBoard
    , newAlmostLostBoard
    , boardFromString
    ) where

import Board2048
import Control.Monad (guard, mapM)
import System.Random (newStdGen)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

import Types2048

newAlmostWinningBoard :: Board2048
newAlmostWinningBoard = fromJust $ fromArray [ [1024, 0, 0, 0]
                                             , [0, 0, 0, 0 ]
                                             , [0, 0, 0, 0 ]
                                             , [0, 0, 0, 1024]
                                             ]        

newAlmostLostBoard :: Board2048
newAlmostLostBoard = fromJust $ fromArray [ [64, 32, 64, 32]
                                          , [32, 64, 8, 8 ]
                                          , [64, 32, 64, 32]
                                          , [32, 64, 32, 64]
                                          ]


boardFromString :: String -> Maybe Board2048
boardFromString x = chunksOf 4 <$> (mapM toInt . splitOn "," . filterS $ x) >>= fromArray
    where
        filterS = filter (/= ' ')

toInt :: String -> Maybe Int
toInt x = do
    let result = reads x
    guard (not . null $ result)
    let (n, rest) = head result
    guard (null rest)
    return n


chunksOf :: Int -> [Int] -> [[Int]]
chunksOf _ [] = []
chunksOf n xs
    | n <= 0 = error ("Zero or negative n: " ++ show n)
    | otherwise = (take n xs) : (chunksOf n (drop n xs))


