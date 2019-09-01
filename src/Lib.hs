module Lib
    ( someFunc
    , validateTestFramemework
    , reduce
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


validateTestFramemework :: Int -> Int
validateTestFramemework = (*2)

reduce :: [Int] -> [Int]
reduce [] = []
reduce [x] = [x]
reduce (x:x':xs)
    | x == x' = x * x' : reduce xs
    | otherwise = x : reduce (x':xs)
