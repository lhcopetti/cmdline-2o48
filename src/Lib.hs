module Lib
    ( someFunc
    , reduce
    ) where




someFunc :: IO ()
someFunc = putStrLn "someFunc"


reduce :: [Int] -> [Int]
reduce = reduce' . filter (/= 0) 

reduce' :: [Int] -> [Int]
reduce' [] = []
reduce' [x] = [x]
reduce' (x:x':xs)
    | x == x' = x * x' : reduce xs
    | otherwise = x : reduce (x':xs)
