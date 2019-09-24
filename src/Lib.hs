module Lib
    ( collapse
    , reduce
    ) where

reduce :: [Int] -> [Int]
reduce xs = let oldSize = length xs
                newXs = collapse xs
                newSize = length newXs
            in  newXs ++ (replicate (oldSize - newSize) 0)

collapse :: [Int] -> [Int]
collapse = collapse' . filter (/= 0) 

collapse' :: [Int] -> [Int]
collapse' [] = []
collapse' [x] = [x]
collapse' (x:x':xs)
    | x == x' = x * x' : collapse' xs
    | otherwise = x : collapse' (x':xs)
