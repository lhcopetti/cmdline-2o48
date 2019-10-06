module Core2048
    ( reduce
    , collapse

    , reduceL
    , reduceR
    , reduceU
    , reduceD

    , rows
    , cols
    ) where

reduceL :: [[Int]] -> [[Int]]
reduceL = map reduce

reduceR :: [[Int]] -> [[Int]]
reduceR = map (reverse . reduce . reverse)

reduceU :: [[Int]] -> [[Int]]
reduceU = cols . map reduce . cols

reduceD :: [[Int]] -> [[Int]]
reduceD = cols . map (reverse . reduce . reverse) . cols

reduce :: [Int] -> [Int]
reduce xs = let oldSize = length xs
                newXs = collapse xs
                newSize = length newXs
            in  newXs ++ replicate (oldSize - newSize) 0

collapse :: [Int] -> [Int]
collapse = collapse' . filter (/= 0) 

collapse' :: [Int] -> [Int]
collapse' [] = []
collapse' [x] = [x]
collapse' (x:x':xs)
    | x == x' = x + x' : collapse' xs
    | otherwise = x : collapse' (x':xs)


rows :: [[Int]] -> [[Int]]
rows = id

cols :: [[Int]] -> [[Int]]
cols [] = []
cols xss = map head xss : cols (filter (not . null) . map tail $ xss)

