{-# LANGUAGE FlexibleContexts #-}
module Board2048 (

    newEmptyBoard,
    newRandomBoard,

    Board2048,
    view,
    score,
    fromArray,

    getRow,

    reduceLeft,
    reduceRight,
    reduceUp,
    reduceDown,

    cols,
    rows,

    replaceAt,

    addTileToBoard,

    reduce,
    collapse,

    emptySlotsCount,
    highestTileValue
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (liftM)
import Data.Fixed (mod')
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import LogRecord
import System.Random
import Types2048

defaultSize :: Int
defaultSize = 4

newEmptyBoard :: Board2048
newEmptyBoard = Board2048 (replicate defaultSize (replicate defaultSize 0))

emptySlotsCount :: Board2048 -> Int
emptySlotsCount = length . filter (==0) . concat . view

highestTileValue :: Board2048 -> Int
highestTileValue = maximum . concat . view

view :: Board2048 -> [[Int]]
view (Board2048 b) = b

score :: Board2048 -> Int
score (Board2048 b) = sum . concat $ b


fromArray :: [[Int]] -> Maybe Board2048
fromArray xs = do
    guard (not . null $ xs)
    guard (all ((== defaultSize) . length) xs)
    guard (all (>=0) . concat $ xs)
    guard (all powerOf2OrZero . concat $ xs)
    return (Board2048 xs)

powerOf2OrZero :: Int -> Bool
powerOf2OrZero = (||) <$> powerOf2 <*> (==0)

powerOf2 :: Int -> Bool
powerOf2 = (== 0) . (`mod'` 1) . logBase (fromIntegral 2) . fromIntegral

newRandomBoard :: M2048 Board2048
newRandomBoard = do
    count <- stRandomR (0, 15)
    let left = replicate count 0
        right = replicate (15 - count) 0
        arr = left ++ [2] ++ right
        in
        return . fromJust . fromArray . chunksOf defaultSize $ arr

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf _ [] = []
chunksOf n xs
    | n <= 0 = error ("Zero or negative n: " ++ show n)
    | otherwise = (take n xs) : (chunksOf n (drop n xs))


stRandomR :: MonadState StdGen m => (Int, Int) -> m Int
stRandomR (lo, hi) = do
    s <- get
    let (x, s') = randomR (lo, hi) s
    put s'
    return x

getRow :: Board2048 -> Int -> [Int]
getRow (Board2048 b) row = b !! row

reduceLeft :: Board2048 -> Board2048
reduceLeft (Board2048 b) = Board2048 (map reduce b)

reduceRight :: Board2048 -> Board2048
reduceRight (Board2048 b) = Board2048 (map (reverse . reduce . reverse) b)

reduceUp :: Board2048 -> Board2048
reduceUp (Board2048 b) = Board2048 . cols . map reduce . cols $ b

reduceDown :: Board2048 -> Board2048
reduceDown (Board2048 b) = Board2048 . cols . map (reverse . reduce . reverse) . cols $ b

rows :: [[Int]] -> [[Int]]
rows = id

cols :: [[Int]] -> [[Int]]
cols [] = []
cols xss = map head xss : cols (filter (not . null) . map tail $ xss)

addTileToBoard :: ( MonadState StdGen m
                  , MonadWriter LogRecord m
                  , MonadReader UTCTime m
                  ) => Board2048 -> m Board2048
addTileToBoard (Board2048 b) = do
    let coord = zip [0..] (concat b)
        onlyZeros = filter ((== 0) . snd) coord

    info $ "There are " ++ show (length onlyZeros) ++ " slot candidates for the new tile"

    if null onlyZeros then do
        warn "the grid is full, no slot available to fill"
        return (Board2048 b)
    else do
        randomCoord <- makeCoord <$> (getRandomFromPool onlyZeros)
        newRandomTile <- getNewRandomTile
        debug $ "Filling coord: " ++ show randomCoord ++ " with value " ++ show newRandomTile
        let newB = replaceAt b randomCoord newRandomTile
        return (Board2048 newB)

makeCoord :: Int -> (Int, Int)
makeCoord idx = (idx `div` defaultSize, idx `mod` defaultSize)

type Coord = Int
getRandomFromPool :: MonadState StdGen m => [(Coord, Int)] -> m Coord
getRandomFromPool xs = do
    rndIndex <- stRandomR (0, length xs - 1)
    return . fst . (!! rndIndex) $ xs


getNewRandomTile :: MonadState StdGen m => m Int
getNewRandomTile = liftM (*2) (stRandomR (1, 2))

replaceValue :: [Int] -> Coord -> Int -> [Int]
replaceValue xs x n = take x xs ++ [n] ++ rest
    where
        rest = drop (x+1) xs

replaceAt :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
replaceAt xss (x, y) n = take x xss ++ [replaceValue row y n] ++ rest
    where
        row = xss !! x
        rest = drop (x+1) xss


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
    | x == x' = x + x' : collapse' xs
    | otherwise = x : collapse' (x':xs)
