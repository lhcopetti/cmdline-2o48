module Board2048 (
    Board2048,
    newBoard,
    isEmpty,
    view,
    score,
    fromArray,
    newRandomBoard,
    getRow,

    reduceLeft,
    reduceRight,
    reduceUp,
    reduceDown,

    cols,
    rows,

    replaceAt,

    step,
    Direction (..),

    addTileToBoard


    ) where

import Control.Monad.State
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import System.Random
import Lib (reduce)

data Board2048 = Board2048 [[Int]]
    deriving (Show, Eq)

defaultSize :: Int
defaultSize = 4

newBoard :: Board2048
newBoard = Board2048 (replicate defaultSize (replicate defaultSize 0))

isEmpty :: Board2048 -> Bool
isEmpty (Board2048 b) = all (== 0) . concat $ b

view :: Board2048 -> [[Int]]
view (Board2048 b) = b

score :: Board2048 -> Int
score (Board2048 b) = sum . concat $ b

fromArray :: [[Int]] -> Maybe Board2048
fromArray xs = do
    guard (not . null $ xs)
    guard (all (not . null) xs)
    guard (all ((== defaultSize) . length) xs)
    return (Board2048 xs)

newRandomBoard :: State StdGen Board2048
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


stRandomR :: (Int, Int) -> State StdGen Int
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


addTileToBoard :: Board2048 -> State StdGen Board2048
addTileToBoard (Board2048 b) = do
    let coord = zip [0..] (concat b)
        onlyZeros = filter ((== 0) . snd) coord
    if null onlyZeros then
        return (Board2048 b)
    else do
        randomIdx <- getRandomFromPool onlyZeros
        newRandomTile <- getNewRandomTile
        let newB = replaceAt b (makeCoord randomIdx) newRandomTile
        return (Board2048 newB)

makeCoord :: Int -> (Int, Int)
makeCoord idx = (idx `div` defaultSize, idx `mod` defaultSize)

type Coord = Int
getRandomFromPool :: [(Coord, Int)] -> State StdGen Coord
getRandomFromPool xs = do
    rndIndex <- stRandomR (0, length xs - 1)
    return . fst . (!! rndIndex) $ xs


getNewRandomTile :: State StdGen Int
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

data Direction = DUp | DLeft | DRight | DDown

step :: Board2048 -> Direction -> State StdGen Board2048
step board dir = do
    let stepped = stepDir board dir
    if stepped /= board then
        addTileToBoard stepped
    else
        return stepped

stepDir :: Board2048 -> Direction -> Board2048
stepDir b DUp = reduceUp b
stepDir b DDown = reduceDown b
stepDir b DLeft = reduceLeft b
stepDir b DRight = reduceRight b
