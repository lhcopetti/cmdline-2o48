{-# LANGUAGE RecordWildCards #-}
module Main where

import Arrays
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (bimap)
import Data.Char
import System.IO
import System.Process
import System.Random

import BoardPrinter
import Directions
import Game2048
import Types2048
import Custom2048Boards

import Control.Concurrent (threadDelay)


main :: IO ()
main = clearScreen >> loopAnimation (horizontalBorder '2' '0' '4' '8')
    
data Animation = LA { lines :: [String]
                    , anims :: [AnimationSpec]
                    }

data AnimationSpec = AS { coord       :: (Int, Int)
                        , value       :: ValueSpec
                        , direction   :: Direction
                        , steps       :: Int
                        , currentStep :: Int
                        }


data ValueSpec = AnimatedValue String Int | StaticValue Char
    deriving (Show)

allAnimationsEnded :: Animation -> Bool
allAnimationsEnded = all animationEnded . anims

animationEnded :: AnimationSpec -> Bool
animationEnded AS {..} = currentStep == steps


stepValueSpec :: ValueSpec -> (Char, ValueSpec)
stepValueSpec (StaticValue c) = (c, StaticValue c)
stepValueSpec (AnimatedValue xs n) = let
    nextN = if n+1 >= length xs then 0 else n+1
    value = xs !! n
    in (value, AnimatedValue xs nextN)


horizontalBorder :: Char -> Char -> Char -> Char -> Animation
horizontalBorder value1 value2 value3 value4 = LA board allAnims
    where
        allAnims =  [ mkStatic (7,4) DRight value1 10
                    , mkStatic (8,4) DRight value2 10
                    , mkStatic (7,5) DRight value3 10
                    , mkStatic (8,5) DRight value4 10

                    , mkStatic (17,2) DDown value1 8
                    , mkStatic (18,2) DDown value2 8
                    , mkStatic (17,3) DDown value3 8
                    , mkStatic (18,3) DDown value4 8

                    , mkAnimated (0,0) DUp "-\\|/" 0
                    ]
        board = [ maskBorder
                , "|    |    |    |    |"
                , "|    |    |    |    |"
                , maskBorder
                , "|    |    |    |    |"
                , "|    |    |    |    |"
                , maskBorder
                , "|    |    |    |    |"
                , "|    |    |    |    |"
                , maskBorder
                , "|    |    |    |    |"
                , "|    |    |    |    |"
                , maskBorder
                ]

mkStatic :: (Int, Int) -> Direction -> Char -> Int -> AnimationSpec
mkStatic coord dir v steps = AS coord (StaticValue v) dir steps 0

mkAnimated :: (Int, Int) -> Direction -> String -> Int -> AnimationSpec
mkAnimated coord dir v steps = AS coord (AnimatedValue v 0) dir steps 0

maskBorder :: String
maskBorder = "+----+----+----+----+"

--3 -> 18
mask :: String
mask = "|    |    |    |    |"

loopAnimation :: Animation -> IO ()
loopAnimation la = do
    mapM_ putStrLn animated
    putStrLn $ show $ (allAnimationsEnded la)
    threadDelay (250 * 10 ^ 3)
    clearScreen
    if allAnimationsEnded la then
        loopAnimation (horizontalBorder '2' '0' '4' '8')
    else
        loopAnimation newLa
    where
        (animated, newLa) = stepAnimation la


stepAnimation :: Animation -> ([String], Animation)
stepAnimation l@LA {..} = let
    (newLine, newAnims) = stepAnimation' lines anims []
    in (newLine, l { anims = newAnims })

stepAnimation' :: [String] -> [AnimationSpec] -> [AnimationSpec] -> ([String], [AnimationSpec])
stepAnimation' lines [] newSpecs = (lines, newSpecs)
stepAnimation' lines (spec:specs) newSpecs = let
    (newLine, newSpec) = stepSpec lines spec
    in stepAnimation' newLine specs (newSpec:newSpecs)


stepSpec :: [String] -> AnimationSpec -> ([String], AnimationSpec)
stepSpec lines as@(AS coord valueSpec dir steps currentStep) = let
    (ch, newValueSpec) = stepValueSpec valueSpec
    computedCoord = walk coord dir currentStep
    result = replaceLines lines computedCoord ch
    newCount = if currentStep >= steps then currentStep else currentStep + 1
    in (result, as { currentStep = newCount, value = newValueSpec })


walk :: (Int, Int) -> Direction -> Int -> (Int, Int)
walk (x, y) dir count = let
    (f, g) = getVectorFromDirection count dir
    in (f x, g y)
    --multByCount = ((*count) .)
    --addVector = bimap multByCount multByCount (getVectorFromDirection dir)
    --newCoord = uncurry bimap addVector coord
    --in newCoord

getVectorFromDirection :: Int -> Direction -> (Int -> Int, Int -> Int)
getVectorFromDirection count DRight = ( (+count), id )
getVectorFromDirection count DLeft  = ( (subtract count), id )
getVectorFromDirection count DUp    = ( id, (subtract count) )
getVectorFromDirection count DDown  = ( id, (+count) )



replaceLines :: [String] -> (Int, Int) -> Char -> [String]
replaceLines lines (x, y) ch = replace y (replace x ch (lines !! y)) lines

clearScreen :: IO ()
clearScreen = callCommand "clear"

