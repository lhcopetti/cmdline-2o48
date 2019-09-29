module Board2048Spec where

import Board2048
import Game2048
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Directions
import System.Random
import Test.Hspec
import Types2048

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    testViewBoard
    testNewFromArray
    testNewRandomBoard
    testReduce
    testCollapse
    testReduces
    testReplaceAt
    testMultipleReduces
    testStepping
    testAddTileToBoard

testViewBoard :: Spec
testViewBoard = describe "test view board" $
    it "should return the 2d array that the board is comprised of" $
        view newEmptyBoard `shouldBe` [[0, 0, 0, 0],[0, 0, 0, 0],[0, 0, 0, 0],[0, 0, 0, 0]]

testNewRandomBoard :: Spec
testNewRandomBoard = describe "test new random board" $ do
    it "all new random boards should have a score equal to 2" $ do
        randomBoards <- replicateM 500 (board <$> new2048GameIO)
        all ((\x -> (x == 2) || (x == 4)) . score) randomBoards `shouldBe` True


testNewFromArray :: Spec
testNewFromArray = describe "test from array" $ do
    it "should return error for empty arrays" $ do
        fromArray [] `shouldBe` Nothing
        fromArray [[], []] `shouldBe` Nothing
    it "should return a board from the constructed array" $ do
        let arr = [ [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2] ]
        view <$> (fromArray arr) `shouldBe` (Just arr)
    it "should invalidate boards with size different than 4" $ do
        let arr = [ [2, 2, 2, 2], [2, 2], [2, 2], [2, 2, 2, 2] ]
        fromArray arr `shouldBe` Nothing


testReduces :: Spec
testReduces = describe "test reduces" $ do
        it "should push all tiles to the left" $ do
            let arr = [ [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2] ]
                arr' = [ [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0] ]
            reduceLeft <$> (fromArray arr) `shouldBe` fromArray arr'
        it "should push all the tiles to the right" $ do
            let arr = [ [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0] ]
                arr' = [ [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2] ]
            reduceRight <$> (fromArray arr) `shouldBe` fromArray arr'
        it "should push all the tiles upwards" $ do
            let arr = [ [2, 0, 0, 0], [0, 2, 0, 0], [0, 0, 2, 0], [0, 0, 0, 2] ]
                arr' = [ [2, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
            reduceUp <$> (fromArray arr) `shouldBe` fromArray arr'
        it "should push all the tiles downwards" $ do
            let arr = [ [2, 0, 0, 0], [0, 2, 0, 0], [0, 0, 2, 0], [0, 0, 0, 2] ]
                arr' = [ [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [2, 2, 2, 2] ]
            reduceDown <$> (fromArray arr) `shouldBe` fromArray arr'

testMultipleReduces :: Spec
testMultipleReduces = describe "test multiple reduces" $ do
    let arr = [ [8, 0, 0, 0]
              , [0, 0, 0, 0]
              , [0, 0, 0, 0]
              , [0, 0, 0, 0]]
    it "should send the tile in the clockwise direction" $ do
        (reduceUp . reduceLeft . reduceDown . reduceRight) <$> (fromArray arr) `shouldBe` fromArray arr

    it "should sum the two tiles together" $ do
        reduceRight <$> (fromArray [ [8, 8, 0, 0]
                                   , [0, 0, 0, 0]
                                   , [0, 0, 0, 0]
                                   , [0, 0, 0, 0]]) `shouldBe` fromArray [ [0, 0, 0, 16]
                                                                        , [0, 0, 0, 0]
                                                                        , [0, 0, 0, 0]
                                                                        , [0, 0, 0, 0]]

    it "should sum the four tiles together" $ do
        (reduceRight . reduceRight) <$> (fromArray [ [8, 8, 8, 8]
                                                   , [0, 0, 0, 0]
                                                   , [0, 0, 0, 0]
                                                   , [0, 0, 0, 0]]) `shouldBe` fromArray [ [0, 0, 0, 32]
                                                                                         , [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 0]]
    it "should send the tile clockwise direction" $ do
        (reduceRight . reduceDown) <$> (fromArray [ [8, 0, 0, 0]
                                                   , [0, 0, 0, 8]
                                                   , [0, 0, 0, 0]
                                                   , [0, 0, 0, 0]]) `shouldBe` fromArray [ [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 16]]
    it "should send the tile clockwise direction" $ do
        (reduceRight . reduceDown) <$> (fromArray [ [8, 0, 0, 0]
                                                   , [0, 0, 0, 8]
                                                   , [0, 0, 0, 0]
                                                   , [0, 0, 0, 0]]) `shouldBe` fromArray [ [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 0]
                                                                                         , [0, 0, 0, 16]]

    it "should be able to reduce all tiles to only one in the bottom-right corner" $ do
        (reduceDown . reduceDown . reduceRight . reduceRight) <$> (fromArray [ [8, 0, 0, 0]
                                                                             , [0, 0, 0, 8]
                                                                             , [4, 0, 0, 4]
                                                                             , [4, 0, 0, 4]]) `shouldBe` fromArray [ [0, 0, 0, 0]
                                                                                                                     , [0, 0, 0, 0]
                                                                                                                     , [0, 0, 0, 0]
                                                                                                                     , [0, 0, 0, 32]]

    it "should be able to reduce all tiles to only one in the top-left corner" $ do
        (reduceUp . reduceUp . reduceLeft . reduceLeft) <$> (fromArray [ [16, 0, 0, 0]
                                                                       , [0, 0, 0, 16]
                                                                       , [8, 0, 0, 8]
                                                                       , [8, 0, 0, 8]]) `shouldBe` fromArray [ [64, 0, 0, 0]
                                                                                                             , [0, 0, 0, 0]
                                                                                                             , [0, 0, 0, 0]
                                                                                                             , [0, 0, 0, 0]]
    let arr2 = [[8, 0, 0, 0]
              , [0, 0, 0, 8]
              , [4, 0, 0, 4]
              , [4, 0, 0, 4]]
    let res2 = [[32, 0, 0, 0]
              , [0, 0, 0, 0]
              , [0, 0, 0, 0]
              , [0, 0, 0, 0]]
    it "should send the tile in the clockwise direction" $ do
        (reduceUp . reduceLeft . reduceDown . reduceDown . reduceRight) <$> (fromArray arr2) `shouldBe` fromArray res2


testReplaceAt :: Spec
testReplaceAt = describe "test replaceAt" $ do
    it "should replace first element of the first row" $ do
        let arr = [ [1, 2], [3, 4] ]
        replaceAt arr (0, 0) 10 `shouldBe` [ [10, 2], [3, 4] ]
    it "should replace the middle element of the middle row" $ do
        let arr = [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]
        replaceAt arr (1, 1) 19 `shouldBe` [ [1, 2, 3], [4, 19, 6], [7, 8, 9] ]
    it "should replace the last element of the last row" $ do
        let arr = [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]
        replaceAt arr (2, 2) 42 `shouldBe` [ [1, 2, 3], [4, 5, 6], [7, 8, 42] ]


testStepping :: Spec
testStepping = describe "test stepping" $ do
    it "stepping the elements upwards" $ do
        let arr1 = [[8, 0, 0, 0]
                  , [0, 0, 0, 8]
                  , [4, 0, 0, 4]
                  , [4, 0, 0, 4]]
            res1 = [[8, 0, 0, 8]
                  , [8, 0, 0, 8]
                  , [0, 4, 0, 0]
                  , [0, 0, 0, 0]]
            gen = mkStdGen 42
        stepped <- step (fromJust $ fromArrayG arr1 gen) DUp
        fromRight newEmptyBoard (board <$> stepped) `shouldBe` fromJust (fromArray res1)
    
    it "stepping the elements and adding a new tile on the same row should not be a problem" $ do
        let arr = [[0, 0, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 4, 4]]
            res = [[0, 0, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 0, 4]
                  ,[0, 0, 0, 8]]
            gen = mkStdGen 40
        stepped <- step (fromJust $ fromArrayG arr gen) DRight
        fromRight newEmptyBoard (board <$> stepped) `shouldBe` fromJust (fromArray res)

    it "stepping the elements and adding a new tile on the same column should not be a problem" $ do
        let arr = [[0, 0, 0, 0]
                  ,[0, 2, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 0, 4]]
            
            res = [[0, 0, 0, 0]
                  ,[0, 0, 0, 4]
                  ,[0, 0, 0, 0]
                  ,[0, 2, 0, 4]]
            gen = mkStdGen 42
        stepped <- step (fromJust $ fromArrayG arr gen) DDown
        fromRight newEmptyBoard (board <$> stepped) `shouldBe` fromJust (fromArray res)


testAddTileToBoard :: Spec
testAddTileToBoard = describe "test add tile to the board" $ do
    it "adding tile to the board" $ do
        let arr = [[0, 0, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 0, 8]]
            res = [[0, 0, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 0, 0]
                  ,[0, 0, 4, 8]]
            gen = mkStdGen 42
        newBoard <- runM2048Gen gen $ constructEmpty2048 =<< addTileToBoard (fromJust (fromArray arr))
        board newBoard `shouldBe` (fromJust (fromArray res))
        --evalState <$> (addTileToBoard <$> fromArray arr) <*> gen `shouldBe` fromArray res

    it "adding tile to a full board should be a noop" $ do
        let arr = [[2, 4, 2, 4]
                  ,[2, 4, 2, 4]
                  ,[2, 4, 2, 4]
                  ,[2, 4, 2, 4]]
            gen = mkStdGen 42
        newGame <- runM2048Gen gen $ constructEmpty2048 =<< addTileToBoard (fromJust (fromArray arr))
        board newGame `shouldBe` fromJust (fromArray arr)
        --evalState <$> (addTileToBoard <$> fromArray arr) <*> gen `shouldBe` fromArray arr


testCollapse :: Spec
testCollapse = describe "test collapse" $ do
    it "should return the empty list" $
        collapse [] `shouldBe` []
    it "should return the same element" $
        collapse [2] `shouldBe` [2]
    it "should not collapse the elements if they are different" $
        collapse [2, 4] `shouldBe` [2, 4]
    it "should collapse elements when they are equal" $
        collapse [2, 2] `shouldBe` [4]
    it "should collapse elements only once for each pass" $ do
        collapse [2, 2, 2, 2] `shouldBe` [4, 4]
        collapse [4, 2, 2] `shouldBe` [4, 4]

    it "empty leading spaces (equal to zero) should be removed" $ do
        collapse [0, 0, 2, 2] `shouldBe` [4]
    it "empty trailling spaces (equal to zero) should be removed" $ do
        collapse [2, 2, 0, 0] `shouldBe` [4]

testReduce :: Spec
testReduce = describe "test reduce" $ do
    it "should not change the array size" $ do
        reduce [0, 0, 0, 0, 0, 0] `shouldBe` [0, 0, 0, 0, 0, 0]
    it "when collapsing elements, should restore the array size" $ do
        reduce [2, 2, 2, 2] `shouldBe` [4, 4, 0, 0]
        reduce [0, 0, 2, 2] `shouldBe` [4, 0, 0, 0]
