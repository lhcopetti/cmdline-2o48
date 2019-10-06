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
    testReplaceAt
    testStepping
    testAddTileToBoard
    testNewFromArrayExtend


testViewBoard :: Spec
testViewBoard = describe "test view board" $
    it "should return the 2d array that the board is comprised of" $
        view newEmptyBoard `shouldBe` [[0, 0, 0, 0],[0, 0, 0, 0],[0, 0, 0, 0],[0, 0, 0, 0]]

testNewRandomBoard :: Spec
testNewRandomBoard = describe "test new random board" $
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
        view <$> fromArray arr `shouldBe` Just arr
    it "should invalidate boards with size different than the default" $ do
        let arr = [ [2, 2, 2, 2], [2, 2], [2, 2], [2, 2, 2, 2] ]
        fromArray arr `shouldBe` Nothing
    it "should invalidate boards that contain values less than zero" $ do
        let arr = [ [-200, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2] ]
        fromArray arr `shouldBe` Nothing
    it "should invalidate boards that contain tailes are not power of 2" $ do
        let arr = [ [2, 2, 2, 2], [2, 2, 2, 2], [3, 2, 2, 2], [2, 2, 2, 2] ]
        fromArray arr `shouldBe` Nothing
        let arr1 = [ [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 1023] ]
        fromArray arr1 `shouldBe` Nothing

testNewFromArrayExtend :: Spec
testNewFromArrayExtend = describe "test from array extend" $ do
    it "incomplete arrays will be padded with zeros (rows)" $ do
        let arr = [ [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2] ]
            res = [ [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 2], [2, 2, 2, 0] ]
        view <$> fromArrayExtend arr `shouldBe` Just res
    it "incomplete arrays will be padded with zeros (columns)" $ do
        let arr = [ [2, 2, 2 ], [2, 2, 2 ], [2, 2, 2 ], [2, 2, 2] ]
            res = [ [2, 2, 2, 0], [2, 2, 2, 0], [2, 2, 2, 0], [2, 2, 2, 0] ]
        view <$> fromArrayExtend arr `shouldBe` Just res
    it "incomplete arrays will be padded with zeros (single value)" $ do
        let arr = [ [ 2048 ] ]
            res = [ [2048, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
        view <$> fromArrayExtend arr `shouldBe` Just res
    it "empty arrays are not allowed" $ do
        fromArrayExtend [ [] ] `shouldBe` Nothing
        fromArrayExtend [] `shouldBe` Nothing
    
        

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
        board newBoard `shouldBe` fromJust (fromArray res)
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
