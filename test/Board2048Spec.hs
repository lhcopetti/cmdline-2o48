module Board2048Spec where

import Board2048
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import System.Random
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    testNewBoard
    testViewBoard
    testNewFromArray
    testNewRandomBoard
    testReduces

testNewBoard :: Spec
testNewBoard = describe "test new board" $ do
    it "newBoard should set up an empty board" $
        isEmpty newBoard `shouldBe` True

testViewBoard :: Spec
testViewBoard = describe "test view board" $
    it "should return the 2d array that the board is comprised of" $
        view newBoard `shouldBe` [[0, 0, 0, 0],[0, 0, 0, 0],[0, 0, 0, 0],[0, 0, 0, 0]]

testNewRandomBoard :: Spec
testNewRandomBoard = describe "test new random board" $ do
    it "all new random boards should have a score equal to 2" $ do
        let stdGen = mkStdGen 42
            randomBoards = evalState (replicateM 500 newRandomBoard) stdGen
        all ((== 2) . score) randomBoards `shouldBe` True
        --score newRandomBoard `shouldBe` 2


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
