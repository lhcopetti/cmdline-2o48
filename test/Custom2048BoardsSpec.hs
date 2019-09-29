module Custom2048BoardsSpec where

import Board2048
import Custom2048Boards
import Data.Maybe (fromJust)
import Game2048 (winningTileValue)
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = testBoardFromString >> testAlmostBoards

testBoardFromString :: Spec
testBoardFromString = describe "test board from string" $ do
    it "should return nothing for empty strings" $ do
        boardFromString "" `shouldBe` Nothing
    it "should return Just board when the board is well formed" $ do
        let arr = [ [2, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
        view <$> boardFromString "2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0" `shouldBe` (Just arr)
    it "should fail for consecutive commas in the input" $
        view <$> boardFromString "0,,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0" `shouldBe` Nothing
    it "should fail when there are invalid characters in the input" $
        view <$> boardFromString "0,0,0,0,0,0,0,0theseareletters,0,0,0,0,0,0,0,0" `shouldBe` Nothing
    it "empty spaces are not relevant in the input" $ do
        let arr = [ [2, 4, 8, 16], [32, 64, 128, 256], [512, 1024, 2048, 1024], [512, 256, 128, 64] ]
        view <$> boardFromString "2,4,8,16, 32,64,128,256, 512,1024,2048,1024, 512,256,128,64" `shouldBe` Just arr
    it "should fill missing row tiles with zero" $ do
        let arr = [ [32, 32, 32, 32], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
        view <$> boardFromString "32,32,32,32" `shouldBe` Just arr
    it "should fill missing columns tiles with zero" $ do
        let arr = [ [8, 8, 8, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
        view <$> boardFromString "8, 8, 8" `shouldBe` Just arr
    it "supplying a single value is also allowed" $ do
        let arr = [ [1024, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
        view <$> boardFromString "1024" `shouldBe` Just arr


testAlmostBoards :: Spec
testAlmostBoards = describe "test almost boards" $ do
    it "should return a board that is almost winning" $ do
        score newAlmostWinningBoard `shouldSatisfy` (>=winningTileValue)
    it "should return a board that is almost losing" $ do
        freeTiles newAlmostLostBoard `shouldBe` 0
