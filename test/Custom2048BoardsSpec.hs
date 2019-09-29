module Custom2048BoardsSpec where

import Board2048
import Custom2048Boards
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    testBoardFromString

testBoardFromString :: Spec
testBoardFromString = describe "test board from string" $ do
    it "should return nothing for empty strings" $ do
        boardFromString "" `shouldBe` Nothing
    it "should return Just board when the board is well formed" $ do
        let arr = [ [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
        view <$> boardFromString "0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0" `shouldBe` (Just arr)
    it "should fail for consecutive commas in the input" $
        view <$> boardFromString "0,,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0" `shouldBe` Nothing
    it "should fail when there are invalid characters in the input" $
        view <$> boardFromString "0,0,0,0,0,0,0,0theseareletters,0,0,0,0,0,0,0,0" `shouldBe` Nothing
    it "empty spaces are not relevant in the input" $ do
        let arr = [ [2, 4, 8, 16], [32, 64, 128, 256], [512, 1024, 2048, 1024], [512, 256, 128, 64] ]
        view <$> boardFromString "2,4,8,16, 32,64,128,256, 512,1024,2048,1024, 512,256,128,64" `shouldBe` Just arr
