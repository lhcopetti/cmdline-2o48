module BoardPrinterSpec where

import Board2048
import BoardPrinter
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    testBottomValue

testBottomValue :: Spec
testBottomValue = describe "test value separation" $ do
    it "should always return the bottom value (least significant digits)" $ do
        bottomValue 2048 `shouldBe` 48
        bottomValue 5 `shouldBe` 5
        bottomValue 123 `shouldBe` 23
    it "should always return the top value (most significant digits)" $ do
        topValue 2048 `shouldBe` 20
        topValue 5 `shouldBe` 0
        topValue 123 `shouldBe` 1
    it "the digit to string should be blank when dealing with zeros on the left" $ do
        format 0 `shouldBe` "  "
        format 20 `shouldBe` "20"
        format 48 `shouldBe` "48"
        format 51 `shouldBe` "51"
        format 2 `shouldBe` " 2"
