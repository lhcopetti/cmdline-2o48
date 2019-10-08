module ArraysSpec where

import Arrays
import Control.Exception
import Test.Hspec


spec :: Spec
spec = testReplace


testReplace :: Spec
testReplace = describe "test replace" $ do
    it "should replace the value in the array (one element)" $
        replace 0 7 [0] `shouldBe` [7]
    it "should replace the first element correctly" $
        replace 0 15 [0, 1, 2, 3, 4] `shouldBe` [15, 1, 2, 3, 4]
    it "should replace the last element correctly" $
        replace 4 15 [0, 1, 2, 3, 4] `shouldBe` [0, 1, 2, 3, 15]
    it "should replace the middle element correctly" $
        replace 2 15 [0, 1, 2, 3, 4] `shouldBe` [0, 1, 15, 3, 4]
    it "should throw error if the index is greater than the array" $
        evaluate (replace 1 15 [0]) `shouldThrow` errorCall "The index 1 is invalid for the list of length 1"
    it "should throw error if the index is less than zero" $
        evaluate (replace (-1) 15 [0]) `shouldThrow` errorCall "Negative indexes are invalid"
