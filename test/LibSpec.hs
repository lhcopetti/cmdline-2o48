module LibSpec where

import Lib
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do 
    testCollapse
    testReduce

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
