module LibSpec where

import Lib (validateTestFramemework, reduce)
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do 
    testFramework
    testReduce

testFramework :: Spec
testFramework = describe "testFramework" $
       it "should validate that the test framework is working" $
       validateTestFramemework 2 `shouldBe` 4


testReduce :: Spec
testReduce = describe "test reduce" $ do
    it "should return the empty list" $
        reduce [] `shouldBe` []
    it "should return the same element" $
        reduce [2] `shouldBe` [2]
    it "should not collapse the elements if they are different" $
        reduce [2, 4] `shouldBe` [2, 4]
    it "should collapse elements when they are equal" $
        reduce [2, 2] `shouldBe` [4]
    it "should collapse elements only once for each pass" $ do
        reduce [2, 2, 2, 2] `shouldBe` [4, 4]
        reduce [4, 2, 2] `shouldBe` [4, 4]

    it "empty spaces (equal to zero) should be removed" $ do
        reduce [0, 0, 2, 2] `shouldBe` [4]
        reduce [0, 2, 2, 0, 0] `shouldBe` [4]



