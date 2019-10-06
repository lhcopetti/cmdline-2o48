module Core2048Spec where

import Core2048
import Test.Hspec
import Types2048

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    testReduce
    testCollapse
    testReduces
    testMultipleReduces


testReduces :: Spec
testReduces = describe "test reduces" $ do
        it "should push all tiles to the left" $ do
            let arr = [ [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2] ]
                arr' = [ [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0] ]
            reduceL arr `shouldBe` arr'
        it "should push all the tiles to the right" $ do
            let arr = [ [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0] ]
                arr' = [ [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 2] ]
            reduceR arr `shouldBe` arr'
        it "should push all the tiles upwards" $ do
            let arr = [ [2, 0, 0, 0], [0, 2, 0, 0], [0, 0, 2, 0], [0, 0, 0, 2] ]
                arr' = [ [2, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0] ]
            reduceU arr `shouldBe` arr'
        it "should push all the tiles downwards" $ do
            let arr = [ [2, 0, 0, 0], [0, 2, 0, 0], [0, 0, 2, 0], [0, 0, 0, 2] ]
                arr' = [ [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [2, 2, 2, 2] ]
            reduceD arr `shouldBe` arr'

testMultipleReduces :: Spec
testMultipleReduces = describe "test multiple reduces" $ do
    let arr = [ [8, 0, 0, 0]
              , [0, 0, 0, 0]
              , [0, 0, 0, 0]
              , [0, 0, 0, 0]]
    it "should send the tile in the clockwise direction" $
        (reduceU . reduceL . reduceD . reduceR) arr `shouldBe` arr

    it "should sum the two tiles together" $
        reduceR [ [8, 8, 0, 0]
                , [0, 0, 0, 0]
                , [0, 0, 0, 0]
                , [0, 0, 0, 0]] `shouldBe` [ [0, 0, 0, 16]
                                           , [0, 0, 0, 0]
                                           , [0, 0, 0, 0]
                                           , [0, 0, 0, 0]]

    it "should sum the four tiles together" $
        (reduceR . reduceR) [ [8, 8, 8, 8]
                            , [0, 0, 0, 0]
                            , [0, 0, 0, 0]
                            , [0, 0, 0, 0]] `shouldBe` [ [0, 0, 0, 32]
                                                       , [0, 0, 0, 0]
                                                       , [0, 0, 0, 0]
                                                       , [0, 0, 0, 0]]
    it "should send the tile clockwise direction" $
        (reduceR . reduceD) [ [8, 0, 0, 0]
                            , [0, 0, 0, 8]
                            , [0, 0, 0, 0]
                            , [0, 0, 0, 0]] `shouldBe` [ [0, 0, 0, 0]
                                                       , [0, 0, 0, 0]
                                                       , [0, 0, 0, 0]
                                                       , [0, 0, 0, 16]]
    it "should send the tile clockwise direction" $
        (reduceR . reduceD) [ [8, 0, 0, 0]
                            , [0, 0, 0, 8]
                            , [0, 0, 0, 0]
                            , [0, 0, 0, 0]] `shouldBe` [ [0, 0, 0, 0]
                                                       , [0, 0, 0, 0]
                                                       , [0, 0, 0, 0]
                                                       , [0, 0, 0, 16]]

    it "should be able to reduce all tiles to only one in the bottom-right corner" $
        (reduceD . reduceD . reduceR . reduceR) [ [8, 0, 0, 0]
                                                , [0, 0, 0, 8]
                                                , [4, 0, 0, 4]
                                                , [4, 0, 0, 4]] `shouldBe` [ [0, 0, 0, 0]
                                                                           , [0, 0, 0, 0]
                                                                           , [0, 0, 0, 0]
                                                                           , [0, 0, 0, 32]]

    it "should be able to reduce all tiles to only one in the top-left corner" $
        (reduceU . reduceU . reduceL . reduceL) [ [16, 0, 0, 0]
                                                , [0, 0, 0, 16]
                                                , [8, 0, 0, 8]
                                                , [8, 0, 0, 8]] `shouldBe` [ [64, 0, 0, 0]
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
    it "should send the tile in the clockwise direction" $
        (reduceU . reduceL . reduceD . reduceD . reduceR) arr2 `shouldBe` res2


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

    it "empty leading spaces (equal to zero) should be removed" $
        collapse [0, 0, 2, 2] `shouldBe` [4]
    it "empty trailling spaces (equal to zero) should be removed" $
        collapse [2, 2, 0, 0] `shouldBe` [4]

testReduce :: Spec
testReduce = describe "test reduce" $ do
    it "should not change the array size" $
        reduce [0, 0, 0, 0, 0, 0] `shouldBe` [0, 0, 0, 0, 0, 0]
    it "when collapsing elements, should restore the array size" $ do
        reduce [2, 2, 2, 2] `shouldBe` [4, 4, 0, 0]
        reduce [0, 0, 2, 2] `shouldBe` [4, 0, 0, 0]

