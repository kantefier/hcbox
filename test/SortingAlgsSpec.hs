module Main where

import Test.Hspec
import SortingAlgs

main :: IO ()
main = hspec $ do
  describe "SortingAlgs funcs" $ do
    describe "sorted" $ do
      it "confirms that [1,2,3] is sorted" $
        sorted [1,2,3] `shouldBe` True
      it "declines that [1,3,2] is sorted" $
        sorted [1,3,2] `shouldBe` False

    describe "bubbleSort" $ do
      it "must correctly sort all testingLsts" $
        (all(sorted) . map(bubbleSort)) testingLsts `shouldBe` True

    describe "selectSort" $ do
      it "must correctly sort all testingLsts" $
        (all(sorted) . map(selectSort)) testingLsts `shouldBe` True
        

testingLsts :: [[Integer]]
testingLsts = [] :
  [1] :
  [1,2] :
  [2,1] :
  [1,2,2] :
  [1,2,3] :
  [1,1,3] :
  [1,3,3] :
  [3,1,2] :
  [3,2,1] :
  [1,1,4,3] :
  [1,4,4,2] :
  [3,2,7,1] :
  [8,4,3,2,54] :
  [67,34,23,1,45,63] : []