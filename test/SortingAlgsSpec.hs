module Main where

import Test.Hspec
import SortingAlgs

main :: IO ()
main = hspec $ do
  describe "sorted" $ do
    it "confirms that [1,2,3] is sorted" $
      sorted [1,2,3] `shouldBe` True
    it "declines that [1,3,2] is sorted" $
      sorted [1,3,2] `shouldBe` False