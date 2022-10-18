module Exercise.Algorithm.SortSpec (spec) where

import Exercise.Algorithm.Sort

import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

spec :: Spec
spec = do
  describe "mergeSort" $ do
    it "Sort a list in ascending order" $ do
      mergeSort [9, 5, 6, 3, 2, 1, 7, 8, 4, 0] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

  describe "quickSort" $ do
    it "Sort a list in ascending order" $ do
      quickSort [9, 5, 6, 3, 2, 1, 7, 8, 4, 0] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

  describe "quickSortBy" $ do
    it "Sort a list by the return value of a given predicate function" $ do
      quickSortBy [17, 38, 83, 20, 35, 96, 41, 9] (`mod` 10) `shouldBe` [20, 41, 83, 35, 96, 17, 38, 9]
