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
    it "it sort a list in ascending order" $ do
      mergeSort [9, 5, 6, 3, 2, 1, 7, 8, 4, 0] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

  describe "quickSort" $ do
    it "it sort a list in ascending order" $ do
      quickSort [9, 5, 6, 3, 2, 1, 7, 8, 4, 0] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]