module Exercise.Ex01Spec (spec)  where

import Test.Hspec
import Exercise.Ex01 (myLast)

spec :: Spec
spec = do
  describe "myLast" $ do
    it "return the last element in a list" $ do
      myLast ([1,2,3,4] :: [Int]) `shouldBe` (4 :: Int)
      myLast ['x','y','z'] `shouldBe` 'z'