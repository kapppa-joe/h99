module Exercise.Ex01Spec (spec)  where

import Test.Hspec
import Exercise.Ex01 (myLast, myButLast, elementAt)
import Control.Exception (evaluate)
import Test.Hspec.Discover (describe)
import Test.Hspec (it)

spec :: Spec
spec = do
  describe "myLast" $ do
    it "Find the last element of a list." $ do
      myLast ([1,2,3,4] :: [Int]) `shouldBe` (4 :: Int)
      myLast ['x','y','z'] `shouldBe` 'z'
      evaluate (myLast "") `shouldThrow` errorCall "Exception: empty list"
  
  describe "myButLast" $ do
    it "Find the last but one element of a list." $ do
      myButLast ([1,2,3,4] :: [Int]) `shouldBe` (3 :: Int)
      myButLast ['a'..'z'] `shouldBe` 'y'
      evaluate (myButLast ['a']) `shouldThrow` errorCall "Exception: list has less than 2 elements"

  describe "elementAt" $ do
    it "Find the K'th element of a list." $ do
      elementAt [7,8,9] 2 `shouldBe` 8
      elementAt "haskell" 5 `shouldBe` 'e'
      evaluate (elementAt "ha" 5) `shouldThrow` errorCall "Exception: index too large"
      evaluate (elementAt "ha" 0) `shouldThrow` errorCall "Exception: non-positive index"