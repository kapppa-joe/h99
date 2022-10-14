module Exercise.Ex01Spec (spec)  where

import Test.Hspec
import Exercise.Ex01 (myLast, myButLast)
import Control.Exception (evaluate)

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