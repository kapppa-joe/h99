module Exercise.Ex01Spec (spec)  where

import Test.Hspec
import Exercise.Ex01
import Exercise.Ex01 (NestedList)
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

  describe "elementAt" $ do
    it "Find the K'th element of a list." $ do
      elementAt [7,8,9] 2 `shouldBe` 8
      elementAt "haskell" 5 `shouldBe` 'e'
      evaluate (elementAt "ha" 5) `shouldThrow` errorCall "Exception: index too large"
      evaluate (elementAt "ha" 0) `shouldThrow` errorCall "Exception: non-positive index"

  describe "myLength" $ do
    it "Return the number of elements of a list" $ do
      myLength [123, 456, 789] `shouldBe` 3
      myLength "Hello, world!" `shouldBe` 13

  describe "myReverse" $ do
    it "Reverse a list" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

  describe "isPalindrome" $ do
    it "Find out whether a list is a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True
      isPalindrome "abccba" `shouldBe` True

  describe "flatten" $ do
    it "Flatten a nested list structure" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
      flatten (List [Elem 1]) `shouldBe` [1]
    it "can flatten an empty list" $ do
      -- set [] to an empty list of Int to avoid "Ambiguous type variable ‘a0’" error. 
      let emptyList = [] :: [NestedList Int]
      flatten (List emptyList) `shouldBe` []