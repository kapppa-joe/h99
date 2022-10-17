module Exercise.H99.ListSpec (spec) where

import Control.Exception (evaluate)
import Exercise.H99.List
import Test.Hspec (
  Spec,
  describe,
  errorCall,
  it,
  shouldBe,
  shouldThrow,
 )

spec :: Spec
spec = do
  describe "myLast" $ do
    it "Find the last element of a list." $ do
      myLast ([1, 2, 3, 4] :: [Int]) `shouldBe` (4 :: Int)
      myLast ['x', 'y', 'z'] `shouldBe` 'z'
      evaluate (myLast "") `shouldThrow` errorCall "Exception: empty list"

  describe "myButLast" $ do
    it "Find the last but one element of a list." $ do
      myButLast ([1, 2, 3, 4] :: [Int]) `shouldBe` (3 :: Int)
      myButLast ['a' .. 'z'] `shouldBe` 'y'
      evaluate (myButLast ['a']) `shouldThrow` errorCall "Exception: list has less than 2 elements"

  describe "elementAt" $ do
    it "Find the K'th element of a list." $ do
      elementAt [7, 8, 9] 2 `shouldBe` 8
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
      myReverse [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]

  describe "isPalindrome" $ do
    it "Find out whether a list is a palindrome" $ do
      isPalindrome [1, 2, 3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True
      isPalindrome "abccba" `shouldBe` True

  describe "flatten" $ do
    it "Flatten a nested list structure" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1, 2, 3, 4, 5]
      flatten (List [Elem 1]) `shouldBe` [1]
    it "can flatten an empty list" $ do
      -- set [] to an empty list of Int to avoid "Ambiguous type variable ‘a0’" error.
      let emptyList = [] :: [NestedList Int]
      flatten (List emptyList) `shouldBe` []

  describe "compress" $ do
    it "Eliminate consecutive duplicate of list elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
      compress "a" `shouldBe` "a"
      compress "" `shouldBe` ""

  describe "pack" $ do
    it "Pack consecutive duplicates of list elements into sublists." $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]

  describe "encode" $ do
    it "Run-length encoding of a list" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]

  describe "encodeModified" $ do
    it "Run-length encoding of a list (modified version)" $ do
      encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

  describe "decodeModified" $ do
    it
      "Construct the uncompressed version of the run-length encoded list"
      $ do
        decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"

  describe "dupli" $ do
    it "Duplicate the elements of a list." $ do
      dupli [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]

  describe "repli" $ do
    it "Replicate the elements of a list a given number of times." $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"

  describe "dropEvery" $ do
    it "Drop every N'th element from a list." $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  describe "split" $ do
    it "Split a list into two parts; the length of the first part is given." $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")
      split "abcdefghik" 12 `shouldBe` ("abcdefghik", "")

  describe "slice" $ do
    it "Extract a slice from a list given two index i and k. Start counting the elements with 1. Both side inclusive" $ do
      slice "abcdefghik" 3 7 `shouldBe` "cdefg"
      slice "abcdefghik" 1 3 `shouldBe` "abc"
      slice "abcdefghik" 7 3 `shouldBe` ""

  describe "rotate" $ do
    it "Rotate a list N places to the left." $ do
      rotate "abcdefgh" 3 `shouldBe` "defghabc"
    it "Rotate to the right with negative index given." $ do
      rotate "abcdefgh" (-2) `shouldBe` "ghabcdef"
  
  describe "removeAt" $ do
    it "Remove the K'th element from a list." $ do
      removeAt 2 "abcd" `shouldBe` (Just 'b', "acd")
      removeAt 10 "abcd" `shouldBe` (Nothing, "abcd")

  describe "insertAt" $ do
    it "Insert an element at a given position into a list" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

  describe "range" $ do
    it "Create a list containing all integers within a given range" $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]
      range (-1) 3 `shouldBe` [-1, 0, 1, 2, 3]
      range 3 1 `shouldBe` []

  -- describe "rndSelect" $ do
  --   it "Extract a given number of randomly selected elements from a list" $ do
  --     let output = rndSelect "abcdefgh" 3
  --     length output `shouldBe` 3
  --     all (`elem` output) output `shouldBe` True