module Exercise.H99.ArithmeticSpec (spec) where

import Exercise.H99.Arithmetic

import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe
  )

spec :: Spec
spec = do

  describe "isPrime" $ do
    it "Determine whether a given integer number is prime." $ do
      isPrime 7 `shouldBe` True
      isPrime 4 `shouldBe` False

    it "Works correctly for prime number within 100" $ do
      let expected = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
      filter isPrime [1..100] `shouldBe` expected

    it "Can handle a (kind of...?) larger number" $ do
        isPrime 15485863 `shouldBe` True
        isPrime 15485871 `shouldBe` False

  describe "myGCD" $ do
    it "Determine the greatest common divisor of two integers" $ do
      [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9,3,3]

  describe "coprime" $ do
    it "Determine whether two positive integer numbers are coprime" $ do
      coprime 35 64 `shouldBe` True
      coprime 15 99 `shouldBe` False
  