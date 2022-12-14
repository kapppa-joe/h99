module Exercise.H99.ArithmeticSpec (spec) where

import Exercise.H99.Arithmetic

import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

spec :: Spec
spec = do
  describe "isPrime" $ do
    it "Determine whether a given integer number is prime." $ do
      isPrime 7 `shouldBe` True
      isPrime 4 `shouldBe` False

    it "Works correctly for prime number within 100" $ do
      let expected = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
      filter isPrime [1 .. 100] `shouldBe` expected

    it "Can handle a (kind of...?) larger number" $ do
      isPrime 15485863 `shouldBe` True
      isPrime 15485871 `shouldBe` False

  describe "myGCD" $ do
    it "Determine the greatest common divisor of two integers" $ do
      [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9, 3, 3]

  describe "coprime" $ do
    it "Determine whether two positive integer numbers are coprime" $ do
      coprime 35 64 `shouldBe` True
      coprime 15 99 `shouldBe` False

  describe "totient" $ do
    it "Calculate the Euler's totient (phi) of a given positive integer" $ do
      totient 10 `shouldBe` 4
  -- totient 123456 `shouldBe` 41088

  describe "primeFactors" $ do
    it "Calculate the prime factors of a given positive integer. Return a flat list of factors in ascending order" $ do
      primeFactors 315 `shouldBe` [3, 3, 5, 7]
      primeFactors 13 `shouldBe` [13]
      primeFactors 64 `shouldBe` [2, 2, 2, 2, 2, 2]

  describe "primeFactorsMult" $ do
    it "Return the prime factors with their multiplicity." $ do
      primeFactorsMult 315 `shouldBe` [(3, 2), (5, 1), (7, 1)]
      primeFactorsMult 13 `shouldBe` [(13, 1)]
      primeFactorsMult 64 `shouldBe` [(2, 6)]

  describe "phi" $ do
    it "Calculate the Euler's totient (phi) of a given positive integer, with an improved algorithm utilising prime factors" $ do
      phi 10 `shouldBe` 4
      phi 123456 `shouldBe` 41088

  describe "primeR" $ do
    it "Construct a list of all prime numbers in a given range." $ do
      primesR 1 10 `shouldBe` [2, 3, 5, 7]
      primesR 2 10 `shouldBe` [2, 3, 5, 7]
      primesR 10 20 `shouldBe` [11, 13, 17, 19]
      primesR (-100) 100 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

  describe "goldbach" $ do
    it "return two prime numbers that sum up to a given even integer." $ do
      goldbach 28 `shouldBe` (5, 23)
      goldbach 10 `shouldBe` (3, 7)

  describe "goldbachList" $ do
    it "Given a range of integers by its lower and upper limit, return a list of all even numbers and their Goldbach composition" $ do
      goldbachList 9 20 `shouldBe` [(3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)]
      goldbachList 2 20 `shouldBe` [(2, 2), (3, 3), (3, 5), (3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)]
      goldbachList 2 2 `shouldBe` []

  describe "goldbachList'" $ do
    it "Return a list of Goldbach composition in given range where the smaller prime is larger than given threshold k" $ do
      goldbachList' 1 2000 50 `shouldBe` [(73, 919), (61, 1321), (67, 1789), (61, 1867)]