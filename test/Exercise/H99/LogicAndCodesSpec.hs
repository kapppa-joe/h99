module Exercise.H99.LogicAndCodesSpec where

import Exercise.H99.LogicAndCodes

import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

spec :: Spec
spec = do
  describe "table" $ do
    it "returns a truth table of a given logical expression in two variables" $ do
      let inputFunc = (\a b -> and' a (or' a b))
           where
            and' = (&&)
            or' = (||)
      let expected = [[True, True, True], [True, False, True], [False, True, False], [False, False, False]]
      table inputFunc `shouldBe` expected
