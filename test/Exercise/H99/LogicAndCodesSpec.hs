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
      let inputFunc a b = and' a (or' a b)
      let expected = [[True, True, True], [True, False, True], [False, True, False], [False, False, False]]
      table inputFunc `shouldBe` expected

  describe "tablen" $ do
    it "returns a truth table of a given logical expression in n variables" $ do
      let expected = [[True, True, True, True], [True, True, False, True], [True, False, True, True], [True, False, False, True], [False, True, True, True], [False, True, False, True], [False, False, True, True], [False, False, False, True]]
      let actual = tablen 3 (\[a, b, c] -> a `and'` (b `or'` c) `equ` a `and'` b `or'` a `and'` c)
      actual `shouldBe` expected
      
