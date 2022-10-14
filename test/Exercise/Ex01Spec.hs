module Exercise.Ex01Spec (spec)  where

import Test.Hspec
import Exercise.Ex01

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
