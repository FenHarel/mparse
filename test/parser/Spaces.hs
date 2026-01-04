module Spaces (spec) where

import qualified Mparse.EParser as EP
import Test.Hspec

spec :: Spec
spec = do
  describe "Spaces and newlines" $ do
    it "spaces consumes whitespace" $ do
      EP.parseResults EP.spaces "   abc" `shouldBe` [EP.Success ("   ", ("abc", (0, 3, 3)))]
    it "newLine consumes newLine" $ do
      EP.parseResults EP.newLine "\nabc" `shouldBe` [EP.Success ('\n', ("abc", (1, 0, 1)))]
    it "token consumes trailing whitespace" $ do
      EP.parseResults (EP.token (EP.char 'a')) "a   bc" `shouldBe` [EP.Success ('a', ("bc", (0, 4, 4)))]
