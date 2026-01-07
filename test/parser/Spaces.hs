module Spaces (spec) where

import qualified Mparse.EParser as EP
import qualified Mparse.GeneralParser as GP
import Test.Hspec

spec :: Spec
spec = do
  describe "Spaces and newlines" $ do
    it "spaces consumes whitespace" $ do
      EP.eparse GP.spaces "   abc" `shouldBe` GP.ParsedData ("abc", (0, 3, 3), "   ")
    it "newLine consumes newLine" $ do
      EP.eparse GP.newLine "\nabc" `shouldBe` GP.ParsedData ("abc", (1, 0, 1), '\n')
    it "token consumes trailing whitespace" $ do
      EP.eparse (GP.token (GP.char 'a')) "a   bc" `shouldBe` GP.ParsedData ("bc", (0, 4, 4), 'a')
