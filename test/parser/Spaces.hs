module Spaces (spec) where

import qualified Mparse.EParser as EP
import qualified Mparse.MParser as MP
import Test.Hspec

spec :: Spec
spec = do
  describe "Spaces and newlines" $ do
    it "spaces consumes whitespace" $ do
      EP.eparse MP.spaces "   abc" `shouldBe` MP.ParsedData ("abc", (0, 3, 3), "   ")
    it "newLine consumes newLine" $ do
      EP.eparse MP.newLine "\nabc" `shouldBe` MP.ParsedData ("abc", (1, 0, 1), '\n')
    it "token consumes trailing whitespace" $ do
      EP.eparse (MP.token (MP.char 'a')) "a   bc" `shouldBe` MP.ParsedData ("bc", (0, 4, 4), 'a')
