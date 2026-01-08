module Optionals (spec) where

import qualified Mparse.EParser as EP
import qualified Mparse.MParser as MP
import Test.Hspec

spec :: Spec
spec = do
  describe "Handlers for failed parses" $ do
    it "optional makes a parser optional" $ do
      EP.eparse (MP.optional (MP.char 'a')) "abc" `shouldBe` MP.ParsedData ("bc", (0, 1, 1), Just 'a')
      EP.eparse (MP.optional (MP.char 'a')) "xyz" `shouldBe` MP.ParsedData ("xyz", (0, 0, 0), Nothing)
    it "defaults returns a default value on failure" $ do
      EP.eparse (MP.defaults 'a' (MP.char 'a')) "abc" `shouldBe` MP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse (MP.defaults 'a' (MP.char 'a')) "xyz" `shouldBe` MP.ParsedData ("xyz", (0, 0, 0), 'a')
