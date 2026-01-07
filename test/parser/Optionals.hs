module Optionals (spec) where

import qualified Mparse.EParser as EP
import qualified Mparse.GeneralParser as GP
import Test.Hspec

spec :: Spec
spec = do
  describe "Handlers for failed parses" $ do
    it "optional makes a parser optional" $ do
      EP.eparse (GP.optional (GP.char 'a')) "abc" `shouldBe` GP.ParsedData ("bc", (0, 1, 1), Just 'a')
      EP.eparse (GP.optional (GP.char 'a')) "xyz" `shouldBe` GP.ParsedData ("xyz", (0, 0, 0), Nothing)
    it "defaults returns a default value on failure" $ do
      EP.eparse (GP.defaults 'a' (GP.char 'a')) "abc" `shouldBe` GP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse (GP.defaults 'a' (GP.char 'a')) "xyz" `shouldBe` GP.ParsedData ("xyz", (0, 0, 0), 'a')
