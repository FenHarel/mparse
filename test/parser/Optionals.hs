module Optionals (spec) where

import qualified Mparse.EParser as EP
import Test.Hspec

spec :: Spec
spec = do
  describe "Handlers for failed parses" $ do
    it "optional makes a parser optional" $ do
      EP.parseResults (EP.optional (EP.char 'a')) "abc" `shouldBe` [EP.Success (Just 'a', ("bc", (0, 1, 1)))]
      EP.parseResults (EP.optional (EP.char 'a')) "xyz" `shouldBe` [EP.Success (Nothing, ("xyz", (0, 0, 0)))]
    it "defaults returns a default value on failure" $ do
      EP.parseResults (EP.defaults 'a' (EP.char 'a')) "abc" `shouldBe` [EP.Success ('a', ("bc", (0, 1, 1)))]
      EP.parseResults (EP.defaults 'a' (EP.char 'a')) "xyz" `shouldBe` [EP.Success ('a', ("xyz", (0, 0, 0)))]
