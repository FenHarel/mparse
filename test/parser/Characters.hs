module Characters (spec) where

import Data.Char (isDigit)
import qualified Mparse.EParser as EP
import Test.Hspec

spec :: Spec
spec = do
  describe "Character parser primitives. Building blocks for all parsers" $ do
    it "sat matches characters that satisfy a predicate" $ do
      EP.parseResults (EP.sat isDigit) "123" `shouldBe` [EP.Success ('1', ("23", (0, 1, 1)))]
      EP.parseResults (EP.sat isDigit) "abc" `shouldBe` [EP.Failure (EP.RootError ("Received character: 'a'", (0, 1, 1)))]

    it "sat matches characters that satisfy a predicate" $ do
      EP.parseResults (EP.digit) "123" `shouldBe` [EP.Success ('1', ("23", (0, 1, 1)))]
      EP.parseResults (EP.digit) "abc" `shouldBe` [EP.Failure (EP.RootError ("Received character: 'a'", (0, 1, 1)))]

    it "char matches a specific character" $ do
      EP.parseResults (EP.char 'a') "abc" `shouldBe` [EP.Success ('a', ("bc", (0, 1, 1)))]
      EP.parseResults (EP.char 'a') "xyz" `shouldBe` [EP.Failure (EP.RootError ("Received character: 'x'", (0, 1, 1)))]
