module Characters (spec) where

import Data.Char (isDigit)
import qualified Mparse.EParser as EP
import qualified Mparse.GeneralParser as GP
import Test.Hspec

spec :: Spec
spec = do
  describe "Character parser primitives. Building blocks for all parsers" $ do
    it "sat matches characters that satisfy a predicate" $ do
      EP.eparse (GP.sat isDigit) "123" `shouldBe` GP.ParsedData ("23", (0, 1, 1), '1')
      EP.eparse (GP.sat isDigit) "abc" `shouldBe` GP.ParserError [("bc", (0, 1, 1), "Received character: 'a'")]

    it "sat matches characters that satisfy a predicate" $ do
      EP.eparse (GP.digit) "123" `shouldBe` GP.ParsedData ("23", (0, 1, 1), '1')
      EP.eparse (GP.digit) "abc" `shouldBe` GP.ParserError [("bc", (0, 1, 1), "Received character: 'a'")]

    it "char matches a specific character" $ do
      EP.eparse (GP.char 'a') "abc" `shouldBe` GP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse (GP.char 'a') "xyz" `shouldBe` GP.ParserError [("yz", (0, 1, 1), "Received character: 'x'")]
