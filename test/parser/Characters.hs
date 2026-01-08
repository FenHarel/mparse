module Characters (spec) where

import Data.Char (isDigit)
import qualified Mparse.EParser as EP
import qualified Mparse.MParser as MP
import Test.Hspec

spec :: Spec
spec = do
  describe "Character parser primitives. Building blocks for all parsers" $ do
    it "sat matches characters that satisfy a predicate" $ do
      EP.eparse (MP.sat isDigit) "123" `shouldBe` MP.ParsedData ("23", (0, 1, 1), '1')
      EP.eparse (MP.sat isDigit) "abc" `shouldBe` MP.ParserError [("bc", (0, 1, 1), "Received character: 'a'")]

    it "sat matches characters that satisfy a predicate" $ do
      EP.eparse (MP.digit) "123" `shouldBe` MP.ParsedData ("23", (0, 1, 1), '1')
      EP.eparse (MP.digit) "abc" `shouldBe` MP.ParserError [("bc", (0, 1, 1), "Received character: 'a'")]

    it "char matches a specific character" $ do
      EP.eparse (MP.char 'a') "abc" `shouldBe` MP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse (MP.char 'a') "xyz" `shouldBe` MP.ParserError [("yz", (0, 1, 1), "Received character: 'x'")]
