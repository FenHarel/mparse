module Primitives (spec) where

import qualified Mparse.EParser as EP
import qualified Mparse.MParser as MP
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser primitives. Foundation for all parsers" $ do
    it "result returns a value without consuming input" $ do
      EP.eparse (MP.result (42 :: Int)) "hello" `shouldBe` MP.ParsedData ("hello", (0, 0, 0), 42)

    it "zero always produces empty result" $ do
      EP.eparse (MP.zero :: MP.MParser EP.ParseLocation Char) "anything" `shouldBe` MP.NoData

    it "item consumes one character" $ do
      EP.eparse MP.item "abc" `shouldBe` MP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse MP.item "" `shouldBe` MP.ParserError [("", (0, 0, 0), "Unexpected end of input")]
