module Primitives (spec) where

import qualified Mparse.EParser as EP
import qualified Mparse.GeneralParser as GP
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser primitives. Foundation for all parsers" $ do
    it "result returns a value without consuming input" $ do
      EP.eparse (GP.result (42 :: Int)) "hello" `shouldBe` GP.ParsedData ("hello", (0, 0, 0), 42)

    it "zero always produces empty result" $ do
      EP.eparse (GP.zero :: GP.GeneralParser EP.ParseLocation Char) "anything" `shouldBe` GP.NoData

    it "item consumes one character" $ do
      EP.eparse GP.item "abc" `shouldBe` GP.ParsedData ("bc", (0, 1, 1), 'a')
      EP.eparse GP.item "" `shouldBe` GP.ParserError [("", (0, 0, 0), "Unexpected end of input")]
