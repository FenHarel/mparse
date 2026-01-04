module Primitives (spec) where

import qualified Mparse.EParser as EP
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser primitives. Foundation for all parsers" $ do
    it "result returns a value without consuming input" $ do
      EP.parseResults (EP.result (42 :: Int)) "hello" `shouldBe` [EP.Success (42, ("hello", (0, 0, 0)))]

    it "zero always produces empty result" $ do
      EP.parseResults EP.zero "anything" `shouldBe` ([] :: [EP.ParseResult Int])

    it "item consumes one character" $ do
      EP.parseResults EP.item "abc" `shouldBe` [EP.Success ('a', ("bc", (0, 1, 1)))]
      EP.parseResults EP.item "" `shouldBe` [EP.Failure (EP.RootError ("Unexpected end of input.", (0, 0, 0)))]
