module Parser.Tests (spec) where

import qualified Parser.ParserSpec
import Test.Hspec

spec :: Spec
spec = do
  Parser.ParserSpec.spec
