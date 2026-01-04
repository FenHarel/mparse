module Tests (spec) where

import qualified DotEnv.Tests
import qualified Format.Tests
import qualified Parser.Tests
import Test.Hspec

spec :: Spec
spec = do
  Parser.Tests.spec
  DotEnv.Tests.spec
  Format.Tests.spec
